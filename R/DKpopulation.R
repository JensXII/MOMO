#'  The Danish population, by GROUP and sex
#' @param MOMOgroups groups
#' @import data.table
#' @import RODBC
#' @import ISOweek
#' @import foreach
#' @import doParallel
#' @export DKpop
DKpop <- function(MOMOgroups) {

  MOMOgroups <- MOMOgroups

  con <- RODBC::odbcConnect("ib-ssov-p", readOnlyOptimize = TRUE)
  NursingHome <- data.table::data.table(RODBC::sqlQuery(con, paste0("
      with
      adr as
      (select a.v_pnr as cprnr, a.d_tilflyt_dato, lead(a.d_tilflyt_dato) over(partition by a.v_pnr order by a.d_tilflyt_dato) as d_fraflyt_dato, b.p_plejehjemsnavn
      from
      (SELECT v_pnr, c_kom, c_vej, v_husnum, v_postnr, d_tilflyt_dato FROM IB_DK_Adresser.sstdata.t_adresse with(nolock)
      UNION
      SELECT v_pnr, c_kom, c_vej, v_husnum, v_postnr, d_tilflyt_dato FROM IB_DK_Adresser.sstdata.t_adresse_hist with(nolock)) as a
      left join
      (SELECT distinct p_plejehjemsnavn, c_kom, c_vej, v_husnum, v_postnr
        FROM IB_Plejehjem.dbo.PlejehjemCPR with(nolock)) as b
      on (a.c_kom = b.c_kom) and (a.c_vej = b.c_vej) and (a.v_husnum = b.v_husnum) and (a.v_postnr = b.v_postnr))

      select cprnr, p_plejehjemsnavn as NursingHome, convert(date, d_tilflyt_dato) as tilflyt, convert(date, d_fraflyt_dato) as fraflyt from adr
      where p_plejehjemsnavn is not NULL
      order by cprnr
    "), stringsAsFactors = FALSE, as.is = TRUE))[, `:=`(tilflyt = as.Date(tilflyt), fraflyt = as.Date(fraflyt))]
  RODBC::odbcClose(con)

  con <- RODBC::odbcConnect("srv-mssql-07p", readOnlyOptimize = TRUE)
  DKpop <- data.table::data.table(RODBC::sqlQuery(con, paste0("
      select cprnr, dob, max(exitd) as exitd from
      IB_DKMOMO.DKMOMO.DKpopulation with(nolock)
      group by cprnr, dob
      order by cprnr
    "), stringsAsFactors = FALSE, as.is = TRUE))[, `:=`(dob = as.Date(dob), exitd = as.Date(exitd))]
  RODBC::odbcClose(con)
  rm(con)

  NursingHome <- merge(NursingHome, DKpop, by = "cprnr", all.x = TRUE)[, fraflyt := as.Date(ifelse(is.na(fraflyt), exitd + 1, fraflyt), origin = "1970-01-01")][, exitd := NULL]
  NursingHome <- NursingHome[!is.na(fraflyt)]

  NursingHome <- NursingHome[, .(tilflyt = min(tilflyt), fraflyt = max(fraflyt)), keyby = .(cprnr, dob)]

  rm(DKpop)
  gc()

  # NursingHome.copy <- data.table::copy(NursingHome)
  # NursingHome <- data.table::copy(NursingHome.copy)

  PopDate <- function(cpr) {
    X <- merge(data.table::data.table(expand.grid(cprnr = cpr, date = seq(as.Date("2005-01-01"), Sys.Date(), by = "day"), stringsAsFactors = FALSE)),
               NursingHome, by = "cprnr", all.x = TRUE)[(tilflyt <= date) & (date < fraflyt),
                                                       .(date,
                                                         agegrp = floor(floor(as.numeric(date - dob)/365.25)/5)*5,
                                                         sex = ifelse((as.numeric(substr(cprnr,10,10)) %% 2) == 0, "F",
                                                                      ifelse((as.numeric(substr(cprnr,10,10)) %% 2) == 1, "M", NA)))]
    return(X)
  }

  # library(foreach)
  # library(doParallel)
  cl <- parallel::makeCluster(parallel::detectCores() - 1)
  doParallel::registerDoParallel(cl)
  NursingHome <- foreach::foreach(cpr = unique(NursingHome$cprnr), .combine = rbind, .packages = "data.table", .inorder = FALSE) %dopar% {
    X <- PopDate(cpr)
  }
  parallel::stopCluster(cl)
  rm(cl, PopDate)

  NursingHome <- NursingHome[, .(N = .N), keyby = .(date, sex, agegrp)]

  con <- RODBC::odbcConnect("srv-mssql-07p", readOnlyOptimize = TRUE)
  DKpop <- data.table::data.table(RODBC::sqlQuery(con, paste0("
      select [date], agegrp, sex, sum(N) as N from
      IB_DKMOMO.DKMOMO.DKpopDateRegionAgegrpSex with(nolock)
      group by [date], agegrp, sex
	    order by [date], agegrp, sex
    "), stringsAsFactors = FALSE, as.is = TRUE))
  RODBC::odbcClose(con)
  rm(con)
  DKpop[agegrp < 5, agegrp := 0]
  DKpop <- DKpop[, .(N = sum(N)), keyby = .(date = as.Date(date), agegrp, sex)]

  # Subtract persons in nursinghome
  DKpop <- merge(DKpop, NursingHome, by = c("agegrp", "sex", "date"), all.x = TRUE)
  DKpop[, N := ifelse(!is.na(N.y), N.x - N.y, N.x)][, c("N.x", "N.y") := NULL]

  DKpop <- rbind(DKpop[, NursingHome := 0], NursingHome[, NursingHome := 1])

  DKpop[, age := agegrp + 2][, agegrp := NULL]

  ### Remove age above 120
  DKpop <- DKpop[age < 120]

  cl <- parallel::makeCluster(parallel::detectCores()-1)
  doParallel::registerDoParallel(cl)
  DKpop <- foreach::foreach(g = 1:length(MOMOgroups), .combine = rbind, .packages = "data.table", .inorder = FALSE) %dopar% {
    X <- DKpop[eval(parse(text = MOMOgroups[g])), .(GROUP = names(MOMOgroups)[g], N = sum(N, na.rm = TRUE)), keyby = ISOweek::ISOweek(date)]
  }
  parallel::stopCluster(cl)
  rm(cl)
  DKpop[, `:=`(YoDi = as.numeric(substr(ISOweek,1,4)), WoDi = as.numeric(substr(ISOweek,7,8)))][, ISOweek := NULL]

  return(DKpop[order(GROUP, YoDi, WoDi)])
}
