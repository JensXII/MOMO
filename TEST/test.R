

remove.packages("MOMO")


install.packages("devtools")
devtools::install_github('JensXII/MOMO')

DKdata <- data.table::fread("H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/DK-MOMO/DoD_DoR.csv", sep = ";")[as.Date(DoD) >= as.Date(seq(Sys.Date(), length = 2, by = "-8 years")[2]),]

MOMO::SetOpts(
  DoA = Sys.Date(),			# Date of aggregation/data extraction
  DoPR = as.Date("2013-06-24"),		# Date after which delay is stable
  WStart = 1,
  WEnd = 52,
  country = "Denmark",			# Name of country
  source = "SSI",			# Name of institution
  MDATA = data.frame(DKdata),		# Input R-file
  HDATA = MOMO::BankHolidays(DKdata),	# BankHollidays
  INPUTDIR = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/RMOMO",	# Work directory
  WDIR = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/RMOMO/TEST",	# Work directory
  back = as.numeric(DKdata[!is.na(DoR) & (DoD >= as.Date("2017-05-08")), .(Wdelay = ceiling(quantile(as.numeric(DoR) - as.numeric(DoD), 0.975)/7))])-1,
  WWW = floor(365.25*8/7),
  # WWW = ceiling(5.5*365.25)/7)),
  Ysum = 2019,
  Wsum = 40,
  USEglm2 = TRUE,
  useAUTOMN = TRUE,
  datesISO = FALSE,
  plotGraphs = TRUE,
  delayVersion = "sincos",
  delayVariance = FALSE,
  MOMOgroups = list(`00to14` = "0 <= age & age <= 14",
                    `15to44` = "15 <= age & age <= 44",
                    `45to64` = "45 <= age & age <= 64",
                    `65P` = "age >= 65 & !is.na(age)",
                    `65to74` = "65 <= age & age <= 74",
                    `75to84` = "75 <= age & age <= 84",
                    `85P` = "age >= 85 & !is.na(age)",
                    `Total` = "age >= 0 | is.na(age)"
  ),
  MOMOmodels = c(`00to14` = "LINE",
                 `15to44` = "LINE",
                 `45to64` = "LINE_SIN",
                 `65P` = "LINE_SIN",
                 `65to74` = "LINE_SIN",
                 `75to84` = "LINE_SIN",
                 `85P` = "LINE_SIN",
                 `Total` = "LINE_SIN"
  ),
  Ydrop = 9999,
  Wdrop = 99,
  DropPeriods = "((2020 <= YoDi) & (YoDi <= 2022))",
  # DropPeriods = "((YoDi == 2020) & (1 <= WoDi) & (WoDi <= 26)) | ((YoDi == 2021) & (27 <= WoDi) & (WoDi <= 52))",
  verbose = TRUE
)

#Comments:
#  If country = "Danmark", then the Danish population is included

#DropPeriods excluding the three COVID-19 years:
#  DropPeriods = "((2020 <= YoDi) & (YoDi <= 2022))"

#DropPeriods excluding spring 2020 and autumn 2021 (Danmark):
#  DropPeriods = "((YoDi == 2020) & (1 <= WoDi) & (WoDi <= 26)) | ((YoDi == 2021) & (27 <= WoDi) & (WoDi <= 52))",

#3. Run MOMO
MOMO::RunMoMo()
