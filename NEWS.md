## MOMO 2024.06.04

Using restricted cubic splines in the baseline estimation (not recommend)

## MOMO 2024.03.18

Corrections to accommodate Linux

## MOMO 2023.05.08

Exclusion of the COVID-19 pandemic years, 2020-2022, from baseline estimation

## MOMO 2020.9.3

Spring weeks of 2020, COVID-19, permanently excluded from baseline estimation

## MOMO 2020.4.27

Added the input variables Ydrop and Wdrop

## MOMO 2020.4.17

glmnet removed from list of dependencies

## MOMO 2018.8.24

* There are now more options for extracting data from inside your MoMo run. Previously only `MOMO::dataExport$toSave` was available, however, now there is also `MOMO::dataExport$aggr`, `MOMO::dataExport$aggr_fullDelay`, and `MOMO::dataExport$aggr_delay`

## MOMO 2018.8.23

* The new delay correction that Jens developed in December 2017 is now available. This can be chosen by `delayVersion="2017-12"`, or the original delay correction can be chosen by `delayVersion="original"` (default).
    
## MOMO 2017.12.14

* This is a full port of A-MOMOpack (the code used to analyze mortality data for the [EuroMOMO project](www.euromomo.eu)) from Stata to R.
