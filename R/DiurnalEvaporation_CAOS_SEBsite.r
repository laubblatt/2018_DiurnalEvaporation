#' Reproduce Figures and Tables presented for manuscript Renner et al., 2018 under review for HESS
#' 
#' Analysis of the diurnal hysteresis of meteorological variables measured and modeled 
#' at CAOSII SEB site Petit Nobressart, Luxembourg in 2015

#' @references Renner et al., 2018 "Estimating and understanding model bias
#'     in simulating the diurnal cycle of evapotranspiration:
#'         a case study in Luxembourg" 
#'         available at https://www.hydrol-earth-syst-sci-discuss.net/hess-2018-310/.
#'         
#' @filename DiurnalEvaporation_CAOS_SEBsite.r
#' @author mrenner[at]bgc-jena.mpg.de

#' @import data.table
#' @import REddyProc
#' @import knitr
#' @import lattice
#' @import latticeExtra

#' @keywords internal

#' @version v1.00 copy from M28_diurnalHysteresis_CAOS_SEBsite.r
#' @version v1.01 2018-08-08 new fun camuffo_phaselag_time() which replaces the separate regression and phaselag_time() calc in script phaselag.R
#' @version v1.02 2018-09-18 remove unnecessary figures from script
#' @version v1.02 2018-09-18 naming of xalb in hysteresis figures from Global Radiation to Incoming Solar

#' @version v1.03 2018-09-19 change from MAGNUS to Magnus_Alduchov1996Improved with updated and referenced coeffcients, no visual difference spotted in new function meteo_vapor.R
#' @version v1.04 2018-09-21 new script in package data.table.regression.utils.R  mlm.output.statlong()  mlm.output.statlong.call()
#' @version v1.04 2018-09-21 new script in package utils.aggregate() providing e.g. meann() anomean()
#' @version v1.04 2018-09-21 new script in package utils.colors() providing makeTransparent()
#' @version v1.05 2018-09-28 check with data files from GFZ
#' @version v1.05 2018-09-28 remove the other two OSEB variants from script
#' @version v1.05 2018-10-23 write data for fig12 to csv
#' @version v1.10 2018-11-15 clean script and linked to package phaselag for all functions 
#' @version v1.10 2018-11-15 prepare for github.com/laubblatt/2018_DiurnalEvaporation


#' @TODO use datasets from doi GFZ


# R
## adapt path to your system 
mainpath = "~/bgc/2018_DiurnalEvaporation/"
setwd(mainpath)
pinputdata = paste0(mainpath,"data/") 
pfig = paste0(mainpath,"figures/")
pout = paste0(mainpath,"output/")

## download data into directoy pinputdata 

### source this file to createall figures   
#source(paste0(mainpath,"R/DiurnalEvaporation_CAOS_SEBsite.r"))

library(data.table)
library(lattice)
library(latticeExtra)
library(REddyProc)
library(knitr) # kable()
library(kableExtra)

## package which comes with the paper and contains all functions
# library(devtools)
# install_github("laubblatt/phaselag")
library(phaselag)

# phaselag::anomean(rnorm(19))
# phaselag::aerodynamic_conductance_BM2013(3,3)

### FIGURE label  definitions ---- 
labhourday = expression(bold("Hour of day"))
labWm2 = expression(bold("Energy flux ")*" ("*W*" "*m^-2*")")

labRl = expression(bold("Longwave radiation")*" ("*W*" "*m^-2*")")
labJ = expression(bold("Turbulent heat fluxes")*" ("*W*" "*m^-2*")")
# labRsd = expression(bold("Global radiation "*R[sd])*" ("*W*" "*m^-2*")")
labRsd = expression(bold("Incoming solar radiation "*R[sd])*" ("*W*" "*m^-2*")")
labAE = expression(bold("Available Energy "*R[n]-G)*" ("*W*" "*m^-2*")")
labTemp = expression(bold("Temperature ")*("K"))
labH = expression(bold("Sensible heat flux "*H)*" ("*W*m^-2*")")
labLE = expression(bold("Latent heat flux "*lambda*E)*" ("*W*" "*m^-2*")")
labHeatWm2 = expression(bold("Heat flux ")*" ("*W*" "*m^-2*")")
labEBCgap = expression(bold("Energy balance closure gap "*R[n]-G - H - lambda*E)*" ("*W*" "*m^-2*")")

labTempAnoK = expression(bold("Temperature Anomaly")*" ("*K*")")
labdTsTaK = expression(bold("Skin - Air Temperature difference")*" ("*K*")")
labvp = expression(bold("Vapor pressure")*" (hPa)")

labga = expression(bold("Aerodynamic conductance")*" ("*m*s^-1*")")
labgs = expression(bold("Surface conductance")*" ("*m*s^-1*")")
labgags = expression(bold("Bulk Conductance")*" ("*m*s^-1*")")

##' Color definitions ----------
colRsd = 4
colRld = 2
colRn = 1
colLEPT = 5
colobs = 1
colSTIC = 2
colOSEB = 3
colTSEB = "darkgreen"
colLEPMFAO = 4

pchobs = 0
pchOSEB = 2
pchTSEB = 3
pchSTIC =1
pchLEPT = 1
pchLEPMFAO = 6
colH = 2
colLE = 4
colSoilHF = "brown"
colTair = 1
colTs = 2
coldT = 4
colesat = 2
colVPD = 4
coleair = 1


### read data from csv files ----
dtseb_Mod = fread(paste0(pinputdata,"dtseb_Mod.csv"))
dtseb_Obs = fread(paste0(pinputdata,"dtseb_Obs.csv"))
dtseb = rbind(dtseb_Obs,dtseb_Mod)

dtseb

dtseb[ , Date := as.IDate(Date)]
dtseb[ , Time := as.ITime(Time)]

dtseb[ , range(Date)]
dtseb[ , min(I(Date + Time/86400))]
dtseb[ Date == "2015-06-11" , min(hour(Time))]
dtseb[ Date == "2015-07-23" , max(hour(Time))]
dtseb[source == "ObsEC", .(last(Date), last(Time)), by = variable]

### append Incoming Solar as extra column ----
(Rsdobs = dtseb[source =="ObsEC" & variable == "IncomingShortwave", .(Date,Time, IncomingShortwave = value)][order(Date,Time),])
setkey(Rsdobs, Date,Time)
key(dtseb)
setkey(dtseb, Date,Time,source, variable)
dtseb = Rsdobs[dtseb]

### use model_OSEB_kB23 as standard model OSEB ! ----
dtseb[ , unique(source)]
dtseb[source == "model_OSEB_kB23" ,  ]
dtseb[source == "model_OSEB_kB23" , source := "model_OSEB" ]

### cast data into a wide format with variables as columns ----
### perform diurnal anomalies and rbind
dmdtseb = dtseb[  ,  .(Time, value = anomean(value,na.rm=TRUE), IncomingShortwave), by = list(Date,variable = paste0("dm_",variable), source)]
dmdtseb[variable == "dm_AirTemperature" & Date == "2015-07-02", ]

dtsebdm = rbind(dtseb,dmdtseb)
str(dtsebdm)

(dtsebwide = dcast.data.table(dtsebdm[ Date %in% c(as.Date("2015-06-12"):as.Date("2015-07-22")), ], Date + Time ~ variable + source))

# slope_sat(20)
# s_linear(20+273)
### compute Available Energy
dtsebwide[ , AE_ObsEC := (NetRadiation_ObsEC - SoilHeatFlux_ObsEC)]
dtsebwide[ , .(AirTemperature_ObsEC)]
dtsebwide[ , sair := Magnus_Alduchov1996Improved_slope_sat(AirTemperature_ObsEC-273.15) * 100]
# Priestley-Taylor potential evaporation
dtsebwide[ , LEPT := 1.26 * sair / (sair + 65) * (AE_ObsEC)]
dtsebwide[hour(Time) == 12 , .(LEPT,LatentHeatFlux_ObsEC)]
# potential Radiation
dtsebwide[ , Rsdpot := fCalcPotRadiation(DoY.V.n = yday(Date), Time/3600, 49.779, 5.803, TimeZone = +0)]

dtsebwide[ , LE_AEmH := (NetRadiation_ObsEC - SoilHeatFlux_ObsEC)-SensibleHeatFlux_ObsEC]
dtsebwide[ , SoilMoisture_ObsEC := (SoilMoisture_5cm_ObsEC * 100 + SoilMoisture_15cm_ObsEC * 100 + SoilMoisture_30cm_ObsEC * 100) / (300) ]

## @update 2018-02-05 use a emissivity= 0.98 for inferring Ts and thus obs dT!!!
dtsebwide[ , Ts := (OutgoingLongwave_ObsEC / (0.98 * 5.67e-8) )^0.25 ]
dtsebwide[ , dT_ObsEC := Ts - AirTemperature_ObsEC]


###  Daily Regressions  ####################
### Regression and other analysis on Date basis with column sensitivity
### stuff is rbind and then it can be joined back to dtsebwide
(dtseb_dailyreg_LEPT = dtsebwide[ , mlm.output.statlong.call("LatentHeatFlux_ObsEC ~ LEPT",.SD), by = list(Date) ][ , sensitivity := "LEPTreg"])
dtseb_dailyreg_EFmax = dtsebwide[ , mlm.output.statlong.call("LatentHeatFlux_ObsEC ~ I(SensibleHeatFlux_ObsEC + LatentHeatFlux_ObsEC)",.SD), by = list(Date) ][ , sensitivity := "EFmaxreg"]
dtseb_dailyreg_EFmin = dtsebwide[ , mlm.output.statlong.call("LatentHeatFlux_ObsEC ~ AE_ObsEC",.SD), by = list(Date) ][ , sensitivity := "EFminreg"]

dtseb_dailyreg = rbind(dtseb_dailyreg_EFmax, dtseb_dailyreg_EFmin, dtseb_dailyreg_LEPT)
(dtseb_dailyreg_wide = dcast.data.table(dtseb_dailyreg, Date ~ sensitivity + statistic))
key(dtseb_dailyreg_wide)
key(dtsebwide)
setkey(dtsebwide, Date, Time)
dtsebwide = dtseb_dailyreg_wide[dtsebwide]

### defining a cloudiness ratio to potential radiation
# xyplot(IncomingShortwave_ObsEC + IncomingShortwave_model_WRFc + I(0.78 * Rsdpot) + IncomingShortwave_model_WRFdry + IncomingShortwave_model_WRFwet ~ Time | Date , data = dtsebwide, type = "l")
dtsebwide[!is.na(IncomingShortwave_ObsEC) , Rsd2Rsdpot := (sum(IncomingShortwave_ObsEC) /  sum(0.78 * Rsdpot)), by = Date]
dtsebwide[ , SaturationVaporPressureSurface_obsEC := Magnus_Alduchov1996Improved(SkinTemperature_ObsEC - 273.15)]
dtsebwide[ , SaturationVaporPressureAir_obsEC := Magnus_Alduchov1996Improved(AirTemperature_ObsEC - 273.15)]
dtsebwide[ , VaporPressureAir_obsEC :=  Magnus_Alduchov1996Improved(AirTemperature_ObsEC - 273.15) * RelativeHumidity_ObsEC/100]
dtsebwide[ , VaporPressureDeficitSurfaceAir :=  SaturationVaporPressureSurface_obsEC - VaporPressureAir_obsEC]
dtsebwide[ , VaporPressureDeficitAir :=  SaturationVaporPressureAir_obsEC - VaporPressureAir_obsEC]
dtsebwide[ , list(Date,Time,AirTemperature_ObsEC,RelativeHumidity_ObsEC,SaturationVaporPressureSurface_obsEC, VaporPressureAir_obsEC)]

colnames(dtsebwide)

#' Energy balance CLosure correction ------------------------
#' @version 2017-10-13 LE_BRC should be now standard, it uses the 30min regression EF estimate which should be rather robust
### now correct LE -- LE_BRC should be now standard, it uses the 30min regression EF estimate which should be rather robust
dtsebwide[ , LE_BRC :=   LatentHeatFlux_ObsEC + EBCgap_ObsEC * EFmaxreg_slope1 ]
dtsebwide[ , H_BRC :=   SensibleHeatFlux_ObsEC + EBCgap_ObsEC * (1 - EFmaxreg_slope1) ]
## check OK
dtsebwide[ , AE_ObsEC -  (H_BRC + LE_BRC)]
xyplot(AE_ObsEC  + I(H_BRC + LE_BRC) + I(SensibleHeatFlux_ObsEC + LatentHeatFlux_ObsEC) ~ Time | Date , data = dtsebwide , auto.key = list(space = "bottom", column = 2 ), lwd = c(2,3,1,1,1,1,1,1), type ="l")

### compute EF for models
dtsebwide[ , EF_model_STIC := LatentHeatFlux_model_STIC / (NetRadiation_ObsEC - SoilHeatFlux_ObsEC)]
dtsebwide[ , EF_model_OSEB := LatentHeatFlux_model_OSEB / (NetRadiation_ObsEC - SoilHeatFlux_ObsEC)]
dtsebwide[ , EF_model_TSEB := LatentHeatFlux_model_TSEB / (NetRadiation_ObsEC - SoilHeatFlux_ObsEC)]
dtsebwide[ , EF_model_PT := LEPT / (NetRadiation_ObsEC - SoilHeatFlux_ObsEC)]

## calc bulk conductance
dtsebwide[ , ga_BM13 := aerodynamic_conductance_BM2013(ustar = FrictionVelocity_ObsEC, u = WindSpeed_ObsEC)]
dtsebwide[ , ga_Hinvt := aerodynamic_conductance_Hinvert(H = SensibleHeatFlux_ObsEC, Ts = Ts, Ta = AirTemperature_ObsEC)]
dtsebwide[ , ga_HBRCinvt := aerodynamic_conductance_Hinvert(H = H_BRC, Ts = Ts, Ta = AirTemperature_ObsEC)]
dtsebwide[ , ga_ustaru := aerodynamic_conductance_ustaru(ustar = FrictionVelocity_ObsEC, u = WindSpeed_ObsEC)]
dtsebwide[ , ga_thom := aerodynamic_conductance_withcanopy_Thom1975(ustar = FrictionVelocity_ObsEC, u = WindSpeed_ObsEC)]

dtsebwide[ , ga_OSEB := 1/ResistanceAerodynamic_model_OSEB]
dtsebwide[ , ga_TSEB := 1/ResistanceAerodynamic_model_TSEB]

dtsebwide[ , ga_STIC := ConductanceAerodynamicSTIC_model_STIC]
# dtsebwide[ , ga_PM := ConductanceAerodynamicPM_model_STIC]
dtsebwide[ , ga_PMFAO := WindSpeed_ObsEC / 208]
dtsebwide

lims = c(0,0.05)
xyplot(ga_thom + ga_BM13 ~ ga_ustaru | Date, data = dtsebwide, type = "p",auto.key = list(space = "top", column = 3 ), xlim = lims, ylim = lims)
xyplot(ga_ustaru + ga_thom + ga_Hinvt + ga_HBRCinvt ~ Time | Date, data = dtsebwide, type = "l",auto.key = list(space = "top", column = 3 ), ylim = c(0,0.1))

#' @update use ga_thom
#' @update add FAO PM reference evapotranspiration through fixed rs = 70 and ra = 208/u2 (see Allen 1998, BOX)
dtsebwide[ ,LE_PMFAO := LatentHeatFlux_PenmanMonteith(AE = AE_ObsEC, s = sair/100, esurf = SaturationVaporPressureAir_obsEC, eair = VaporPressureAir_obsEC, ga = WindSpeed_ObsEC / 208, gs = 1/70) ]
dtsebwide[ ,LE_PM := LatentHeatFlux_PenmanMonteith(AE = AE_ObsEC, s = sair/100, esurf = SaturationVaporPressureAir_obsEC, eair = VaporPressureAir_obsEC, ga = ga_thom, gs = 1/70) ]

dtsebwide[ ,LE_PM_gaustaru := LatentHeatFlux_PenmanMonteith(AE = AE_ObsEC, s = sair/100, esurf = SaturationVaporPressureAir_obsEC, eair = VaporPressureAir_obsEC, ga = ga_ustaru, gs = 1/100) ]

xyplot(LE_BRC + LE_PM + LE_PM_gaustaru + LE_PMFAO ~ I(Time) | Date, data = dtsebwide, type = "l", lwd = c(3,1,1),auto.key = list(space = "top", column = 3 ))

dtsebwide[ ,gs_LE_PM := LatentHeatFlux_PenmanMonteith_gsinvert(AE = AE_ObsEC, s = sair/100, esurf = SaturationVaporPressureAir_obsEC, eair = VaporPressureAir_obsEC, ga = ga_thom, LE = LE_BRC) ]
dtsebwide[ ,gs_LE_PM_gaustaru := LatentHeatFlux_PenmanMonteith_gsinvert(AE = AE_ObsEC, s = sair/100, esurf = SaturationVaporPressureAir_obsEC, eair = VaporPressureAir_obsEC, ga = ga_ustaru, LE = LE_BRC) ]

#### H daily plots
xyplot(H_BRC +  SensibleHeatFlux_ObsEC +  SensibleHeatFlux_model_OSEB + SensibleHeatFlux_model_TSEB + SensibleHeatFlux_model_STIC ~ Time | Date, data = dtsebwide,type = "l",auto.key = list(space = "top", column = 3 ))


### get dry - wet - classification
## @version 2017-11-03 use EFmaxreg_slope1
#' @definition wet-dry
dtsebwide[ , wetdry := ifelse(EFmaxreg_slope1 < 0.5, "dry", ifelse(EFmaxreg_slope1 > 0.6, "wet", "normal")) ]
dtsebwide[ , wet := wetdry == "wet"]
dtsebwide[ , dry := wetdry == "dry"]

dtsebwide_daily = dtsebwide[ , lapply(.SD,meann, nmin = 24, na.rm = TRUE), by = Date]
dtsebwide_daily[ , wetdry := ifelse(wet == TRUE, "wet", ifelse(dry == TRUE,"dry", "normal"))]

(sunnydates = dtsebwide[Rsd2Rsdpot > 0.85, unique(Date) ])

(dtsunnywetdry_days =  dtsebwide_daily[Rsd2Rsdpot > 0.85, list(Date,Rsd2Rsdpot,SoilMoisture_ObsEC,SoilMoisture_5cm_ObsEC, EFmaxreg_slope1), by = wetdry][order(Date),])
fwrite(dtsunnywetdry_days, file = paste0(pout,"dtsunnywetdry_days.csv"))

### diurnal course composite averages
## @version 2017-05-17 make sure all data is the same for all models
## @version 2017-05-17 make sure that sunnydates is used !
## @version 2017-08-04 use higher threshold because whole period data is available

vars = c("SensibleHeatFlux_ObsEC", "LatentHeatFlux_ObsEC", "IncomingShortwave_ObsEC")
(dtsebwidecomplete = na.omit(dtsebwide, cols = vars))

dtsebwide_byhouravg_sunnywetdry_mean =   dtsebwidecomplete[Date %in% sunnydates , lapply(.SD, mean, na.rm = TRUE), by = list(wetdry,  hour(Time))]
dtsebwide_byhouravg_sunnywetdry_sd   =   dtsebwidecomplete[Date %in% sunnydates , lapply(.SD, sd, na.rm = TRUE), by = list(wetdry,  hour(Time))]

cona = colnames(dtsebwide_byhouravg_sunnywetdry_sd)[-c(1:2)]
(conanew = paste0("sd_",cona))
setnames(dtsebwide_byhouravg_sunnywetdry_sd, cona, conanew)
setkey(dtsebwide_byhouravg_sunnywetdry_sd,wetdry,hour)
setkey(dtsebwide_byhouravg_sunnywetdry_mean,wetdry,hour)
dtsebwide_byhouravg_sunnywetdry = dtsebwide_byhouravg_sunnywetdry_mean[dtsebwide_byhouravg_sunnywetdry_sd]


###  phase lag regression ----------------------------------------------------------
### MLR regression to estimate the phase lag / hysteresis ala Camuffo and Bernhardi 1982
#' @version 0.31   2018-01-30
dtsebwide[ , dRsd := c(NA,diff(IncomingShortwave_ObsEC))]
dtsebwide[ , dRsdpot := c(NA,diff(0.78*Rsdpot))]
# dtsebwide[ , IRsdpot := cumsum(Rsdpot - mean(Rsdpot)), by = Date]

## melt wide frame and use reference variables as extra columns for regression
colnames(dtsebwide)
(dtsebwidemeltcamuffo =  melt(dtsebwide[ , .(Date,Time, IncomingShortwave_ObsEC, Rsdpot,  dT_ObsEC,
   LE_BRC, H_BRC, LatentHeatFlux_ObsEC, SensibleHeatFlux_ObsEC,  NetRadiation_ObsEC,
    SoilHeatFlux_ObsEC, AirTemperature_ObsEC, Ts, IncomingLongwave_ObsEC,
     SoilTemperature_2cm_ObsEC, SoilTemperature_5cm_ObsEC, SoilTemperature_15cm_ObsEC, SoilTemperature_30cm_ObsEC,
   AE_ObsEC, SkinTemperature_ObsEC, VaporPressureAir_obsEC, SaturationVaporPressureSurface_obsEC,
    VaporPressureDeficitSurfaceAir, VaporPressureDeficitAir, ga_ustaru, ga_Hinvt, gs_LE_PM, gs_LE_PM_gaustaru, ga_thom,
     LatentHeatFlux_model_OSEB, LatentHeatFlux_model_TSEB, LatentHeatFlux_model_STIC,
     LE_PM, LE_PMFAO,LEPT,
     dRsd, dRsdpot) ], id.vars = c("Date", "Time", "IncomingShortwave_ObsEC", "dRsd", "dRsdpot"))
)

## calc the number of measures per day and variable to ensure the regression gets sufficient data
dtsebwidemeltcamuffo[!is.na(value) , ndayvar := .N, by = list(Date,variable)]

### estimate the Phase lag from regression:
#' @version 2018-02-06 with the help of Luigi
#' @version 2018-08-08 full function from new R-package phaselag::camuffo_phaselag_time() taking care of the regression and the phaselag in time units

(dtseb_camufforeg_dRsd_widestat = dtsebwidemeltcamuffo[ ndayvar > 22 ,
   camuffo_phaselag_time(Y = value, X = IncomingShortwave_ObsEC, dX = dRsd, nday = 48, timeunitperday = 60*24),
   by = list(Date, variable) ][ , sensitivity := "Rsd_dRsd"] )

colnames(dtseb_camufforeg_dRsd_widestat)

# dtseb_camufforeg_dRsd_widestat
key(dtseb_camufforeg_dRsd_widestat)
setkey(dtseb_camufforeg_dRsd_widestat, Date, variable)
dtseb_camufforeg  =  dtsebwide_daily[dtseb_camufforeg_dRsd_widestat]
dtseb_camufforeg

xyplot(phaselagtime + slope2_pvalue + R2adj ~ Date, data = droplevels(dtseb_camufforeg_dRsd_widestat[variable %in% c("LE_BRC", "H_BRC","LatentHeatFlux_ObsEC"), ]), group = variable, type = "l",auto.key = list(space = "top", column = 3 ),scales=list( y=list(relation='free')) )

xyplot(phaselagtime ~ Date, data = droplevels(dtseb_camufforeg[variable %in% c("AirTemperature_ObsEC","SkinTemperature_ObsEC", "H_BRC","LatentHeatFlux_ObsEC"), ]), group = variable, type = "l",auto.key = list(space = "top", column = 3 ))

##' add Rn-G as reference for phase lags -----------------
#' @update 2018-11-12 add column of the phase lag, when AE is used as a reference
dtsebwide[ , dAE_ObsEC := c(NA,diff(AE_ObsEC))]
(dtsebwidemeltcamuffo_AE =  melt(dtsebwide[ , .(
  Date,Time, IncomingShortwave_ObsEC, Rsdpot,  dT_ObsEC,
  LE_BRC, H_BRC, LatentHeatFlux_ObsEC, SensibleHeatFlux_ObsEC,  NetRadiation_ObsEC,
  SoilHeatFlux_ObsEC, AirTemperature_ObsEC, Ts, IncomingLongwave_ObsEC,
  SoilTemperature_2cm_ObsEC, SoilTemperature_5cm_ObsEC, SoilTemperature_15cm_ObsEC, SoilTemperature_30cm_ObsEC,
  AE_ObsEC, dAE_ObsEC, SkinTemperature_ObsEC, VaporPressureAir_obsEC, SaturationVaporPressureSurface_obsEC,
  VaporPressureDeficitSurfaceAir, VaporPressureDeficitAir, ga_ustaru, ga_Hinvt, gs_LE_PM, gs_LE_PM_gaustaru, ga_thom,
  LatentHeatFlux_model_OSEB, LatentHeatFlux_model_TSEB, LatentHeatFlux_model_STIC,
  LE_PM, LE_PMFAO,LEPT,
  dRsd, dRsdpot) ], id.vars = c("Date", "Time", "IncomingShortwave_ObsEC", "dRsd", "dRsdpot", "AE_ObsEC", "dAE_ObsEC"))
)
dtsebwidemeltcamuffo_AE[!is.na(value) , ndayvar := .N, by = list(Date,variable)]
### estimate the Phase lag from regression:

(dtseb_camufforeg_dAE_widestat = dtsebwidemeltcamuffo_AE[ ndayvar > 22 ,
      camuffo_phaselag_time(Y = value, X = AE_ObsEC, dX = dAE_ObsEC, nday = 48, timeunitperday = 60*24),
      by = list(Date, variable) ][ , sensitivity := "AE_dAE"] )
dtseb_camufforeg_dAE_widestat

key(dtseb_camufforeg_dAE_widestat)
setkey(dtseb_camufforeg_dAE_widestat, Date, variable)
key(dtsebwide_daily)

dtseb_camufforeg_widestat_dRsd_dAE =  rbind(dtseb_camufforeg_dAE_widestat, dtseb_camufforeg_dRsd_widestat)

dtseb_camufforeg_widestat_dRsd_dAE
(numCols = names(dtseb_camufforeg_widestat_dRsd_dAE)[sapply(dtseb_camufforeg_widestat_dRsd_dAE, is.double)])

dtseb_camufforeg_widestat_dRsd_dAE_wide = dcast(dtseb_camufforeg_widestat_dRsd_dAE, Date + variable  ~ sensitivity, value.var = numCols)

dtseb_camufforeg_AE  =  dtsebwide_daily[dtseb_camufforeg_widestat_dRsd_dAE_wide]
dtseb_camufforeg_AE
colnames(dtseb_camufforeg_AE)


## >> end prepare data #####################


#### TABLES AND NUMBERS #######################

## Energy balance closure --------------------
## numbers in text section 2.2.2

### FILTER for missing data in all components before taking the avg, otherwise there can be different samplings  
dtsebwide[!is.na(EBCgap_ObsEC) , .( EBCgap = mean(EBCgap_ObsEC), GapRatio =  mean(LatentHeatFlux_ObsEC + SensibleHeatFlux_ObsEC,na.rm = FALSE) /mean(AE_ObsEC,na.rm = FALSE) ), by = Date]

dtsebwide[!is.na(EBCgap_ObsEC) , .( EBCgap = mean(EBCgap_ObsEC), GapRatio =  mean(LatentHeatFlux_ObsEC + SensibleHeatFlux_ObsEC,na.rm = FALSE) /mean(AE_ObsEC,na.rm = FALSE) )]
# EBCgap  GapRatio -------
# 1: 37.21362 0.7909497

xyplot(I(LatentHeatFlux_ObsEC + SensibleHeatFlux_ObsEC) ~  AE_ObsEC, data = dtsebwide)
lm(I(LatentHeatFlux_ObsEC + SensibleHeatFlux_ObsEC) ~  AE_ObsEC, data = dtsebwide)
### This is the correct ratio as reported in the text 
# Coefficients:
#   (Intercept)     AE_ObsEC  
# -2.5681       0.8054  

xyplot(EBCgap_ObsEC ~ Time | Date, data = dtsebwide, type = "l")
xyplot(EBCgap_ObsEC ~ Time, data = dtsebwide, type = "p", group = Date)
bwplot(EBCgap_ObsEC ~ as.factor(Time), data = dtsebwide)
boxplot(EBCgap_ObsEC ~ hour(Time) , data = dtsebwide, xlab = labhourday, ylab = labEBCgap)


## TABLE 3 on PERFORMANCE stats ---------------------------
### OBS: LE_BRCday, i.e. the corrected Latentheatflux with constant daytime Bowen Ratio
### prepare a table with r, KGE, mean, IQR
### get info on data coverage

dtsebwide[Date %in% sunnydates , sum(!is.na(LE_BRC)), by = Date]
dtsebwide[Date %in% sunnydates & hour(Time) %in% 7:17 & !is.na(LE_BRC),sum(!is.na(LE_BRC)), by = Date]

#' @define a daylight-time full dataset to compute mean values
#' @update 20180117 use r2 instead of r; use 6-6 hours
# LEvars = c("LE_BRC", "LatentHeatFlux_ObsEC", "LEPT", "LatentHeatFlux_model_OSEB_kB23", "LatentHeatFlux_model_OSEB_Kustas", "LatentHeatFlux_model_OSEB_Lhomme", "LatentHeatFlux_model_TSEB", "LatentHeatFlux_model_STIC")
# LEvars = c("LE_BRC", "LatentHeatFlux_ObsEC", "LEPT", "LE_PMFAO", "LatentHeatFlux_model_OSEB", "LatentHeatFlux_model_OSEB_Kustas", "LatentHeatFlux_model_OSEB_Lhomme", "LatentHeatFlux_model_TSEB", "LatentHeatFlux_model_STIC")

LEvars = c("LE_BRC", "LatentHeatFlux_ObsEC", "LEPT", "LE_PMFAO", "LatentHeatFlux_model_OSEB", "LatentHeatFlux_model_TSEB", "LatentHeatFlux_model_STIC")

datsunnyfullobs = dtsebwide[Date %in% sunnydates & hour(Time) %in% 6:17 & !is.na(LE_BRC), ]
datsunnyfullobs_stats = rbind(
datsunnyfullobs[ ,  lapply(.SD,function(x) round(mean(x)) ), by = wetdry, .SDcols = LEvars ]
[ , stat := "mean"],
datsunnyfullobs[ , lapply(.SD,function(x) round(cor(x, y = LE_BRC)^2,2) ), by = wetdry, .SDcols = LEvars]
[ , stat := "r2"],
datsunnyfullobs[ , lapply(.SD,function(x) round(rmse(x, obs = LE_BRC)) ), by = wetdry, .SDcols = LEvars]
[ , stat := "rmse"])
datsunnyfullobs_stats

## an attempt to estimate the linearity to Rsd using correlation
datsunnyfullobs[ , lapply(.SD,function(x) round(cor(x, y = IncomingShortwave_ObsEC)^2,2) ), by = wetdry, .SDcols = LEvars]
datsunnyfullobs[ , lapply(.SD,function(x) round(cor(x, y = IncomingShortwave_ObsEC)^2,2) ), by = list(Date,wetdry), .SDcols = LEvars][ , lapply(.SD,base::mean), by = wetdry]

datsunnyfullobs[ , lapply(.SD,function(x) round(rmse(x, obs = LE_BRC)) ), by = wetdry, .SDcols = LEvars]

datdaytimefullobs = dtsebwide[hour(Time) %in% 6:17 & !is.na(LE_BRC), ]

datdaytimefullobs[ , lapply(.SD,function(x) round(mean(x)) ), .SDcols = LEvars ]
datdaytimefullobs[ , lapply(.SD,function(x) round(rmse(x, obs = LE_BRC)) ), .SDcols = LEvars ]
datdaytimefullobs[ , lapply(.SD,function(x) round(cor(x, y = LE_BRC)^2,2) ), .SDcols = LEvars ]

datdaytimefullobs_stats = rbind(
datdaytimefullobs[ , lapply(.SD,function(x) round(mean(x)) ), .SDcols = LEvars ]
[ , stat := "mean"],
datdaytimefullobs[ , lapply(.SD,function(x) round(cor(x, y = LE_BRC)^2,2) ), .SDcols = LEvars ]
[ , stat := "r2"],
datdaytimefullobs[ , lapply(.SD,function(x) round(rmse(x, obs = LE_BRC)) ), .SDcols = LEvars ]
[ , stat := "rmse"])


datdaytimefullobs[ , wetdry := "all"]
datdaytimefullobs_stats[ , wetdry := "all"]

(daytime_all_sunny_stats =  rbind(datdaytimefullobs_stats,datsunnyfullobs_stats))
ordstat <- c("mean","rmse","r2")
daytime_all_sunny_stats[ , stats := factor(stat,levels = ordstat)]
daytime_all_sunny_stats[order(stats) , ]
as.data.frame(lapply(daytime_all_sunny_stats[order(stats), .SD,.SDcols = LEvars ], sprintf, fmt = c("%.0f", "%.0f", "%.6f") ) )

(X <- as.data.frame(lapply(daytime_all_sunny_stats[order(stats), .SD,.SDcols = LEvars ], sprintf, fmt = c("%.0f", "%.0f", "%.0f", "%.0f", "%.0f", "%.0f", "%.2f", "%.2f", "%.2f"))))
cona = colnames(X)
cona1 =  sub("LatentHeatFlux_model_", "",cona)
cona2 =  sub("LatentHeatFlux_", "",cona1)
colnames(X) <- cona2

out =  cbind(daytime_all_sunny_stats[order(stats), .(stat, period = wetdry)], X)
cat(kable(out, format = "html"), sep = "\n", file = paste0(pfig,"tab03_PN_LE_daytime_stats.html"))
out

### @Table4 Phase lag per day of wet and dry conditions --------------
### create a table: variable, wet/dry, b dY/dRsd, c, phaselagtime, R2adj
#' @version 20180202, 20180321
#' @update 20181114 Table 4 added another column  

varphasetable = c("NetRadiation_ObsEC", "SoilHeatFlux_ObsEC", "AE_ObsEC",  "H_BRC", "IncomingLongwave_ObsEC",
                  "LE_BRC", "LatentHeatFlux_ObsEC", "LEPT", "LE_PM", "LE_PMFAO" ,"LatentHeatFlux_model_OSEB", "LatentHeatFlux_model_TSEB", "LatentHeatFlux_model_STIC",
                  "AirTemperature_ObsEC", "SkinTemperature_ObsEC",
                  "dT_ObsEC", "VaporPressureAir_obsEC", "VaporPressureDeficitAir")

varphasetablename = c("Net Radiation", "Soil Heat Flux", "Available Energy", "Sensible Heat Flux", "Incoming Longwave",
                      "LE BRC", "LE uncor", "Priestley-Taylor", "Penman-Monteith const. gs" , "FAO Penman-Monteith", "LE OSEB" , "LE TSEB", "LE STIC", "Air Temperature", "Surface Temperature",
                      "Ts - Ta", "Vapor Pressure", "Vapor Pressure Deficit")


(dtseb_camufforeg_kableraw_AE =  dtseb_camufforeg_AE[ Rsd2Rsdpot > 0.85, ][ variable %in% varphasetable,
  list(b = mean(slope1_Rsd_dRsd), bsd = sd(slope1_Rsd_dRsd), c = mean(slope2_Rsd_dRsd), csd = sd(slope2_Rsd_dRsd),                     Phaselag = mean(phaselagtime_Rsd_dRsd), Phaselagsd = sd(phaselagtime_Rsd_dRsd), R2adj = mean(R2adj_Rsd_dRsd),
        bAE = mean(slope1_AE_dAE), bAEsd = sd(slope1_AE_dAE), cAE = mean(slope2_AE_dAE), cAEsd = sd(slope2_AE_dAE),
        PhaselagAE = mean(phaselagtime_AE_dAE), PhaselagAEsd = sd(phaselagtime_AE_dAE), R2AEadj = mean(R2adj_AE_dAE)
), by = list(variable, wetdry)])

dtseb_camufforeg_kable_AE = dtseb_camufforeg_kableraw_AE[,  list(
  Variable = factor(variable,levels = varphasetable) , 'Moisture Conditions' = wetdry,
 'Slope b' = paste0(sprintf('%.4f',b), " (", sprintf('%.4f',bsd),")"),
 'Phase Lag to Rsd (in min.)' = paste0(sprintf('%.0f',Phaselag), " (", sprintf('%.0f',Phaselagsd),")"),
  R2adj = sprintf('%.3f',R2adj),                                                                 'Phase Lag to Rn-G (in min.)' = paste0(sprintf('%.0f',PhaselagAE), " (", sprintf('%.0f',PhaselagAEsd),")")
) ][order(Variable),]


levels(dtseb_camufforeg_kable_AE$Variable) = varphasetablename
str(dtseb_camufforeg_kable_AE)
# dtseb_camufforeg_kable[ , Variable := as.character(Variable)]
dtseb_camufforeg_kable_AE

(dtseb_camufforeg_kable_AE_html = kable(dtseb_camufforeg_kable_AE, format = "html")%>%
    kable_styling(full_width = FALSE) %>%
    column_spec(1, bold = TRUE) %>%
    collapse_rows(columns = 1))
# does not work under linux ?!
# (dtseb_camufforeg_kable_html = kable(dtseb_camufforeg_kable, format = "html")%>%
#   kable_styling(full_width = FALSE) %>%
#   column_spec(1, bold = TRUE) )
cat(dtseb_camufforeg_kable_AE_html, sep = "\n", file = paste0(pfig,"tab04_dtseb_camufforeg_kable_AE_html.html"))

dtseb_camufforeg_kable_AE_simpletex = kable(dtseb_camufforeg_kable_AE, format = "latex")
cat(dtseb_camufforeg_kable_AE_simpletex, sep = "\n", file = paste0(pfig,"dtseb_camufforeg_kable_AE_simple.tex"))

# Table for Reply to Review ----
dtseb_camufforeg_kable_AE_small = dtseb_camufforeg_kableraw_AE[,  
   list(Variable = factor(variable,levels = varphasetable) , wetdry,
   'PhaseLag(min) to Rsd' = paste0(sprintf('%.0f',Phaselag), " (", sprintf('%.0f',Phaselagsd),")"),
   'PhaseLag(min) to AE' = paste0(sprintf('%.0f',PhaselagAE), " (", sprintf('%.0f',PhaselagAEsd),")")
) ][order(Variable),]

levels(dtseb_camufforeg_kable_AE_small$Variable) = varphasetablename
dtseb_camufforeg_kable_AE_small
dtseb_camufforeg_kable_AE_simpletex_small = kable(dtseb_camufforeg_kable_AE_small, format = "latex")
cat(dtseb_camufforeg_kable_AE_simpletex_small, sep = "\n", file = paste0(pout,"dtseb_camufforeg_kable_AE_simple_small.tex"))


###############################################
#### FIGURES ##########
colnames(dtsebwide)

### diurnal course when all models available
gname = paste(pout,"CAOSIIPN_SoilMoisture5cm_wetdry.pdf",sep="")
pdf(gname,5,5)
print(
bwplot(wetdry ~ SoilMoisture_5cm_ObsEC, data = dtsebwide, horiz = T)
)
gg = dev.off()
system(paste("pdfcrop ",gname,gname))


### Fig 3a) SoilMoisture5cm_EFmax_Rsd2Rsdpot_Precip ---------------
gname = paste(pfig,"fig03a_CAOSIIPN_SoilMoisture5cm_EFmax_Rsd2Rsdpot_Precip_daily.pdf",sep="")
pdf(gname,5.5,5)
op = par(mar = c(4,5,3,0.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01)
plot(EFmaxreg_slope1 ~ Date, data = dtsebwide_daily, ylim = c(0,1.2), type = "s", lwd = 3, xlab = expression(bold("Date in 2015")),  ylab = expression(bold("Daily fraction")))
# lines(SoilMoisture_ObsEC ~ Date, data = dtsebwide_daily, col = 4)
lines(SoilMoisture_5cm_ObsEC ~ Date, data = dtsebwide_daily, col = 4)
# lines(SoilMoisture_15cm_ObsEC ~ Date, data = dtsebwide_daily, col = 4)
# lines(SoilMoisture_30cm_ObsEC ~ Date, data = dtsebwide_daily, col = 4)

lines(Rsd2Rsdpot ~Date, data = dtsebwide_daily, type = "s", col = "orange", lwd = 2)
lines(Precipitation_ObsEC ~ Date, data = dtsebwide_daily, type = "s", col = "grey", lwd = 2)
abline(h = 0.6, col = "blue", lty = 3)
abline(h = 0.5, col = "red", lty = 3)
lines(EFmaxreg_slope1 ~ Date, data = dtsebwide_daily, type = "s", lwd = 3)
points(EFmaxreg_slope1 ~ Date, data = dtsebwide_daily[wet==TRUE & Rsd2Rsdpot > 0.85,], col = 4, pch = 16, cex = 1.5)
points(EFmaxreg_slope1 ~ Date, data = dtsebwide_daily[dry==TRUE & Rsd2Rsdpot > 0.85,], col = 2, pch = 15, cex = 1.5)
legend("topleft", c("Incoming Solar to Potential Radiation", expression("Evaporative Fraction, "*f[E]==lambda*E / (H + lambda*E)), expression("Soil Moisture "*(m^3/m^3)), "Daily mean Precipitation (mm/30min)" ), col = c("orange", "black", "blue", "grey"), lwd = c(2,3,2,2), bty = "n", cex = 0.8)
gg = dev.off()
system(paste("pdfcrop ",gname,gname))
# system(paste("pdftoppm -singlefile -rx 300 -ry 300 -png ",gname, gname))

## figure which shows the separation of dry and wet dates
##' @Figure3b  SoilMoisture5cm_EFmax_Feddes_sunny -------------
#' @update 2017-11-03, 20180105, 20180917 xlabel
gname = paste(pfig,"fig03b_CAOSIIPN_SoilMoisture5cm_EFmax_Feddes_sunny.pdf",sep="")
pdf(gname,5.5,5)
op = par(mar = c(4,5,3,0.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01)
plot(EFmaxreg_slope1 ~ SoilMoisture_5cm_ObsEC, data = dtsebwide_daily[order(Date), ],
 xlab = expression(bold("Top Soil Moisture ")*(m^3/m^3)),
  ylab = expression(bold("Daily Evaporative Fraction ")("-")), type = "b", lwd = 0.5, col = "grey" )
polygon( x = c(0,1,1,0), y = c(0.6,0.6,1,1), col = rgb(0,0,1,0.2), border = NA)
polygon( x = c(0,1,1,0), y = c(0.5,0.5,0,0), col = rgb(1,0,0,0.2), border = NA)
abline(h = 0.6, col = "lightblue", lty = 3)
abline(h = 0.5, col = "red", lty = 3)
points(EFmaxreg_slope1 ~ SoilMoisture_5cm_ObsEC, data = dtsebwide_daily[wet==TRUE & Rsd2Rsdpot > 0.85,], col = 4, pch = 16, cex = 1.5)
points(EFmaxreg_slope1 ~ SoilMoisture_5cm_ObsEC, data = dtsebwide_daily[dry==TRUE & Rsd2Rsdpot > 0.85,], col = 2, pch = 15, cex = 1.5)
text(EFmaxreg_slope1 ~ SoilMoisture_5cm_ObsEC, data = dtsebwide_daily[Rsd2Rsdpot > 0.85,  ], label = paste(month.name[month(Date)],mday(Date),sep="-") , adj = c(0,0.25), cex = 0.7)
legend("bottomright", c("sunny wet day","sunny dry day", "cloudy day"), col = c(4,2,1), pch = c(16,15,1), bty = "n")
legend("topright", expression(bold("'wet'")), inset = c(0.1,0.17), bty = "n")
legend("bottomright", expression(bold("'dry'")), inset = c(0.1,0.25), bty = "n")
gg = dev.off()
system(paste("pdfcrop ",gname,gname))
system(paste("pdftoppm -singlefile -rx 300 -ry 300 -png ",gname, gname))


##' @FIGURE 4 DIURNAL COURSE of Radiation  for Observations and Models #### 
#### higher air temperature (especially during night) when wet as compared to dry
#### but similar skin temperature !!!
#### check on how to plot
### Figure 4 (a)
gname = paste(pfig,"fig04a_CAOSIIPN_RadiationFluxes_Diurnal_sunnywetdry_lines.pdf",sep="")
pdf(gname,5,5)
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "dry", ]
lims = c(-50,900)
op = par(mar = c(4,5,3,4), las = 1, mgp = c(1.5,0.15,0), tck = 0.01, pty = "sq")

plot(IncomingShortwave_ObsEC ~ hour, data = dat ,type = "b", ylim = lims, xlab = labhourday, ylab = labWm2, lwd = 3, pch = 0, col = NA, axes = FALSE)
# axis(1,at = seq(0,24,6))
(atl = seq(0,24,6))
att = atl - .5
axis(1,at = att, labels = atl)

axis(2)
box()
abline( v = seq(0,24,6), lty = 3, col = "lightgrey")
abline(h = seq(0,1000,200), lty = 3, col = "lightgrey")
lines(IncomingShortwave_ObsEC ~ hour, data = dat ,type = "l", ylim = lims, col = colRsd,pch = 3, lwd = 2)
lines(IncomingLongwave_ObsEC ~ hour, data = dat ,type = "l", ylim = lims, col = colRld,pch = 1, lwd = 2)
lines(NetRadiation_ObsEC ~ hour, data = dat ,type = "l", ylim = lims, col = colRn,pch = 0, lwd = 2)
lines(SoilHeatFlux_ObsEC ~ hour, data = dat ,type = "l", ylim = lims, col = colSoilHF,pch = 0, lwd = 2)
# lines(LE_BRC ~ hour, data = dat ,type = "l", ylim = lims, col = colLE,pch = 0, lwd = 2)
# lines(H_BRC ~ hour, data = dat ,type = "l", ylim = lims, col = colH,pch = 0, lwd = 2)

dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "wet", ]
lines(IncomingShortwave_ObsEC ~ hour, data = dat ,type = "l", ylim = lims, col = colRsd,pch = 3, lwd = 3,lty = 2)
lines(IncomingLongwave_ObsEC ~ hour, data = dat ,type = "l", ylim = lims, col = colRld,pch = 1, lwd = 3,lty = 2)
lines(NetRadiation_ObsEC ~ hour, data = dat ,type = "l", ylim = lims, col = colRn,pch = 0, lwd = 3,lty = 2)
lines(SoilHeatFlux_ObsEC ~ hour, data = dat ,type = "l", ylim = lims, col = colSoilHF,pch = 0, lwd = 3, lty = 2)
# lines(LE_BRC ~ hour, data = dat ,type = "l", ylim = lims, col = colLE,pch = 0, lwd = 3, lty = 2)
# lines(H_BRC ~ hour, data = dat ,type = "l", ylim = lims, col = colH,pch = 0, lwd = 3, lty = 2)
legend("topleft", c(expression(plain(R[sd])), expression(plain(R[n])), expression(plain(R[ld])), expression(plain(G)) ), col = c(colRsd,colRn,colRld,colSoilHF), bty = "n", lwd = 3)

legend("topright", c("dry", "wet"), lwd = 3, lty = c(1,2), bty = "n")
gg = dev.off()
system(paste("pdfcrop ",gname,gname))
system(paste("pdftoppm -singlefile -rx 300 -ry 300 -png ",gname, gname))

## Figure 4 (b) TempsVaporPressure_Diurnal ------
gname = paste(pfig,"fig04b_CAOSIIPN_TempsVaporPressure_Diurnal_sunnywetdry_lines.pdf",sep="")
pdf(gname,5,5)
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "dry", ]
lims = c(270,310)
op = par(mar = c(4,5,3,4), las = 1, mgp = c(1.5,0.15,0), tck = 0.01, pty = "sq")
plot(IncomingShortwave_ObsEC ~ hour, data = dat ,type = "b", ylim = lims, xlab = labhourday, ylab = labTemp, lwd = 3, pch = 0, col = NA, axes = FALSE)
# axis(1,at = seq(0,24,6))
(atl = seq(0,24,6))
att = atl - .5
axis(1,at = att, labels = atl)
axis(2)
box()
abline( v = seq(0,24,6), lty = 3, col = "lightgrey")
abline(h = seq(0,1000,5), lty = 3, col = "lightgrey")
lines(AirTemperature_ObsEC ~ hour, data = dat ,type = "l", ylim = lims, col = colTair, lwd = 2)
lines(Ts ~ hour, data = dat ,type = "l", ylim = lims, col = colTs,pch = 3, lwd = 2)
x0 = dat[,hour]; x1 = x0;
y0 = dat[, AirTemperature_ObsEC + sd_AirTemperature_ObsEC ]
y1 = dat[, AirTemperature_ObsEC - sd_AirTemperature_ObsEC ]
# arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colTair)
# polygon(c(x0,rev(x0)),c(y1,rev(y0)),col= rgb(0.1,.1,.1), border = NA)

dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "wet", ]
lines(AirTemperature_ObsEC ~ hour, data = dat ,type = "l", ylim = lims, col = colTair,pch = 3, lwd = 3, lty = 2)
lines(Ts ~ hour, data = dat ,type = "l", ylim = lims, col = colTs,pch = 3, lwd = 3, lty = 2)

x0 = dat[,hour]; x1 = x0;
y0 = dat[, AirTemperature_ObsEC + sd_AirTemperature_ObsEC ]
y1 = dat[, AirTemperature_ObsEC - sd_AirTemperature_ObsEC ]
legend("topleft", c(expression(plain(T[s])), expression(plain(T[a])), expression(plain(e[a]))) , col = c(colTs,colTair,4), bty = "n", lwd = 3)
legend("topright", c("dry", "wet"), lwd = 3, lty = c(1,2), bty = "n")

par(new = TRUE)
limeair = c(10,30)
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "dry", ]
plot(VaporPressureAir_obsEC ~ hour, data = dat ,type = "l", col = 4, lwd = 2,axes = FALSE, xlab = "", ylab = "", ylim = limeair)
axis(4)
mtext(side = 4, text = expression(bold("Vapor Pressure "*e[a])(hPa)), las = 0, line = 1.3)
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "wet", ]
lines(VaporPressureAir_obsEC ~ hour, data = dat ,type = "l", col = 4,pch = 3, lwd = 3, lty = 2)
gg = dev.off()
system(paste("pdfcrop ",gname,gname))



##' @FIGURE 5 LatentHeatFlux_ObsModel_Diurnal ---- 
#' ## THIS IS THE REFERENCE DIURNAL COURSE plot for Observations and Models
#' @version 2017-05-15 add BowenRatioDaily as closure
#' @version add FAO PM
gname = paste(pfig,"fig05_CAOSIIPN_LatentHeatFlux_ObsModel_Diurnal_sunnywetdry.pdf",sep="")
pdf(gname,11,6)
par(mfrow = c(1,2))
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "dry", ]
lims = c(-50,500)
op = par(mar = c(4,5,3,0.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01, pty = "sq")

plot(LatentHeatFlux_ObsEC ~ hour, data = dat ,type = "b", ylim = lims, xlab = labhourday, ylab = labLE, lwd = 3, pch = 0, col = NA, axes = FALSE)
title("dry", line = 0.3)
(atl = seq(0,24,6))
att = atl - .5
axis(1,at = att, labels = atl)
axis(2)
box()

x0 = dat[,hour]; x1 = x0;
yplow = dat[, LatentHeatFlux_ObsEC]
yphigh = dat[ , LE_AEmH]
polygon(c(x0,rev(x0)),c(yplow,rev(yphigh)),col="grey", border = NA)
# lines(LE_BRCdaily ~ hour, data = dat ,type = "b", ylim = lims, col = colobs,pch = 0, lwd = 3)
lines(LE_BRC ~ hour, data = dat ,type = "b", ylim = lims, col = colobs,pch = 0, lwd = 3)
x0 = dat[,hour]; x1 = x0;
# y0 = dat[, LatentHeatFlux_ObsEC + sd_LatentHeatFlux_ObsEC ]
# y1 = dat[, LatentHeatFlux_ObsEC - sd_LatentHeatFlux_ObsEC ]
# y0 = dat[, LE_BRCdaily + sd_LE_BRCdaily ]
# y1 = dat[, LE_BRCdaily - sd_LE_BRCdaily ]
y0 = dat[, LE_BRC + sd_LE_BRC ]
y1 = dat[, LE_BRC - sd_LE_BRC ]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colobs, lwd = 1)

# lines(LE_AEmH     ~ hour, data = dat ,type = "b", ylim = lims, col = colobs,pch = 0, lwd = 1.5)
lines(LEPT ~ hour, data = dat ,type = "b", ylim = lims, col = colLEPT,pch = 1)
y0 = dat[, LEPT + sd_LEPT]
y1 = dat[, LEPT - sd_LEPT]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colLEPT, lwd = .2)

lines(LE_PMFAO ~ hour, data = dat ,type = "b", ylim = lims, col = colLEPMFAO,pch = pchLEPMFAO)
y0 = dat[, LE_PMFAO + sd_LE_PMFAO]
y1 = dat[, LE_PMFAO - sd_LE_PMFAO]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colLEPMFAO, lwd = 0.2)

lines(LatentHeatFlux_model_STIC ~ hour, data = dat ,type = "b", ylim = lims, col = colSTIC,pch = pchSTIC)
y0 = dat[, LatentHeatFlux_model_STIC + sd_LatentHeatFlux_model_STIC]
y1 = dat[, LatentHeatFlux_model_STIC - sd_LatentHeatFlux_model_STIC]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colSTIC, lwd = 0.2)

lines(LatentHeatFlux_model_OSEB ~ hour, data = dat ,type = "b", ylim = lims, col = colOSEB,pch = pchOSEB)
y0 = dat[, LatentHeatFlux_model_OSEB + sd_LatentHeatFlux_model_OSEB]
y1 = dat[, LatentHeatFlux_model_OSEB - sd_LatentHeatFlux_model_OSEB]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colOSEB, lwd = 0.2)

lines(LatentHeatFlux_model_TSEB ~ hour, data = dat ,type = "b", ylim = lims, col = colTSEB,pch = pchTSEB)
y0 = dat[, LatentHeatFlux_model_TSEB + sd_LatentHeatFlux_model_TSEB]
y1 = dat[, LatentHeatFlux_model_TSEB - sd_LatentHeatFlux_model_TSEB]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colTSEB, lwd = 0.2)
legend("topleft", c("Priestley-Taylor", "FAO Penman-Monteith", "Observations (corr.)", "OSEB", "TSEB", "STIC"), col = c(colLEPT,colLEPMFAO,colobs,colOSEB,colTSEB,colSTIC), lty = c(1), pch = c(pchLEPT,pchLEPMFAO,pchobs,pchOSEB,pchTSEB,pchSTIC), pt.lwd = c(1,1,2,1,1,1), bty = "n")

## wet panel
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "wet", ]
op = par(mar = c(4,2,3,3.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01, pty = "sq")
plot(LatentHeatFlux_ObsEC ~ hour, data = dat ,type = "b", ylim = lims, xlab = labhourday, ylab = "", lwd = 3, pch = pchobs, col = NA,axes = FALSE)
title("wet", line = 0.3)
(atl = seq(0,24,6))
att = atl - .5
axis(1,at = att, labels = atl)
# axis(1,at = seq(0,24,6))
axis(2)
box()
x0 = dat[,hour]; x1 = x0;
yplow = dat[, LatentHeatFlux_ObsEC]
yphigh = dat[ , LE_AEmH]
polygon(c(x0,rev(x0)),c(yplow,rev(yphigh)),col="grey", border = NA)
lines(LE_BRC ~ hour, data = dat ,type = "b", ylim = lims, col = colobs,pch = 0, lwd = 3)
# y0 = dat[, LatentHeatFlux_ObsEC + sd_LatentHeatFlux_ObsEC ]
# y1 = dat[, LatentHeatFlux_ObsEC - sd_LatentHeatFlux_ObsEC ]
# y0 = dat[, LE_BRCdaily + sd_LE_BRCdaily ]
# y1 = dat[, LE_BRCdaily - sd_LE_BRCdaily ]
y0 = dat[, LE_BRC + sd_LE_BRC ]
y1 = dat[, LE_BRC - sd_LE_BRC ]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colobs)

lines(LEPT ~ hour, data = dat ,type = "b", ylim = lims, col = colLEPT,pch = 1)
y0 = dat[, LEPT + sd_LEPT]
y1 = dat[, LEPT - sd_LEPT]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colLEPT, lwd = .2)

lines(LE_PMFAO ~ hour, data = dat ,type = "b", ylim = lims, col = colLEPMFAO,pch = pchLEPMFAO)
y0 = dat[, LE_PMFAO + sd_LE_PMFAO]
y1 = dat[, LE_PMFAO - sd_LE_PMFAO]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colLEPMFAO, lwd = 0.2)

lines(LatentHeatFlux_model_STIC ~ hour, data = dat ,type = "b", ylim = lims, col = colSTIC,pch = pchSTIC)
y0 = dat[, LatentHeatFlux_model_STIC + sd_LatentHeatFlux_model_STIC]
y1 = dat[, LatentHeatFlux_model_STIC - sd_LatentHeatFlux_model_STIC]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colSTIC, lwd = 0.2)

lines(LatentHeatFlux_model_OSEB ~ hour, data = dat ,type = "b", ylim = lims, col = colOSEB,pch = pchOSEB)
y0 = dat[, LatentHeatFlux_model_OSEB + sd_LatentHeatFlux_model_OSEB]
y1 = dat[, LatentHeatFlux_model_OSEB - sd_LatentHeatFlux_model_OSEB]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colOSEB, lwd = 0.2)

lines(LatentHeatFlux_model_TSEB ~ hour, data = dat ,type = "b", ylim = lims, col = colTSEB,pch = pchTSEB)
y0 = dat[, LatentHeatFlux_model_TSEB + sd_LatentHeatFlux_model_TSEB]
y1 = dat[, LatentHeatFlux_model_TSEB - sd_LatentHeatFlux_model_TSEB]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colTSEB, lwd = 0.2)

gg = dev.off()
system(paste("pdfcrop ",gname,gname))
system(paste("pdftoppm -singlefile -rx 300 -ry 300 -png ",gname, gname))


##' @FIGURE 6 LEmodels_Hysteresis -----------
gname = paste(pfig,"fig06_CAOSIIPN_LEmodels_Hysteresis_sunnywetdry.pdf",sep="")
pdf(gname,11,6)
par(mfrow = c(1,2))
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "dry", ]
lims = c(-50,500)
op = par(mar = c(4,5,3,0.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01, pty = "sq")

plot(LE_BRC   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", ylim = lims, xlab = labRsd, ylab = labLE, pch = pchobs, lwd = 2)
title("dry", line = 0.3)
polygon(dat$IncomingShortwave_ObsEC, dat$LE_BRC, col = makeTransparent(colobs,70), border = colobs )
# lines(LEPT   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", col = colLEPT, pchLEPT)

x0 = dat[,IncomingShortwave_ObsEC]; x1 = dat[,IncomingShortwave_ObsEC];
y0 = dat[, LE_BRC + sd_LE_BRC  ]
y1 = dat[, LE_BRC - sd_LE_BRC  ]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colobs)

polygon(dat$IncomingShortwave_ObsEC, dat$LE_PMFAO, col = makeTransparent(colLEPMFAO,70), border = colLEPMFAO )
polygon(dat$IncomingShortwave_ObsEC, dat$LEPT, col = makeTransparent(colLEPT,70), border = colLEPT )
polygon(dat$IncomingShortwave_ObsEC, dat$LatentHeatFlux_model_TSEB, col = makeTransparent(colTSEB,70), border = colTSEB )
polygon(dat$IncomingShortwave_ObsEC, dat$LatentHeatFlux_model_OSEB, col = makeTransparent(colOSEB,70), border = colOSEB )
polygon(dat$IncomingShortwave_ObsEC, dat$LatentHeatFlux_model_STIC, col = makeTransparent(colSTIC,70), border = colSTIC )

## ARROWS
ax0 = dat[hour == 7,IncomingShortwave_ObsEC];  ax1 = dat[hour == 8,IncomingShortwave_ObsEC];
ay0 = dat[hour == 7,LE_BRC];
ay1 = dat[hour == 8,LE_BRC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.25, angle = 30, code = 2, col = colobs, lwd = 4)

ay0 = dat[hour == 7,LEPT];
ay1 = dat[hour == 8,LEPT];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colLEPT, lwd = 3)

ay0 = dat[hour == 7,LE_PMFAO];
ay1 = dat[hour == 8,LE_PMFAO];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colLEPMFAO, lwd = 3)

ay0 = dat[hour == 7,LatentHeatFlux_model_TSEB];
ay1 = dat[hour == 8,LatentHeatFlux_model_TSEB];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colTSEB, lwd = 3)

ay0 = dat[hour == 7,LatentHeatFlux_model_OSEB];
ay1 = dat[hour == 8,LatentHeatFlux_model_OSEB];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colOSEB, lwd = 3)

ay0 = dat[hour == 7,LatentHeatFlux_model_STIC];
ay1 = dat[hour == 8,LatentHeatFlux_model_STIC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colSTIC, lwd = 3)


legend("topleft", c("Priestley-Taylor", "FAO Penman-Monteith", "Observations (corr.)", "OSEB", "TSEB", "STIC"),
 col = c(colLEPT,colLEPMFAO,colobs,colOSEB,colTSEB,colSTIC), lty = c(1), lwd = c(1.5,1.5, 3,1.5,1.5,1.5,1.5), bty = "n")

## wet panel
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "wet", ]
op = par(mar = c(4,2,3,3.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01,pty = "sq")
plot(LE_BRC   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", ylim = lims, xlab = labRsd, ylab = "", pch = pchobs, lwd = 2, xlim = c(0,890))
title("wet", line = 0.3)

x0 = dat[,IncomingShortwave_ObsEC]; x1 = dat[,IncomingShortwave_ObsEC];
y0 = dat[, LE_BRC + sd_LE_BRC  ]
y1 = dat[, LE_BRC - sd_LE_BRC  ]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colobs)

polygon(dat$IncomingShortwave_ObsEC, dat$LE_BRC, col = makeTransparent(colobs,70), border = colobs )

polygon(dat$IncomingShortwave_ObsEC, dat$LE_PMFAO, col = makeTransparent(colLEPMFAO,70), border = colLEPMFAO )
polygon(dat$IncomingShortwave_ObsEC, dat$LEPT, col = makeTransparent(colLEPT,70), border = colLEPT )
polygon(dat$IncomingShortwave_ObsEC, dat$LatentHeatFlux_model_TSEB, col = makeTransparent(colTSEB,70), border = colTSEB )
polygon(dat$IncomingShortwave_ObsEC, dat$LatentHeatFlux_model_OSEB, col = makeTransparent(colOSEB,70), border = colOSEB )
polygon(dat$IncomingShortwave_ObsEC, dat$LatentHeatFlux_model_STIC, col = makeTransparent(colSTIC,70), border = colSTIC )

ax0 = dat[hour == 7,IncomingShortwave_ObsEC];  ax1 = dat[hour == 8,IncomingShortwave_ObsEC];
ay0 = dat[hour == 7,LE_BRC];
ay1 = dat[hour == 8,LE_BRC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.25, angle = 30, code = 2, col = colobs, lwd = 4)

ay0 = dat[hour == 7,LEPT];
ay1 = dat[hour == 8,LEPT];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colLEPT, lwd = 3)

ay0 = dat[hour == 7,LE_PMFAO];
ay1 = dat[hour == 8,LE_PMFAO];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colLEPMFAO, lwd = 3)

ay0 = dat[hour == 7,LatentHeatFlux_model_TSEB];
ay1 = dat[hour == 8,LatentHeatFlux_model_TSEB];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colTSEB, lwd = 3)

ay0 = dat[hour == 7,LatentHeatFlux_model_OSEB];
ay1 = dat[hour == 8,LatentHeatFlux_model_OSEB];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colOSEB, lwd = 3)

ay0 = dat[hour == 7,LatentHeatFlux_model_STIC];
ay1 = dat[hour == 8,LatentHeatFlux_model_STIC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colSTIC, lwd = 3)

gg = dev.off()
system(paste("pdfcrop ",gname,gname))
system(paste("pdftoppm -singlefile -rx 300 -ry 300 -png ",gname, gname))
# system(paste("cp " ,gname, pfigdraft))



##' @FIGURE_08 HYSTERESIS plot for SEB Observations ---- 
gname = paste(pfig,"fig08_CAOSIIPN_EnergyBalanceClosureFlux_Hysteresis_sunnywetdry.pdf",sep="")
pdf(gname,11,6)
par(mfrow = c(1,2))
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "dry", ]
lims = c(-50,500)
op = par(mar = c(4,5,3,0.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01, pty = "sq")
plot(AE_ObsEC  ~ IncomingShortwave_ObsEC , data = dat ,type = "l", ylim = lims, xlab = labRsd, ylab = labHeatWm2)
title("dry", line = 0.3)
polygon(dat$IncomingShortwave_ObsEC, dat$AE_ObsEC, col = makeTransparent(colobs,70), border = colobs )
polygon(dat$IncomingShortwave_ObsEC, dat$H_BRC, col = makeTransparent(colH,70), border = colH )
polygon(dat$IncomingShortwave_ObsEC, dat$LE_BRC, col = makeTransparent(colLE,70), border = colLE )
# polygon(dat$IncomingShortwave_ObsEC, dat$SoilHeatFlux_ObsEC, col = makeTransparent(colSoilHF,70), border = colSoilHF )

x0 = dat[,IncomingShortwave_ObsEC]; x1 = dat[,IncomingShortwave_ObsEC];
y0 = dat[, AE_ObsEC + sd_AE_ObsEC]
y1 = dat[, AE_ObsEC - sd_AE_ObsEC]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = "darkgrey")
ax0 = dat[hour == 7,IncomingShortwave_ObsEC];  ax1 = dat[hour == 8,IncomingShortwave_ObsEC];
ay0 = dat[hour == 7,AE_ObsEC];
ay1 = dat[hour == 8,AE_ObsEC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = 1, lwd = 2)

# lines(SensibleHeatFlux_ObsEC   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", ylim = lims, col = 2)
y0 = dat[, H_BRC + sd_H_BRC  ]
y1 = dat[, H_BRC - sd_H_BRC ]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = rgb(1,0,0,0.5))
# text(dat[ ,IncomingShortwave_ObsEC], dat[, SensibleHeatFlux_ObsEC], dat[, hour], pos = 1, col = 2)
ay0 = dat[hour == 7,H_BRC];
ay1 = dat[hour == 8,H_BRC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = "red", lwd = 2)

#' @version 2017-05-17 BRC closed ET
# lines(LE_BRC   ~ IncomingShortwave_ObsEC , data = dat , col = 4)
y0 = dat[, LE_BRC + sd_LE_BRC  ]
y1 = dat[, LE_BRC - sd_LE_BRC  ]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = rgb(0,0,1,0.5))
ay0 = dat[hour == 7,LE_BRC];
ay1 = dat[hour == 8,LE_BRC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = 4, lwd = 2)

legend("topleft", c(expression(R[n] - G), expression(lambda*E), "H"), col = c(1,4,2), lty = c(1,1,1), bty = "n")

## wet panel
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "wet", ]
op = par(mar = c(4,2,3,3.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01,pty = "sq")
plot(AE_ObsEC   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", ylim = lims, xlab = labRsd, ylab = "")
title("wet",line = 0.3)

polygon(dat$IncomingShortwave_ObsEC, dat$AE_ObsEC, col = makeTransparent(colobs,70), border = colobs )
polygon(dat$IncomingShortwave_ObsEC, dat$H_BRC, col = makeTransparent(colH,70), border = colH )
polygon(dat$IncomingShortwave_ObsEC, dat$LE_BRC, col = makeTransparent(colLE,70), border = colLE )
# polygon(dat$IncomingShortwave_ObsEC, dat$SoilHeatFlux_ObsEC, col = makeTransparent(colSoilHF,70), border = colSoilHF )

x0 = dat[,IncomingShortwave_ObsEC]; x1 = dat[,IncomingShortwave_ObsEC];
y0 = dat[, AE_ObsEC + sd_AE_ObsEC]
y1 = dat[, AE_ObsEC - sd_AE_ObsEC]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = "darkgrey")
ax0 = dat[hour == 7,IncomingShortwave_ObsEC];  ax1 = dat[hour == 8,IncomingShortwave_ObsEC];
ay0 = dat[hour == 7,AE_ObsEC];
ay1 = dat[hour == 8,AE_ObsEC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = 1, lwd = 2)

# lines(SensibleHeatFlux_ObsEC   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", ylim = lims, col = 2)
y0 = dat[, H_BRC + sd_H_BRC  ]
y1 = dat[, H_BRC - sd_H_BRC ]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = rgb(1,0,0,0.5))
# text(dat[ ,IncomingShortwave_ObsEC], dat[, SensibleHeatFlux_ObsEC], dat[, hour], pos = 1, col = 2)
ay0 = dat[hour == 7,H_BRC];
ay1 = dat[hour == 8,H_BRC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = "red", lwd = 2)

#' @version 2017-05-17 BRC closed ET
# lines(LE_BRC   ~ IncomingShortwave_ObsEC , data = dat , col = 4)
y0 = dat[, LE_BRC + sd_LE_BRC  ]
y1 = dat[, LE_BRC - sd_LE_BRC  ]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = rgb(0,0,1,0.5))
ay0 = dat[hour == 7,LE_BRC];
ay1 = dat[hour == 8,LE_BRC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = 4, lwd = 2)

gg = dev.off()
system(paste("pdfcrop ",gname,gname))
system(paste("pdftoppm -singlefile -rx 300 -ry 300 -png ",gname, gname))


### Obs Temperature and its gradient
#' @version 20180104
#' @FIGURE09 Temperatures_Hysteresis  -------------------- 
gname = paste(pfig,"fig09_CAOSIIPN_Temperatures_Hysteresis_sunnywetdry.pdf",sep="")
pdf(gname,11,6)
par(mfrow = c(1,2))
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "dry", ]
lims = c(-15,15)
op = par(mar = c(4,5,3,0.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01, pty = "sq")
plot(dm_AirTemperature_ObsEC  ~ IncomingShortwave_ObsEC , data = dat ,type = "l", ylim = lims, xlab = labRsd, ylab = labTempAnoK)
grid()
title("dry", line = 0.3)

polygon(dat$IncomingShortwave_ObsEC, dat$dm_AirTemperature_ObsEC, col = makeTransparent(colTair,70), border = colTair)
polygon(dat$IncomingShortwave_ObsEC, dat$dm_SkinTemperature_ObsEC, col = makeTransparent(colTs,70), border = colTs)
polygon(dat$IncomingShortwave_ObsEC, dat$dT_ObsEC, col = makeTransparent(coldT,70), border = coldT)

# lines(dm_SkinTemperature_ObsEC~ IncomingShortwave_ObsEC , data = dat ,type = "l",col = 2)
# lines(I(SkinTemperature_ObsEC - AirTemperature_ObsEC) ~ IncomingShortwave_ObsEC , data = dat ,type = "l",col = 4, lwd = 3)
x0 = dat[,IncomingShortwave_ObsEC]; x1 = dat[,IncomingShortwave_ObsEC];
y0 = dat[, dm_AirTemperature_ObsEC + sd_dm_AirTemperature_ObsEC]
y1 = dat[, dm_AirTemperature_ObsEC - sd_dm_AirTemperature_ObsEC]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = "darkgrey")
ax0 = dat[hour == 7,IncomingShortwave_ObsEC];  ax1 = dat[hour == 8,IncomingShortwave_ObsEC];
ay0 = dat[hour == 7,dm_AirTemperature_ObsEC];
ay1 = dat[hour == 8,dm_AirTemperature_ObsEC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = 1, lwd = 2)

y0 = dat[, dm_SkinTemperature_ObsEC + sd_dm_SkinTemperature_ObsEC  ]
y1 = dat[, dm_SkinTemperature_ObsEC - sd_dm_SkinTemperature_ObsEC  ]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = rgb(1,0,0,0.5))
# text(dat[ ,IncomingShortwave_ObsEC], dat[, SensibleHeatFlux_ObsEC], dat[, hour], pos = 1, col = 2)
ay0 = dat[hour == 7,dm_SkinTemperature_ObsEC];
ay1 = dat[hour == 8,dm_SkinTemperature_ObsEC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = "red", lwd = 2)

y0 = dat[, dT_ObsEC + sd_dT_ObsEC  ]
y1 = dat[, dT_ObsEC - sd_dT_ObsEC  ]
# arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = rgb(0,0,1,0.5))
ay0 = dat[hour == 7,dT_ObsEC];
ay1 = dat[hour == 8,dT_ObsEC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = 4, lwd = 2)

# legend("topleft", c(expression(bold("Surface Temperature "*T[s])), expression(bold("Air Temperature "*T[a])), expression(bold(T[s]-T[a]))), col = c(2,1,4), lty = c(1,1,1), bty = "n")
legend("topleft", c(expression("Surface Temperature "*T[s]), expression("Air Temperature "*T[a]), expression(T[s]-T[a])), col = c(2,1,4), lty = c(1,1,1), bty = "n")

## wet panel
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "wet", ]
plot(dm_AirTemperature_ObsEC  ~ IncomingShortwave_ObsEC , data = dat ,type = "l", ylim = lims, xlab = labRsd, ylab = "")
grid()
title("wet", line = 0.3)

polygon(dat$IncomingShortwave_ObsEC, dat$dm_AirTemperature_ObsEC, col = makeTransparent(colTair,70), border = colTair)
polygon(dat$IncomingShortwave_ObsEC, dat$dm_SkinTemperature_ObsEC, col = makeTransparent(colTs,70), border = colTs)
polygon(dat$IncomingShortwave_ObsEC, dat$dT_ObsEC, col = makeTransparent(coldT,70), border = coldT)

# lines(dm_SkinTemperature_ObsEC~ IncomingShortwave_ObsEC , data = dat ,type = "l",col = 2)
# lines(I(SkinTemperature_ObsEC - AirTemperature_ObsEC) ~ IncomingShortwave_ObsEC , data = dat ,type = "l",col = 4, lwd = 3)

x0 = dat[,IncomingShortwave_ObsEC]; x1 = dat[,IncomingShortwave_ObsEC];
y0 = dat[, dm_AirTemperature_ObsEC + sd_dm_AirTemperature_ObsEC]
y1 = dat[, dm_AirTemperature_ObsEC - sd_dm_AirTemperature_ObsEC]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = "darkgrey")
ax0 = dat[hour == 7,IncomingShortwave_ObsEC];  ax1 = dat[hour == 8,IncomingShortwave_ObsEC];
ay0 = dat[hour == 7,dm_AirTemperature_ObsEC];
ay1 = dat[hour == 8,dm_AirTemperature_ObsEC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = 1, lwd = 2)

y0 = dat[, dm_SkinTemperature_ObsEC + sd_dm_SkinTemperature_ObsEC  ]
y1 = dat[, dm_SkinTemperature_ObsEC - sd_dm_SkinTemperature_ObsEC  ]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = rgb(1,0,0,0.5))
# text(dat[ ,IncomingShortwave_ObsEC], dat[, SensibleHeatFlux_ObsEC], dat[, hour], pos = 1, col = 2)
ay0 = dat[hour == 7,dm_SkinTemperature_ObsEC];
ay1 = dat[hour == 8,dm_SkinTemperature_ObsEC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = "red", lwd = 2)

y0 = dat[, dT_ObsEC + sd_dT_ObsEC  ]
y1 = dat[, dT_ObsEC - sd_dT_ObsEC  ]
# arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = rgb(0,0,1,0.5))
ay0 = dat[hour == 7,dT_ObsEC];
ay1 = dat[hour == 8,dT_ObsEC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = 4, lwd = 2)

gg = dev.off()
system(paste("pdfcrop ",gname,gname))
system(paste("pdftoppm -singlefile -rx 300 -ry 300 -png ",gname, gname))


###############################
### vapour transfer hysteresis
### SaturationVaporPressureAir_obsEC is not available

grep("Satur",  colnames(dtsebwide) )
grep("Satur",  colnames(dtsebwide_byhouravg_sunnywetdry) )

### Saturation at AirTemperature
##' @FIGURE 11 VPDAir_Hysteresis ---------------------
gname = paste(pfig,"fig11_CAOSIIPN_VPDAir_Hysteresis_sunnywetdry.pdf",sep="")
pdf(gname,11,6)
par(mfrow = c(1,2))
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "dry", ]
lims = c(0,52)
op = par(mar = c(4,5,3,0.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01, pty = "sq")
plot(SaturationVaporPressureAir_obsEC  ~ IncomingShortwave_ObsEC , data = dat ,type = "l", ylim = lims, xlab = labRsd, ylab = labvp)
title("dry", line = 0.3)
## adjust the transparent colors using colorbrewer stuff for red and blue
# polygon(dat$IncomingShortwave_ObsEC, dat$SaturationVaporPressureAir_obsEC, col = rgb(252,174,145, maxColorValue = 255), border = colesat)
polygon(dat$IncomingShortwave_ObsEC, dat$SaturationVaporPressureAir_obsEC, col = rgb(255,185,185, maxColorValue = 255), border = colesat)
polygon(dat$IncomingShortwave_ObsEC, dat$VaporPressureDeficitAir, col = rgb(185,185,255,maxColorValue = 255), border = colVPD)
polygon(dat$IncomingShortwave_ObsEC, dat$VaporPressureAir_obsEC, col = rgb(1-70/255,1-70/255,1-70/255) , border = coleair)
lines(dat$IncomingShortwave_ObsEC, dat$SaturationVaporPressureAir_obsEC, col =  colesat)


x0 = dat[,IncomingShortwave_ObsEC]; x1 = dat[,IncomingShortwave_ObsEC];
# dat[, sd_SaturationVaporPressureAir_obsEC]
y0 = dat[, SaturationVaporPressureAir_obsEC + sd_SaturationVaporPressureAir_obsEC  ]
y1 = dat[, SaturationVaporPressureAir_obsEC - sd_SaturationVaporPressureAir_obsEC  ]
# arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = makeTransparent(colesat,70))
ax0 = dat[hour == 7,IncomingShortwave_ObsEC];  ax1 = dat[hour == 8,IncomingShortwave_ObsEC];
ay0 = dat[hour == 7,SaturationVaporPressureAir_obsEC];
ay1 = dat[hour == 8,SaturationVaporPressureAir_obsEC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colesat, lwd = 2)

# lines(VaporPressureDeficitSurfaceAir   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", ylim = lims, col = 2)
y0 = dat[, VaporPressureDeficitAir + sd_VaporPressureDeficitAir  ]
y1 = dat[, VaporPressureDeficitAir - sd_VaporPressureDeficitAir  ]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = makeTransparent(colVPD,70))
# text(dat[ ,IncomingShortwave_ObsEC], dat[, SensibleHeatFlux_ObsEC], dat[, hour], pos = 1, col = 2)
ay0 = dat[hour == 7,VaporPressureDeficitAir];
ay1 = dat[hour == 8,VaporPressureDeficitAir];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colVPD, lwd = 2)

# lines(VaporPressureAir_obsEC   ~ IncomingShortwave_ObsEC , data = dat , col = 4)
y0 = dat[, VaporPressureAir_obsEC + sd_VaporPressureAir_obsEC  ]
y1 = dat[, VaporPressureAir_obsEC - sd_VaporPressureAir_obsEC  ]
# arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = makeTransparent(coleair,70))
ay0 = dat[hour == 7,VaporPressureAir_obsEC];
ay1 = dat[hour == 8,VaporPressureAir_obsEC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = coleair, lwd = 2)
legend("topleft", c("Saturation Vapor Pressure @Air", "Vapor Pressure Deficit @Air", "Vapor Pressure @Air"), col = c(colesat,colVPD,coleair), lty = c(1,1,1), lwd = 2.5, bty = "n")

## wet panel
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "wet", ]
op = par(mar = c(4,2,3,3.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01,pty = "sq")
plot(SaturationVaporPressureAir_obsEC  ~ IncomingShortwave_ObsEC , data = dat ,type = "l", ylim = lims, xlab = labRsd, ylab = "")
title("wet",line = 0.3)

polygon(dat$IncomingShortwave_ObsEC, dat$SaturationVaporPressureAir_obsEC, col = rgb(255,185,185, maxColorValue = 255), border = colesat)
polygon(dat$IncomingShortwave_ObsEC, dat$VaporPressureDeficitAir, col = rgb(185,185,255,maxColorValue = 255), border = colVPD)
polygon(dat$IncomingShortwave_ObsEC, dat$VaporPressureAir_obsEC, col = rgb(1-70/255,1-70/255,1-70/255) , border = coleair)
lines(dat$IncomingShortwave_ObsEC, dat$SaturationVaporPressureAir_obsEC, col =  colesat)

x0 = dat[,IncomingShortwave_ObsEC]; x1 = dat[,IncomingShortwave_ObsEC];
# dat[, sd_SaturationVaporPressureAir_obsEC]
y0 = dat[, SaturationVaporPressureAir_obsEC + sd_SaturationVaporPressureAir_obsEC  ]
y1 = dat[, SaturationVaporPressureAir_obsEC - sd_SaturationVaporPressureAir_obsEC  ]
# arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = makeTransparent(colesat,70))
ax0 = dat[hour == 7,IncomingShortwave_ObsEC];  ax1 = dat[hour == 8,IncomingShortwave_ObsEC];
ay0 = dat[hour == 7,SaturationVaporPressureAir_obsEC];
ay1 = dat[hour == 8,SaturationVaporPressureAir_obsEC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colesat, lwd = 2)

# lines(VaporPressureDeficitSurfaceAir   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", ylim = lims, col = 2)
y0 = dat[, VaporPressureDeficitAir + sd_VaporPressureDeficitAir  ]
y1 = dat[, VaporPressureDeficitAir - sd_VaporPressureDeficitAir  ]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = makeTransparent(colVPD,70))
# text(dat[ ,IncomingShortwave_ObsEC], dat[, SensibleHeatFlux_ObsEC], dat[, hour], pos = 1, col = 2)
ay0 = dat[hour == 7,VaporPressureDeficitAir];
ay1 = dat[hour == 8,VaporPressureDeficitAir];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colVPD, lwd = 2)

# lines(VaporPressureAir_obsEC   ~ IncomingShortwave_ObsEC , data = dat , col = 4)
y0 = dat[, VaporPressureAir_obsEC + sd_VaporPressureAir_obsEC  ]
y1 = dat[, VaporPressureAir_obsEC - sd_VaporPressureAir_obsEC  ]
# arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = makeTransparent(coleair,70))
ay0 = dat[hour == 7,VaporPressureAir_obsEC];
ay1 = dat[hour == 8,VaporPressureAir_obsEC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = coleair, lwd = 2)

gg = dev.off()
system(paste("pdfcrop ",gname,gname))
system(paste("pdftoppm -singlefile -rx 300 -ry 300 -png ",gname, gname))

##' @FIGURE 10  ga_noontime_boxstripplot -------
##### wet dry differences on inferred ga
#' @version 0.14 2017-04-21
#' @version 0.14 2018-01-19 use  ga_thom in surface conductance inverted by PM with LE_BRC
#' @version 0.14 2018-05-22 add FAO-PM which is simply u/208
#' DONE invert PM to get gc / done by Kansiska
#' DONE use u* and windspeed to get ga
#'

gavars = c( "ga_STIC",  "ga_TSEB", "ga_OSEB","ga_PMFAO", "ga_Hinvt", "ga_thom",  "ga_ustaru")
### while melting make sure that ONLY same time steps are kept
# dtsebwidemelt = melt(dtsebwide, id.var = c("Date", "Time"), measure.vars = gavars)
# # http://stackoverflow.com/questions/28918522/r-extract-complete-cases-included-observations-from-linear-model-or-formula-var
# na.omit(dtsebwide, cols = gavars)
(dtsebwidemeltgav = melt(na.omit(dtsebwide, cols = gavars), id.var = c("Date", "Time", "wetdry"), measure.vars = gavars))

### now separate per Time  and drywet and sunny
lims = c(0,0.06)
tims = 10:14
ngav = length(gavars)
# datdry = dtsebwidemeltgav[variable %in% gavars & hour(Time) %in% tims & wetdry == "dry" & Date %in% sunnydates, ]
dat = dtsebwidemeltgav[variable %in% gavars & hour(Time) %in% tims & Date %in% sunnydates, ]
levels(dat$variable)
# levels(dat$variable) <- c("FAO PM", "STIC", "TSEB", "OSEB", "H invert", "Thom 1972", "u*2/u")
levels(dat$variable) <- c( "STIC", "TSEB", "OSEB", "FAO PM", "H invert", "Thom 1972", "u*2/u")

gname = paste(pfig,"fig10_CAOSIIPN_ga_noontime_boxstripplot_sunnywetdry.pdf",sep="")
pdf(gname,6,6)
op = par(mar = c(4,5,3,0.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01, pty = "sq")
boxplot(value ~ variable, data = dat[wetdry == "dry", ],range = 0, horizontal = TRUE, ylim = lims, xlab = labga, boxwex = 0.4, col = rgb(1,0,0,0.3), at = 0:(ngav-1) + 0.6)
boxplot(value ~ variable, data = dat[wetdry == "wet", ],range = 0, horizontal = TRUE, ylim = lims, xlab = labga, yaxt = "n", boxwex = 0.4, col = rgb(0,0,1,.3), at = 0:(ngav-1) + 0.3, add = TRUE)
legend("topright", c("dry", "wet"), fill = c(2,4), bty = "n")
# stripchart(phaselagtime ~ varplotorder, data = dat[wetdry == "dry", ], at = 0:(ngav-1) + 0.6,      method = "jitter", add = TRUE, pch = 20, col = 2)
stripchart(value ~ variable, data = dat[wetdry == "dry", ], at = 0:(ngav-1) + 0.6,
          method = "jitter", add = TRUE, pch = 0, col = 2, cex = 0.5)
stripchart(value ~ variable, data = dat[wetdry == "wet", ], at = 0:(ngav-1) + 0.3,
                      method = "jitter", add = TRUE, pch = 1, col = 4, cex = 0.5)
boxplot(value ~ variable, data = dat[wetdry == "dry", ],range = 0, horizontal = TRUE, ylim = lims, xlab = labga, boxwex = 0.4, col = rgb(1,0,0,0.3), at = 0:(ngav-1) + 0.6, axes = FALSE, add = TRUE)
boxplot(value ~ variable, data = dat[wetdry == "wet", ],range = 0, horizontal = TRUE, ylim = lims, xlab = labga, yaxt = "n", boxwex = 0.4, col = rgb(0,0,1,.3), at = 0:(ngav-1) + 0.3, add = TRUE, axes = FALSE)

gg = dev.off()
system(paste("pdfcrop ",gname,gname))
system(paste("pdftoppm -singlefile -rx 300 -ry 300 -png ",gname, gname))


##' @FIGURE 7 Phase lag by Camuffo -----
varphaseLE = c("LE_BRC", "LatentHeatFlux_ObsEC", "LEPT", "LE_PMFAO", "LatentHeatFlux_model_OSEB", "LatentHeatFlux_model_TSEB", "LatentHeatFlux_model_STIC")
varphaseLEplotname = c("Observations (corr.)","Observations", "Priestley Taylor", "FAO Penman Monteith" ,"OSEB",  "TSEB", "STIC")

dat = droplevels(dtseb_camufforeg[Rsd2Rsdpot > 0.85 & variable %in% varphaseLE,])
dat[ , varplotorder := factor(variable, levels = rev(varphaseLE))]

ngav = length(varphaseLE)
levels(dat$variable)
levels(dat$varplotorder)
levels(dat$varplotorder) = rev(varphaseLEplotname)

labPhase = expression(bold("Phase lag to Incoming Solar Radiation ")(min))

gname = paste(pfig,"fig07_CAOSIIPN_LELagCamuffo_boxplot_sunnywetdry.pdf",sep="")
pdf(gname,6.5,6)
op = par(mar = c(5,9,3,0.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01, pty = "sq")
boxplot(phaselagtime ~ varplotorder, data = dat[wetdry == "dry", ], horizontal = TRUE, range = 0,
        xlab = labPhase, boxwex = 0.4, col = rgb(1,0,0,0.3), at = 0:(ngav-1) + 0.6, ylim = c(-25,50))
# stripchart(phaselagtime ~ varplotorder, data = dat[wetdry == "dry", ], at = 0:(ngav-1) + 0.6,      method = "jitter", add = TRUE, pch = 20, col = 2)
stripchart(phaselagtime ~ varplotorder, data = dat[wetdry == "dry" & slope2_pvalue < 0.05, ], at = 0:(ngav-1) + 0.6,
           method = "jitter", add = TRUE, pch = 15, col = 2)
stripchart(phaselagtime ~ varplotorder, data = dat[wetdry == "dry" & slope2_pvalue >= 0.05, ], at = 0:(ngav-1) + 0.6,
           method = "jitter", add = TRUE, pch = 0, col = 2)

abline(v = 0, col = "grey")

grid()
boxplot(phaselagtime ~ varplotorder, data = dat[wetdry == "wet", ], horizontal = TRUE,
        range = 0,  xlab = "", yaxt = "n", boxwex = 0.4, col = rgb(0,0,1,0.3), at = 0:(ngav-1) + 0.3, add = TRUE)
# stripchart(phaselagtime ~ varplotorder, data = dat[wetdry == "wet", ], at = 0:(ngav-1) + 0.3, method = "jitter", add = TRUE, pch = 19, col = 4)
stripchart(phaselagtime ~ varplotorder, data = dat[wetdry == "wet" & slope2_pvalue < 0.05, ], at = 0:(ngav-1) + 0.3, method = "jitter", add = TRUE, pch = 16, col = 4)
stripchart(phaselagtime ~ varplotorder, data = dat[wetdry == "wet" & slope2_pvalue >= 0.05, ], at = 0:(ngav-1) + 0.3, method = "jitter", add = TRUE, pch = 1, col = 4)
legend("topright", c("dry", "wet"), fill = c(2,4), bty = "n")
gg = dev.off()
system(paste("pdfcrop ",gname,gname))
system(paste("pdftoppm -singlefile -rx 300 -ry 300 -png ",gname, gname))


##' @FIGURE 12  varphase --------- 

varphase = c("NetRadiation_ObsEC", "SoilHeatFlux_ObsEC", "AE_ObsEC",  "LE_BRC", "H_BRC", "IncomingLongwave_ObsEC", "AirTemperature_ObsEC", "SkinTemperature_ObsEC",
             "dT_ObsEC", "VaporPressureDeficitAir")

varphaseplotname = c("Net Radiation", "Soil Heat Flux", "Available Energy",  "Latent Heat Flux", "Sensible Heat Flux", "Incoming Longwave", "Air Temperature", "Surface Temperature",
                     "Ts - Ta", "Vapor Pressure Deficit")

#### @update 2018-02-06, 2018-02-20
# dat = droplevels(dtseb_camufforeg[R2adj > 0.7 & Rsd2Rsdpot > 0.85 & variable %in% varphase,])
dat = droplevels(dtseb_camufforeg[R2adj > 0.2 & Rsd2Rsdpot > 0.85 & variable %in% varphase,])
dat[ , varplotorder := factor(variable, levels = rev(varphase))]
ngav = length(varphase)
levels(dat$variable)
levels(dat$varplotorder)
levels(dat$varplotorder) = rev(varphaseplotname)
labPhase = expression(bold("Phase lag to Incoming Solar Radiation ")(min))
dat
fwrite(
  dat[ ,.(Date,variable, varplotorder, phaselagtime, wetdry, slope1, slope1_sd, slope1_pvalue, slope2, slope2_sd, slope2_pvalue, R2adj, Rsd2Rsdpot)]
  , file = paste0(pout, "fig12_daily_phaselaginminutes.csv"))
#' @version 20180220 with jitter
gname = paste(pfig,"fig12_CAOSIIPN_varphase_boxjitterplot_sunnywetdry.pdf",sep="")
pdf(gname,6.5,6)
op = par(mar = c(5,9,3,0.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01, pty = "sq")
boxplot(phaselagtime ~ varplotorder, data = dat[wetdry == "wet", ], horizontal = TRUE, range = 0,  xlab = "", xaxt = "n", boxwex = 0.4, col = rgb(0,0,1,0.3), at = 0:(ngav-1) + 0.3, ylim = c(-40,220))
seq30min = seq(-120,240,30)
axis(1,at = seq30min)
stripchart(phaselagtime ~ varplotorder, data = dat[wetdry == "wet" & slope2_pvalue < 0.05, ], at = 0:(ngav-1) + 0.3, method = "jitter", add = TRUE, pch = 16, col = 4)
stripchart(phaselagtime ~ varplotorder, data = dat[wetdry == "wet" & slope2_pvalue >= 0.05, ], at = 0:(ngav-1) + 0.3, method = "jitter", add = TRUE, pch = 1, col = 4)

boxplot(phaselagtime  ~ varplotorder, data = dat[wetdry == "dry", ], horizontal = TRUE, range = 0, xlab = labPhase, boxwex = 0.4, col = rgb(1,0,0,0.3), at = 0:(ngav-1) + 0.6, add = TRUE, axes = FALSE)
stripchart(phaselagtime ~ varplotorder, data = dat[wetdry == "dry" & slope2_pvalue < 0.05, ], at = 0:(ngav-1) + 0.6,
           method = "jitter", add = TRUE, pch = 15, col = 2)
stripchart(phaselagtime ~ varplotorder, data = dat[wetdry == "dry" & slope2_pvalue >= 0.05, ], at = 0:(ngav-1) + 0.6,
           method = "jitter", add = TRUE, pch = 0, col = 2)

abline(v = 0, col = "grey")
abline(v = seq30min, col = "grey", lty = 3)
abline(h = (0:ngav )+0.3, col = "grey", lty = 3)

legend("topright", c("dry", "wet"), fill = c(2,4), bty = "n")
gg = dev.off()
system(paste("pdfcrop ",gname,gname))
system(paste("pdftoppm -singlefile -rx 300 -ry 300 -png ",gname, gname))




### -----------------------------------------------------
### FIGURES for extra analysis --------------------------

#### Compare with VPD as reference variable -----
### LE 2 VPD vs LE 2 Rg hysteresis such as done by Zhang et al., 2014

gname = paste(pout,"CAOSIIPN_LE_Hysteresis_2Rsd_2VPD_sunnywetdry.pdf",sep="")
pdf(gname,11,6)
par(mfrow = c(1,2))
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "dry", ]
lims = c(0,400)
op = par(mar = c(4,5,3,0.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01, pty = "sq")
plot(LE_BRC  ~ IncomingShortwave_ObsEC , data = dat ,type = "l", ylim = lims, xlab = labRsd, ylab = labLE)
title("dry", line = 1.3)

par(new = TRUE)
plot(LE_BRC  ~ VaporPressureDeficitAir , data = dat ,type = "l", col = 2, axes = FALSE, xlab = "", ylab = "", ylim = lims)
axis(3, col = 2)
mtext(3,adj = 1, text = expression(bold("VPD")), col = 2, line = 0.9)


dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "wet", ]
lims = c(0,400)
op = par(mar = c(4,5,3,0.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01, pty = "sq")
plot(LE_BRC  ~ IncomingShortwave_ObsEC , data = dat ,type = "l", ylim = lims, xlab = labRsd, ylab = labLE)
title("wet", line = 1.3)

par(new = TRUE)
plot(LE_BRC  ~ VaporPressureDeficitAir , data = dat ,type = "l", col = 2, axes = FALSE, xlab = "", ylab = "", ylim = lims)
axis(3, col = 2)
mtext(3,adj = 1, text = expression(bold("VPD")), col = 2, line = 0.9)

gg = dev.off()
system(paste("pdfcrop ",gname,gname))
# system(paste("pdftoppm -singlefile -rx 300 -ry 300 -png ",gname, gname))


gname = paste(pout,"CAOSIIPN_LE_NormalizedHysteresis_2Rsd_2VPD_sunnywetdry.pdf",sep="")
pdf(gname,11,6)
par(mfrow = c(1,2))
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "dry", ]
op = par(mar = c(4,5,3,0.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01, pty = "sq")
plot(I(LE_BRC/max(LE_BRC))  ~ I(IncomingShortwave_ObsEC/max(IncomingShortwave_ObsEC)) , data = dat ,type = "l", xlab = "Rg/Rgmax", ylab = "LE/LEmax")
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "wet", ]
lines(I(LE_BRC/max(LE_BRC))  ~ I(IncomingShortwave_ObsEC/max(IncomingShortwave_ObsEC)) , data = dat , lty = 2)
legend("topleft", c("dry", "wet"), lwd = 3, lty = c(1,2), bty = "n")

dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "dry", ]
plot(I(LE_BRC/max(LE_BRC))  ~ I(VaporPressureDeficitAir/max(VaporPressureDeficitAir)) , data = dat ,type = "l", xlab = "VPD/VPDmax", ylab = "LE/LEmax", col = 2)
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "wet", ]
lines(I(LE_BRC/max(LE_BRC))  ~ I(VaporPressureDeficitAir/max(VaporPressureDeficitAir)) , data = dat , lty = 2, col = 2)
legend("topleft", c("dry", "wet"), lwd = 3, lty = c(1,2), bty = "n")

gg = dev.off()
system(paste("pdfcrop ",gname,gname))
# system(paste("pdftoppm -singlefile -rx 300 -ry 300 -png ",gname, gname))


gname = paste(pout,"CAOSIIPN_LE_VPD_diurnal_sunnywetdry.pdf",sep="")
pdf(gname,11,6)
par(mfrow = c(1,2))
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "dry", ]
lims = c(0,400)
op = par(mar = c(4,5,3,0.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01, pty = "sq")
plot(LE_BRC  ~ hour , data = dat ,type = "l", ylim = lims, xlab = labhourday, ylab = labLE)
title("dry", line = 0.3)
grid()
par(new = TRUE)
plot(VaporPressureDeficitAir ~hour , data = dat ,type = "l", col = 2, axes = FALSE, xlab = "", ylab = "")
axis(4, col = 2)
mtext(4, text = expression(bold("VPD")), col = 2, line = 0.9, las= 0)


dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "wet", ]
plot(LE_BRC  ~ hour , data = dat ,type = "l", ylim = lims, xlab = labhourday, ylab = labLE)
title("wet", line = 0.3)
grid()
par(new = TRUE)
plot(VaporPressureDeficitAir ~hour , data = dat ,type = "l", col = 2, axes = FALSE, xlab = "", ylab = "")
axis(4, col = 2)
mtext(4, text = expression(bold("VPD")), col = 2, line = 0.9, las= 0)

gg = dev.off()
system(paste("pdfcrop ",gname,gname))
# system(paste("pdftoppm -singlefile -rx 300 -ry 300 -png ",gname, gname))


##' Surface and Aerodynamic conductance subdaily -----------
lims = c(0,.1)
gsvars = c("gs_LE_PM", "ga_STIC", "ga_OSEB", "ga_TSEB","ga_Hinvt", "ga_ustaru")
(dtsebwidemeltgsv = melt(na.omit(dtsebwide, cols = gsvars), id.var = c("Date", "Time", "wetdry"), measure.vars = gsvars))
ngsv = length(gsvars)
gname = paste(pout,"CAOSIIPN_gags_noontime_boxplot_sunnywetdry.pdf",sep="")
pdf(gname,6,6)
op = par(mar = c(4,5,3,0.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01, pty = "sq")
boxplot(value ~ variable, data = dtsebwidemeltgsv[variable %in% gsvars & hour(Time) %in% tims & wetdry == "dry" & Date %in% sunnydates, ], horizontal = TRUE, ylim = lims, xlab = labgags, boxwex = 0.4, col = 2, at = 0:(ngsv-1) + 0.6)
boxplot(value ~ variable, data = dtsebwidemeltgsv[variable %in% gsvars & hour(Time) %in% tims & wetdry == "wet" & Date %in% sunnydates, ], horizontal = TRUE, ylim = lims, xlab = labgags, yaxt = "n", boxwex = 0.4, col = 4, at = 0:(ngsv-1) + 0.3, add = TRUE)
legend("topright", c("dry", "wet"), fill = c(2,4), bty = "n")
gg = dev.off()
system(paste("pdfcrop ",gname,gname))
# system(paste("pdftoppm -singlefile -rx 300 -ry 300 -png ",gname, gname))


xyplot(ga_BM13 +   I(1/ResistanceAerodynamic_model_OSEB) +  I(1/ResistanceAerodynamic_model_TSEB) + ConductanceAerodynamicSTIC_model_STIC + ConductanceSurfaceSTIC_model_STIC ~ IncomingShortwave_ObsEC  | wetdry, data = dtsebwide_byhouravg_sunnywetdry,
 auto.key = list(space = "bottom", column = 2 ),par.settings = simpleTheme(pch=0:5, col = 1:6, lwd =c(3,2,2,2,2,2), cex = c(2,1,1,1,1,1)), type = c("b"), ylab = labga, xlab = labRsd)

### ga HYSTERESIS plot for Models and Observations
gname = paste(pout,"CAOSIIPN_ga_Hysteresis_sunnywetdry.pdf",sep="")
pdf(gname,11,6)
par(mfrow = c(1,2))
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "dry", ]
lims = c(0,0.051)
op = par(mar = c(4,5,3,0.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01, pty = "sq")
plot(ga_ustaru   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", ylim = lims, xlab = labRsd, ylab = labga, pch = pchobs, lwd = 2)
title("dry", line = 0.3)
x0 = dat[,IncomingShortwave_ObsEC]; x1 = dat[,IncomingShortwave_ObsEC];
y0 = dat[, ga_ustaru + sd_ga_ustaru  ]
y1 = dat[, ga_ustaru - sd_ga_ustaru  ]
# arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colobs)
ax0 = dat[hour == 7,IncomingShortwave_ObsEC];  ax1 = dat[hour == 8,IncomingShortwave_ObsEC];
ay0 = dat[hour == 7,ga_ustaru];
ay1 = dat[hour == 8,ga_ustaru];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.25, angle = 30, code = 2, col = colobs, lwd = 4)

lines(ga_Hinvt ~ IncomingShortwave_ObsEC, data = dat, lwd = 2, lty = 2)
ay0 = dat[hour == 7,ga_Hinvt];
ay1 = dat[hour == 8,ga_Hinvt];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.25, angle = 30, code = 2, col = colobs, lwd = 4)

lines(ga_OSEB   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", col = colOSEB, pchOSEB)
# arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colOSEB)
# text(dat[ ,IncomingShortwave_ObsEC], dat[, SensibleHeatFlux_ObsEC], dat[, hour], pos = 1, col = 2)
ay0 = dat[hour == 7,ga_OSEB];
ay1 = dat[hour == 8,ga_OSEB];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colOSEB, lwd = 3)

lines(ga_TSEB   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", col = colTSEB, pchTSEB)
ay0 = dat[hour == 7,ga_TSEB];
ay1 = dat[hour == 8,ga_TSEB];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colTSEB, lwd = 3)

lines(ga_STIC   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", col = colSTIC, pchSTIC)
ay0 = dat[hour == 7,ga_STIC];
ay1 = dat[hour == 8,ga_STIC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colSTIC, lwd = 3)

legend("topleft", c("ustar/u", "ga Hinvt", "OSEB", "TSEB", "STIC"), col = c(colobs,colobs,colOSEB,colTSEB,colSTIC), lty = c(1,2,1,1,1,1,1), lwd = c(2,2,1,1,1,1), bty = "n")

## wet panel
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "wet", ]
op = par(mar = c(4,2,3,3.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01,pty = "sq")
plot(ga_ustaru   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", ylim = lims, xlab = labRsd, ylab = "", pch = pchobs, lwd = 2)
title("wet", line = 0.3)
x0 = dat[,IncomingShortwave_ObsEC]; x1 = dat[,IncomingShortwave_ObsEC];
y0 = dat[, ga_ustaru + sd_ga_ustaru  ]
y1 = dat[, ga_ustaru - sd_ga_ustaru  ]
# arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colobs)
ax0 = dat[hour == 7,IncomingShortwave_ObsEC];  ax1 = dat[hour == 8,IncomingShortwave_ObsEC];
ay0 = dat[hour == 7,ga_ustaru];
ay1 = dat[hour == 8,ga_ustaru];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.25, angle = 30, code = 2, col = colobs, lwd = 4)

lines(ga_Hinvt ~ IncomingShortwave_ObsEC, data = dat, lwd = 2, lty = 2)
ay0 = dat[hour == 7,ga_Hinvt];
ay1 = dat[hour == 8,ga_Hinvt];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.25, angle = 30, code = 2, col = colobs, lwd = 4)

lines(ga_OSEB   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", col = colOSEB, pchOSEB)
# arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colOSEB)
# text(dat[ ,IncomingShortwave_ObsEC], dat[, SensibleHeatFlux_ObsEC], dat[, hour], pos = 1, col = 2)
ay0 = dat[hour == 7,ga_OSEB];
ay1 = dat[hour == 8,ga_OSEB];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colOSEB, lwd = 3)

lines(ga_TSEB   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", col = colTSEB, pchTSEB)
ay0 = dat[hour == 7,ga_TSEB];
ay1 = dat[hour == 8,ga_TSEB];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colTSEB, lwd = 3)

lines(ga_STIC   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", col = colSTIC, pchSTIC)
ay0 = dat[hour == 7,ga_STIC];
ay1 = dat[hour == 8,ga_STIC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colSTIC, lwd = 3)

gg = dev.off()
system(paste("pdfcrop ",gname,gname))
# system(paste("pdftoppm -singlefile -rx 300 -ry 300 -png ",gname, gname))


gname = paste(pout,"CAOSIIPN_gs_Hysteresis_sunnywetdry.pdf",sep="")
pdf(gname,11,6)
par(mfrow = c(1,2))
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "dry", ]
lims = c(0,0.051)
op = par(mar = c(4,5,3,0.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01, pty = "sq")
plot(gs_LE_PM   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", ylim = lims, xlab = labRsd, ylab = labgs, pch = pchobs, lwd = 2)
title("dry", line = 0.3)
x0 = dat[,IncomingShortwave_ObsEC]; x1 = dat[,IncomingShortwave_ObsEC];
y0 = dat[, gs_LE_PM + sd_gs_LE_PM  ]
y1 = dat[, gs_LE_PM - sd_gs_LE_PM  ]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colobs)
ax0 = dat[hour == 7,IncomingShortwave_ObsEC];  ax1 = dat[hour == 8,IncomingShortwave_ObsEC];
ay0 = dat[hour == 7,gs_LE_PM];
ay1 = dat[hour == 8,gs_LE_PM];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.25, angle = 30, code = 2, col = colobs, lwd = 4)


lines(ConductanceSurfaceSTIC_model_STIC   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", col = colSTIC, pchSTIC)
ay0 = dat[hour == 7,ConductanceSurfaceSTIC_model_STIC];
ay1 = dat[hour == 8,ConductanceSurfaceSTIC_model_STIC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colSTIC, lwd = 3)

legend("topleft", c("gs Penman-Monteith with ustar", "gs STIC"), col = c(colobs,colSTIC), lty = c(1,2,1,1,1,1,1), lwd = c(2,2,1,1,1,1), bty = "n")

## wet panel
dat = dtsebwide_byhouravg_sunnywetdry[wetdry == "wet", ]
op = par(mar = c(4,2,3,3.5), las = 1, mgp = c(1.5,0.15,0), tck = 0.01,pty = "sq")
plot(gs_LE_PM   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", ylim = lims, xlab = labRsd, ylab = "", pch = pchobs, lwd = 2)
title("wet", line = 0.3)
x0 = dat[,IncomingShortwave_ObsEC]; x1 = dat[,IncomingShortwave_ObsEC];
y0 = dat[, gs_LE_PM + sd_gs_LE_PM  ]
y1 = dat[, gs_LE_PM - sd_gs_LE_PM  ]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = 0.05, angle = 90, code = 3, col = colobs)
ax0 = dat[hour == 7,IncomingShortwave_ObsEC];  ax1 = dat[hour == 8,IncomingShortwave_ObsEC];
ay0 = dat[hour == 7,gs_LE_PM];
ay1 = dat[hour == 8,gs_LE_PM];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.25, angle = 30, code = 2, col = colobs, lwd = 4)

lines(ConductanceSurfaceSTIC_model_STIC   ~ IncomingShortwave_ObsEC , data = dat ,type = "l", col = colSTIC, pchSTIC)
ay0 = dat[hour == 7,ConductanceSurfaceSTIC_model_STIC];
ay1 = dat[hour == 8,ConductanceSurfaceSTIC_model_STIC];
arrows(x0 = ax0, y0 = ay0, x1 = ax1, y1 = ay1, length = 0.15, angle = 30, code = 2, col = colSTIC, lwd = 3)

gg = dev.off()
system(paste("pdfcrop ",gname,gname))
# system(paste("pdftoppm -singlefile -rx 300 -ry 300 -png ",gname, gname))





