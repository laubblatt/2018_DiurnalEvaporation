#!/bin/bash
### @filename 2018_DiurnalEvaporation_makefigures.sh

#' @version 0.01 20180117 new folder and script for figures of the CAOS diurnal LE draft
#' @version 0.02 20180424 add (b) panel labels and copy figures to HESSD submission folder
#' @version 0.03 20180522 rerun figures due to labeling etc.
#' @version 1.00 20180917 collect figures from repository folder
#' @version 1.01 20180917 renaming of figures in script using fig03a_ notation
#' @version 1.01 20180917 remove the figure folder code in tex files
#' @version 1.02 20180918 get figures do labeling croping and move final figures to folder manuscript_revised
#' @version 1.03 20181120 adapt to github/laubblatt/2018_DiurnalEvaporation/
#' @version 1.04 20190115 adapt for final figures

#' @DONE figure 1 must be updated with the panel labeling in keynote, or using latex instead



pfig=~/bgc/github/laubblatt/2018_DiurnalEvaporation/figures/
# pfigms=~/bgc/ownCloud/work/manuscripts_mr/M28_diurnalHysteresis/CAOSIISynthesisET/draft/HESSD/revision1/figures
pfigrev=~/bgc/ownCloud/work/manuscripts_mr/M28_diurnalHysteresis/CAOSIISynthesisET/draft/HESSD/final/figures/
# echo $pfigms
# ls $pfigms
# ls $pfig/fig*

## go to current directory where figs should be save to
cd $pfig
ls
## I need all figures for ms in the figures folder to run latex on them
# rsync -urz --progress $pfig/fig* $pfigms
# rsync -urz --progress $pfig/tab* $pfigms

### copy Table 3 and 4
cp tab04_dtseb_camufforeg_kable_AE_html.html $pfigrev/tab04.html
cp tab03_PN_LE_daytime_stats.html $pfigrev/tab03.html


# Figure 1: sketch
# pdftoppm -png -f 2 -l 2 -singlefile -rx 300 -ry 300 -png ../sketch/Boerner/Graphics.pdf 126_Fig1
# pdftoppm -singlefile -rx 450 -ry 450 -png HysteresisLoopSketch_panels.pdf HysteresisLoopSketch_panels
pdfcrop HysteresisLoopSketch.pdf HysteresisLoopSketch.pdf
pdflatex M28_sketch_panels.tex
pdfcrop M28_sketch_panels.pdf fig01_M28_sketch_panels.pdf
# evince M28_sketch_panels.pdf
pdftoppm -singlefile -rx 450 -ry 450 -png fig01_M28_sketch_panels.pdf fig01_M28_sketch_panels
cp fig01_M28_sketch_panels.pdf $pfigrev/fig01.pdf
cp fig01_M28_sketch_panels.png $pfigrev/fig01.png


# Figure 2: map is prepared in EC_Site_PetitNobressart.key (orig Claire)
# use qpdf on mac
# pdftk EC_Site_PetitNobressart.pdf cat 1 fig02_EC_Site_PetitNobressart.pdf
# qpdf --pages EC_Site_PetitNobressart.pdf 1 -- EC_Site_PetitNobressart.pdf fig02_EC_Site_PetitNobressart.pdf
# pdfcrop fig02_EC_Site_PetitNobressart.pdf fig02_EC_Site_PetitNobressart.pdf
# pdftoppm -singlefile -rx 450 -ry 450 -png fig02_EC_Site_PetitNobressart.pdf  fig02_EC_Site_PetitNobressart
# cp fig02_EC_Site_PetitNobressart.pdf $pfigrev/fig02.pdf
# cp fig02_EC_Site_PetitNobressart.png $pfigrev/fig02.png

cp ~/bgc/ownCloud/work/manuscripts_mr/M28_diurnalHysteresis/CAOSIISynthesisET/draft/HESSD/revision1/manuscript_revised/fig02.p* $pfigrev


### FIGURE 3 Daily Time series
pdflatex M28_DailyMoistureObs.tex
pdfcrop M28_DailyMoistureObs.pdf fig03_M28_DailyMoistureObs.pdf
pdftoppm -singlefile -rx 450 -ry 450 -png  fig03_M28_DailyMoistureObs.pdf fig03_M28_DailyMoistureObs
# evince M28_DailyMoistureObs.pdf
cp fig03_M28_DailyMoistureObs.pdf $pfigrev/fig03.pdf
cp fig03_M28_DailyMoistureObs.png $pfigrev/fig03.png

### FIGURE 4 Energy and States Diurnal cycles
pdflatex M28_CAOSIIPN_DiurnalCourseRadTempHum_sunnywetdry.tex
pdfcrop M28_CAOSIIPN_DiurnalCourseRadTempHum_sunnywetdry.pdf fig04_M28_CAOSIIPN_DiurnalCourseRadTempHum_sunnywetdry.pdf
pdftoppm -singlefile -rx 450 -ry 450 -png fig04_M28_CAOSIIPN_DiurnalCourseRadTempHum_sunnywetdry.pdf fig04_M28_CAOSIIPN_DiurnalCourseRadTempHum_sunnywetdry
#evince M28_CAOSIIPN_DiurnalCourseRadTempHum_sunnywetdry.pdf
cp fig04_M28_CAOSIIPN_DiurnalCourseRadTempHum_sunnywetdry.png $pfigrev/fig04.png
cp fig04_M28_CAOSIIPN_DiurnalCourseRadTempHum_sunnywetdry.pdf $pfigrev/fig04.pdf


 ### FIGURE 5 LE diurnal cycles
pdflatex M28_CAOSIIPN_LatentHeatFlux_ObsModel_Diurnal_sunnywetdry.tex
pdfcrop M28_CAOSIIPN_LatentHeatFlux_ObsModel_Diurnal_sunnywetdry.pdf fig05_M28_CAOSIIPN_LatentHeatFlux_ObsModel_Diurnal_sunnywetdry.pdf
pdftoppm -singlefile -rx 450 -ry 450 -png fig05_M28_CAOSIIPN_LatentHeatFlux_ObsModel_Diurnal_sunnywetdry.pdf fig05_M28_CAOSIIPN_LatentHeatFlux_ObsModel_Diurnal_sunnywetdry
# evince M28_CAOSIIPN_LatentHeatFlux_ObsModel_Diurnal_sunnywetdry.pdf
cp fig05_M28_CAOSIIPN_LatentHeatFlux_ObsModel_Diurnal_sunnywetdry.png $pfigrev/fig05.png
cp fig05_M28_CAOSIIPN_LatentHeatFlux_ObsModel_Diurnal_sunnywetdry.pdf $pfigrev/fig05.pdf

# cp ../../figures/CAOSIIPN_LatentHeatFlux_ObsModel_Diurnal_sunnywetdry.pdf ./
# pdftoppm -singlefile -rx 450 -ry 450 -png  CAOSIIPN_LatentHeatFlux_ObsModel_Diurnal_sunnywetdry.pdf CAOSIIPN_LatentHeatFlux_ObsModel_Diurnal_sunnywetdry
# evince CAOSIIPN_LatentHeatFlux_ObsModel_Diurnal_sunnywetdry.pdf

 ### FIGURE 6 LE hysteresis
pdflatex M28_CAOSIIPN_LEmodels_Hysteresis_sunnywetdry.tex
pdfcrop M28_CAOSIIPN_LEmodels_Hysteresis_sunnywetdry.pdf fig06_M28_CAOSIIPN_LEmodels_Hysteresis_sunnywetdry.pdf
pdftoppm -singlefile -rx 450 -ry 450 -png fig06_M28_CAOSIIPN_LEmodels_Hysteresis_sunnywetdry.pdf fig06_M28_CAOSIIPN_LEmodels_Hysteresis_sunnywetdry
# evince M28_CAOSIIPN_LEmodels_Hysteresis_sunnywetdry.pdf
cp fig06_M28_CAOSIIPN_LEmodels_Hysteresis_sunnywetdry.png  $pfigrev/fig06.png
cp fig06_M28_CAOSIIPN_LEmodels_Hysteresis_sunnywetdry.pdf  $pfigrev/fig06.pdf

# cp ../../figures/CAOSIIPN_LEmodels_Hysteresis_sunnywetdry.pdf ./
# pdftoppm -singlefile -rx 450 -ry 450 -png CAOSIIPN_LEmodels_Hysteresis_sunnywetdry.pdf CAOSIIPN_LEmodels_Hysteresis_sunnywetdry

 ### FIGURE 7 Phase lag LE
pdftoppm -singlefile -rx 450 -ry 450 -png fig07_CAOSIIPN_LELagCamuffo_boxplot_sunnywetdry.pdf fig07_CAOSIIPN_LELagCamuffo_boxplot_sunnywetdry
cp fig07_CAOSIIPN_LELagCamuffo_boxplot_sunnywetdry.png  $pfigrev/fig07.png
cp fig07_CAOSIIPN_LELagCamuffo_boxplot_sunnywetdry.pdf  $pfigrev/fig07.pdf
# evince CAOSIIPN_LELagCamuffo_boxplot_sunnywetdry.pdf

 ### FIGURE 8 SEB hysteresis
pdflatex M28_CAOSIIPN_EnergyBalanceClosureFlux_Hysteresis_sunnywetdry.tex
pdfcrop M28_CAOSIIPN_EnergyBalanceClosureFlux_Hysteresis_sunnywetdry.pdf fig08_M28_CAOSIIPN_EnergyBalanceClosureFlux_Hysteresis_sunnywetdry.pdf
pdftoppm -singlefile -rx 450 -ry 450 -png  fig08_M28_CAOSIIPN_EnergyBalanceClosureFlux_Hysteresis_sunnywetdry.pdf fig08_M28_CAOSIIPN_EnergyBalanceClosureFlux_Hysteresis_sunnywetdry
cp fig08_M28_CAOSIIPN_EnergyBalanceClosureFlux_Hysteresis_sunnywetdry.png $pfigrev/fig08.png
cp fig08_M28_CAOSIIPN_EnergyBalanceClosureFlux_Hysteresis_sunnywetdry.pdf $pfigrev/fig08.pdf
# evince M28_CAOSIIPN_EnergyBalanceClosureFlux_Hysteresis_sunnywetdry.pdf
# cp ../../figures/CAOSIIPN_EnergyBalanceClosureFlux_Hysteresis_sunnywetdry.pdf ./
# pdftoppm -singlefile -rx 450 -ry 450 -png CAOSIIPN_EnergyBalanceClosureFlux_Hysteresis_sunnywetdry.pdf CAOSIIPN_EnergyBalanceClosureFlux_Hysteresis_sunnywetdry


 ### FIGURE 9 Temp hysteresis
pdflatex M28_CAOSIIPN_Temperatures_Hysteresis_sunnywetdry.tex
pdfcrop M28_CAOSIIPN_Temperatures_Hysteresis_sunnywetdry.pdf fig09_M28_CAOSIIPN_Temperatures_Hysteresis_sunnywetdry.pdf
# evince M28_CAOSIIPN_Temperatures_Hysteresis_sunnywetdry.pdf
pdftoppm -singlefile -rx 450 -ry 450 -png fig09_M28_CAOSIIPN_Temperatures_Hysteresis_sunnywetdry.pdf fig09_M28_CAOSIIPN_Temperatures_Hysteresis_sunnywetdry
cp fig09_M28_CAOSIIPN_Temperatures_Hysteresis_sunnywetdry.png $pfigrev/fig09.png
cp fig09_M28_CAOSIIPN_Temperatures_Hysteresis_sunnywetdry.pdf $pfigrev/fig09.pdf


## FIGURE 10 ga wetdry
# evince CAOSIIPN_ga_noontime_boxstripplot_sunnywetdry.pdf
pdftoppm -singlefile -rx 450 -ry 450 -png fig10_CAOSIIPN_ga_noontime_boxstripplot_sunnywetdry.pdf fig10_CAOSIIPN_ga_noontime_boxstripplot_sunnywetdry
cp fig10_CAOSIIPN_ga_noontime_boxstripplot_sunnywetdry.png $pfigrev/fig10.png
cp fig10_CAOSIIPN_ga_noontime_boxstripplot_sunnywetdry.pdf $pfigrev/fig10.pdf


## FIGURE 11 VPD hysteresis
pdflatex M28_CAOSIIPN_VPDAir_Hysteresis_sunnywetdry.tex
pdfcrop M28_CAOSIIPN_VPDAir_Hysteresis_sunnywetdry.pdf fig11_M28_CAOSIIPN_VPDAir_Hysteresis_sunnywetdry.pdf
pdftoppm -singlefile -rx 450 -ry 450 -png fig11_M28_CAOSIIPN_VPDAir_Hysteresis_sunnywetdry.pdf fig11_M28_CAOSIIPN_VPDAir_Hysteresis_sunnywetdry
cp fig11_M28_CAOSIIPN_VPDAir_Hysteresis_sunnywetdry.png $pfigrev/fig11.png
cp fig11_M28_CAOSIIPN_VPDAir_Hysteresis_sunnywetdry.pdf $pfigrev/fig11.pdf
# evince M28_CAOSIIPN_VPDAir_Hysteresis_sunnywetdry.pdf

## THIS IS FIGURE 12
pdftoppm -singlefile -rx 450 -ry 450 -png fig12_CAOSIIPN_varphase_boxjitterplot_sunnywetdry.pdf fig12_CAOSIIPN_varphase_boxjitterplot_sunnywetdry
cp fig12_CAOSIIPN_varphase_boxjitterplot_sunnywetdry.png $pfigrev/fig12.png
cp fig12_CAOSIIPN_varphase_boxjitterplot_sunnywetdry.pdf $pfigrev/fig12.pdf

ls -l $pfigrev

ls -l $pfigrev*.pdf
