# Reproduce figures and tables of Renner et al., 2018

**_By Maik Renner, Max-Planck Institute for Biogeochemistry, Jena, Germany 2018-11-15_**

This repository provides the R script to perform data analysis and reproduce
figures and tables as shown in the scientific manuscript
    submitted to Hydrology and Earth System Sciences
    as Renner et al., 2018 "Estimating and understanding model bias
    in simulating the diurnal cycle of evapotranspiration:
    a case study in Luxembourg" available at https://www.hydrol-earth-syst-sci-discuss.net/hess-2018-310/ .

## Instructions

Clone this project with git
`git clone https://github.com/laubblatt/2018_DiurnalEvaporation.git`

or download zip file:
`wget https://github.com/laubblatt/2018_DiurnalEvaporation/archive/master.zip`

Functions are contained in a separate repository:
https://github.com/laubblatt/phaselag

Install the **phaselag** package in R:
```R
library(devtools)
install_github("laubblatt/phaselag")
 ```
### Data
  Data are available at the GFZ Data repository.

Observations:
http://doi.org/10.5880/fidgeo.2018.024

Model output:
http://doi.org/10.5880/fidgeo.2018.019

Download and unpack the files. Copy the csv files (_dtseb_Obs.csv_, _dtseb_Mod.csv_) into the subfolder `./data/`

### Execute description
Open the main R script `./R/DiurnalEvaporation_CAOS_SEBsite.r` in a text editor.
 Set path of your git folder via the variable `mainpath = /YOUR_PATH/`
Then execute script in R.

### Report issues here
https://github.com/laubblatt/2018_DiurnalEvaporation/issues

# Model simulations
To reproduce model simulations of OSEB and TSEB see this repository:
https://github.com/ClaireBrenner/pyTSEB_Renner_et_al_2018
