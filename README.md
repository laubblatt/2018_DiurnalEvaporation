# Reproduce figures and tables of Renner et al., 2018

**_By Maik Renner, Max-Planck Institute for Biogeochemistry, Jena, Germany_**
> 2019-01-15

This repository provides the R script to perform data analysis and reproduce
figures and tables as shown in the scientific manuscript
    accepted in the journal Hydrology and Earth System Sciences
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

The script should download and unpack the files into a subfolder `./data/`

### How to execute the script 
Navigate into the main folder of your local clone.  
The script may be run in a shell 
```
Rscript ./R/DiurnalEvaporation_CAOS_SEBsite.r
```
Or within R 
```R
source "./R/DiurnalEvaporation_CAOS_SEBsite.r"
 ```

Make sure that the mainpath is set in accordance with your local settings. 

### Report issues here
https://github.com/laubblatt/2018_DiurnalEvaporation/issues

# Model simulations
To reproduce model simulations of OSEB and TSEB see this repository:
https://github.com/ClaireBrenner/pyTSEB_Renner_et_al_2018
