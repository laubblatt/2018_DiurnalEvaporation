# R script to reproduce figures and tables of Renner et al., 2018

'By Maik Renner, Jena, Germany 2018-11-15'

This repository provides the script to perform data analysis and reproduce
figures and tables as shown in the scientific manuscript
    submitted to Hydrology and Earth System Sciences
    as Renner et al., 2018 "Estimating and understanding model bias
    in simulating the diurnal cycle of evapotranspiration:
    a case study in Luxembourg" available at https://www.hydrol-earth-syst-sci-discuss.net/hess-2018-310/ .
    All functions are contained in a separate repository:
    https://github.com/laubblatt/phaselag

## Instructions
    Install this package in R:
```R
library(devtools)
install_github("laubblatt/phaselag")
 ```
### Data
  Data are available at the GFZ Data repository.
    Observations:
http://doi.org/10.5880/fidgeo.2018.024

Modeloutput:
http://doi.org/10.5880/fidgeo.2018.019

copy the csv files into the subfolder 'data'

### Set path with the main script and execute script
