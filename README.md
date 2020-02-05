# pop4modelling
Population dynamic data for simulation modelling usage

This repo is for the data of population dynamics with birth, death processes, and population forecasts. 

To transform the data into simulation-friendly format, please find out one of the following package:

- PyDemography (Python >= 3.7)
- dp_dt (R > 3.6.1)


## Data format

#### Birth
The birth data were the time-series of number of births by single year.


#### Death
The death data were the crude death rates by single year, single age, and sex. The rates were defined as the numbers of deaths over the mid-year population estimates. 


#### Population size
The population data were the population sizes by single year, single age, and sex. 

All data in this repos were not scaled (i.e. one in population size means one person). For simplicity, we used binary sexes and 0-100 ages as the default.


## Data sources
We mainly used the data from the World Population Prospects (updated to 2019 revision) which was made available under a license CC-BY-3.0-IGO. We choose the medium variant with potential migration processes. 


However, we are happy to include the population data outside the framework or data for specific groups of populations. 


## License

We made the processed dataset available under Creative Commons Attribution 4.0 International, CC-BY-4.0

