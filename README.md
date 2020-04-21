# Metapopulation AGe-structured Epidemiological (MAGE) model for COVID-19 in Georgia, USA

This repository includes code and figures for the report "" which describes the development and application of an epidemiologial metapopulation model to Georgia, USA. The model includes asymptomatic and symptomatic transmission routes, subactute and critically hospitalised compartments; and age-stratified risk. Counties are chosen as the spatial units in the metapopulation framework; where movement of people between counties is modelled implicitly. The code is written in Julia and run in v1.3.1.

Access the report here:
[REPORT TITLE](REPORTLINK)

### Cite the report as:

Beckett SJ, Dominguez-Mirazo M, Lee S, Andris C, Weitz JS (2020) 

### Cite the code as:

Beckett SJ, Dominguez-Mirazo M, Lee S, Andris C, Weitz JS (2020) Code for: Spread of COVID-19 through Georgia, USA. Projections via a metapopulation model. Zenodo.

#### Technical details:

Simulations are run in Julia v1.3.1 in a jupyter notebook using the file: *MAGE_GA_COVID19.ipynb* .  <br>
Simulation output is saved into the *SimulationOutput* directory as csv files. <br>
Figures made for the report are generated using R 3.5.3 using the file: *MappingCovidCounty.R* . <br>
The generated figures are saved to the *figures* directory. <br>
Data required for simulations and mapping is found in the *data* directory
