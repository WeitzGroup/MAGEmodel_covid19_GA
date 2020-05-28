# **M**etapopulation **AG**e-structured **E**pidemiological (MAGE) model for COVID-19 in Georgia, USA

This repository includes code and figures for the report "Spread of COVID-19 through Georgia, USA. Near-term projections and impacts of social distancing via a metapopulation model" which describes the development and application of an epidemiologial metapopulation model to Georgia, USA. The model includes asymptomatic and symptomatic transmission routes, subactute and critically hospitalised compartments; and age-stratified risk. Counties are chosen as the spatial units in the metapopulation framework; where movement of people between counties is modelled implicitly. The code is written in and run in Julia v1.3.1.

## Results summary:<br>
<ins>**Social distancing policies are saving lives**</ins><br>
Reopening threatens to lead to an increase in cases as the population is immunologically naïve.<br>
  
<ins>**Many more cases exist than are measured**</ins><br>
Our models suggest Georgia is detecting between one of every five to ten COVID-19 cases. Both
serological and viral shedding tests are urgently required at scale to assess the true prevalence.<br>
  
<ins>**COVID-19 is a long-term challenge**</ins><br>
Even in the best case scenarios, COVID-19 remains a threat post-Memorial Day. Relaxing social
distancing interventions now without robust plans to contain the outbreak endangers lives.<br>

<p align="center">
<a href="https://github.com/WeitzGroup/MAGEmodel_covid19_GA/blob/master/Report/GA_COVID19_summary_28Apr2020.pdf"><img src="https://raw.githubusercontent.com/WeitzGroup/MAGEmodel_covid19_GA/master/figures/Summary1a.png" width="500" alt="Read the executive report"></a>
</p>

Access the executive summary here: <br>
**[COVID-19 assessment of reopening Georgia](https://github.com/WeitzGroup/MAGEmodel_covid19_GA/blob/master/Report/GA_COVID19_summary_28Apr2020.pdf)**

Access our 2lst April report here: <br>
**[Spread of COVID-19 through Georgia, USA. Near-term projections and impacts of social distancing via a metapopulation model](https://github.com/WeitzGroup/MAGEmodel_covid19_GA/blob/master/Report/GA_COVID19_assessment_21Apr2020.pdf)**

Access our 28th May manuscript here: <br>
**[Spread of COVID-19 through Georgia, USA. Near-term projections and impacts of social distancing via a metapopulation model](https://github.com/WeitzGroup/MAGEmodel_covid19_GA/blob/master/Report/GAmetapopModel_28May2020.pdf)**

### Cite the report as:

Beckett SJ, Dominguez-Mirazo M, Lee S, Andris C, Weitz JS (2020) Spread of COVID-19 through Georgia, USA. Near-term projections and impacts of social distancing via a metapopulation model. https://github.com/WeitzGroup/MAGEmodel_covid19_GA/blob/master/Report/GA_COVID19_assessment_21Apr2020.pdf

### Cite the summary as:

Beckett SJ, Dominguez-Mirazo M, Lee S, Andris C, Weitz JS (2020) COVID-19 assessment of reopening Georgia. https://github.com/WeitzGroup/MAGEmodel_covid19_GA/blob/master/Report/GA_COVID19_summary_28Apr2020.pdf

### Cite the manuscript as:

The manuscript is in submission at medrxiv. Citation will be updated accordingly.

### Cite the code as:

Beckett SJ, Dominguez-Mirazo M, Lee S, Andris C, Weitz JS (2020) Spread of COVID-19 through Georgia, USA. Near-term projections and impacts of social distancing via a metapopulation model. Zenodo. https://doi.org/10.5281/zenodo.3759966

#### Technical details:

Simulations are run in Julia v1.3.1 in a jupyter notebook using the file: *MAGE_GA_COVID19.ipynb* .  <br>
Simulation output is saved into the *SimulationOutput* directory as csv files. <br>
Figures made for the report are generated using R 3.5.3 using the file: *MappingCovidCounty.R* . <br>
The generated figures are saved to the *figures* directory. <br>
Data required for simulations and mapping is found in the *data* directory. <br>
The reports are stored in the *Report* directory.

For the manuscritpt figures were generated in R 4.0.0 using the file: *msFigures.R* <br>
These figures are saved to the *msFigures* directory.

