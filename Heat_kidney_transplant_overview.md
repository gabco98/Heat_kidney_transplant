The Impact of Ambient Heat Exposure on Kidney Transplant Outcomes

Aim: This project evaluated the association between ambient heat exposure and kidney transplant outcomes.

Population: U.S adult first-time kidney-only transplant recipients from 2010–2022 with at least 2 serum creatinine measurements from 1 year post-transplant within consistent 1 year follow-up periods.
Exposure: Ambient heat exposure, defined as the proportion of days with a daily average heat index above a threshold (27–31ºC, 90,95,99 locally-relevant percentiles) for each individual.
Comparison: Higher vs. lower heat exposure.
Outcome: eGFR slope (% change from baseline), time-to-doubling of serum creatinine from baseline.

Datasets: 
- UNOS-STAR file of kidney transplant recipients with follow-up until July, 2024.
- Center-level climate data from the ERA5 reanalysis datase; income data from the 2023 American Community Survey U.S. census data.

Files:
- Data_wrangling1_population: Population selection based on inclusion criteria and eGFR calculation.
- Heat_exposure_extraction: Heat index calculation from ERA5 data.
- Data_wrangling2_exposure_mapping: Determining individual heat exposure for each participant during follow-up. 
- Data_wrangling3_missing_data_and_new_variables: Creation of new variables and multiple imputation for missing data.
- Heat_Transplant_Outcomes: Case time series and mixed-effects models for eGFR slope. Complementary log-log discrete time survival models for doubling of serum creatinine.
- Subgroup_analysis: Subgroup analysis of the mixed-effects models.
- Heat_exposure_plots: Population flowchart, center-level heat exposure map, heat exposure distribution, eGFR trajectory, survival, and hazard ratio plots.

Acknowledgements
Portions of this research were conducted on the O2 High Performance Compute Cluster, supported by the Research Computing Group, at Harvard Medical School. 
See https://it.hms.harvard.edu/our-services/research-computing for more information.
The data reported here have been supplied by the United Network for Organ Sharing as the contractor for the OPTN.
The interpretation and reporting of these data are the responsibility of the authors and in no way should be seen as an official policy of or interpretation by OPTN or the US Government. 
