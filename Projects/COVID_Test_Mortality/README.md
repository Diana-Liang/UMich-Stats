# COVID Tests and Mortality
With COVID-19 continuing to impact the country and even the world, we modelled mortality on the number of positive tests in the United States to see if _the volume of positive tests could be used to predict the number of deaths_. Since these data were basically count data, and we needed to account for state and longitudinal effects, we designed a _Bayesian Poisson Hierarchical model in R2Jags_.

After running the models for over 30 hours, we found that our laptops did not have the computational power to run enough iterations for proper mixing of all of the effects in question. Some state effects and time effects were not properly stabilized (and thus mixed), but the effect of the positive test variables were at least stabilized enough to be able to estimate.

We structured the analysis so that, if the estimates of the positive tests summed to around 1, the mean structure would verify a 1:1 relationship between the tests and mortality. In laymen's terms, if the estimates summed to 1, we could assume that an increase in positive tests could foreshadow a similarly scaled increase in deaths to follow. Our estimates confirmed the 1:1 mean structure but was not able to confirm a Poisson variance structure, so while an increase in positive tests likely predicts an increase in deaths the exact scale of the increase in deaths is unknown.

# Steps in Analysis
## 1. Data Cleaning
- Loaded CSV data (could reload each day to see changes!)
- Select relevant variables and filter for nonnegative number of tests and deaths
- Convert date data from categorical to time format
- Generate cumulative positive tests for weeks leading to date of observation
  - Note date of first positive test and death for each state
  - Transform date into days since first case
  - Sum number of positive tests for 0-7 days prior, 7-14 days prior, and 14-21 days prior
  - Log the cumulative positives for Poisson analysis
 
 ## 2. Create and Run Bayesian Poisson Hierarchical Model
 - Set up parameters for R2Jags
 - Create Poisson model with state level hierarchical effects
 - Control for time using spline of days since first case
 
 ## 3. Visualize
 - Check mixing and n effective for each variable
 - Check for convergence
 - Save plot outputs
 
 # Files
 - 682_project.R: code for analysis
 - BIOSTAT 682 Project Report.pdf: full report
