# Survival Analysis of COMPAS Recidivism and Causal Inference of Absences on Scores

This includes two different common biostatistics techniques as two separate analyses.

# Gender Bias in COMPAS Recidivism Score

The COMPAS Recidivism Algorithm is commonly used to generate a score as a predictive measurement of how likely someone is to be charged with another crime with a higher score correlated with a higher probability of repeated crime. Since the algorithm used is a black box algorithm and relies on a survey that includes demographic question such as number of residents in living situation that seem unrelated with psychological criminality, many suspect that the algorithm is unfairly punishing certain groups. To see if there is a _gender bias in this algorithm_, we used a _Cox Proportional Hazards model_ to find the association between recividism survival and COMPAS score interacted with gender.

Our model suggested that there was a gender bias with women receiving higher COMPAS scores than men even with other factors being controlled. This means that a woman was predicted as more likely to recidivate than if that same person were a man. The implications are independent of the gender inequality in the criminal justice system and are only relevant to whether the COMPAS algorithm itself is biased.

## Cox Model and Survival Analysis
### 1. Data Cleaning
- Load CSV data
- Filter out repeats, cases without COMPAS scores, and time indiscrepencies
- Select variables of interest
- Filter out missing/incomplete data

### 2. Run model
- Run survival analysis on data set for days until recidivism
- Run CoxPH model of score interacted with gender on recidivism survival

### 3. Visualization with knitr and ggplot
- Create table detailing estimates
- Plot the risk of recidivism given gender and COMPAS score

# Causal Inference of 3+ Absences on Math Scores

Especially now that a pandemic has greatly changed school participation, we were interested in _whether absences can affect school performance_. The ethics of forcing students to be absent from school on top of the inability to duplicate the exact person to isolate the effect of absences presented challenges to interpreting a causal relationship between absence and performance. Luckily, _causal inference_ allows for a causal interpretation given that all other possible causal pathways are closed.

To close off other causal pathways, we used _inverted proportional weighting (IPW)_ to balance by other variables that might have an effect on math scores. Once these other possible confounders were equalized, we can isolate and estimate the _average treatement effect (ATE)_ of absences on math scores. We found that 3+ absences had a negative impact on math scores compared to 0-3 absences, verifying that being at school still helps performance at school.

## Causal Pathways and IPW
### 1. Data Cleaning and Confounders
- Recode absences as 'Treatment' or 'Control'
- Select possible confounders

### 2. Modeling with Weights
- Compare distribution of confounders between treatment and control
- Decide that great enough difference that weighting is necessary
- Weight by logistic regression and by boosting
- Run GLM given weights

# Files:
- 504 Projects.Rmd: code for both analyses
- COMPAS recidivism report.pdf: full report for survival analysis
- Causal Inf Absence Scores.pdf: full report for causal inference
