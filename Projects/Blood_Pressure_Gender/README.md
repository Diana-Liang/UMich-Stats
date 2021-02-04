# Blood Pressure by Gender
We modelled the association between blood pressure and a number of factors, such as alcohol consumption and age, interacted with gender to see _how different these factors are in their association with blood pressure by gender_. LASSO was the perfect tool for selecting out factors that didn't differ as much by gender while still ranking the remaining factors by how extreme their estimated associations were.

This code is only for the analysis done in _Stata_, although we also did the analysis in R to compare the LASSO algorithms. We found that women's blood pressure was more strongly associated with caffeine and men's blood pressure was more strongly associated with alcohol. We also discovered that while both Stata and R had functioning and meaningful implementations of LASSO they still differed in the actual calculations made, which resulted in different estimates and interpretations.

# Steps in Analysis
## 1. Data Preparation
- Import 4 SAS data sets: Demographics, Blood Pressure, Day 1 Nutrients, and Day 2 Nutrients
- Drop irrelevant columns and incomplete data
- Aggregate several blood pressure readings as means
- Generate 'Day' labels for merging
- Merge 4 data sets
- Aggregate both days of data as means for each variable
- Standardize

## 2. Run LASSO Model
- Set up gender penalty
- Set up penalty weight matrix
- Run linear LASSO models using optimal parameters by CV
- Run linear LASSO models using set parameters

## 3. Visualize
- Save LASSO output tables and plots

# Files
- group_2_stata.do: code for analysis
- group_2_final.Rmd: full report
