# Food Spending and Health

One of the most important aspects of our health is the food we consume, so we were interested to see the _association between how different measures of food spending and our perceived health_ (1 = Excellent Health to 5 = Poor Health). The different measures mainly included the amount of money spent at food establishments such as grocery stores and fast food restaurants as well as the number of different types of meals such as home-made or prepared. Since estimating these values likely adds variance to the overall model, we first used _random forests in Python_ to avoid standardizing and to accommodate our categorical outcome variable.

After modeling for all 5 categories of health in random forests, we realized that the variance in how people perceived their health was decreasing the prediction accuracy, since it's difficult to tell the difference between 'Fair', 'Good' and 'Very Good' health. We tried a _logistic regression model_ to confirm our findings. After the logistic model provided even lower prediction accuracy, we simplified the categories into 'Good' and 'Bad' health to hopefully decrease this variance.

The simplified random forest model performed better overall but still couldn't beat the majority default. We tried a simplified version of the logistic regression which performed even worse; and a _multi-level perceptron_ which came to the majority default conclusion.

In the end, we interpreted all these models to mean that there was too much variance unaccounted for that there was little association between food spending and health. It's very possible that not enough variables were included such as more demographics or more detailed food spending measurements.

# Steps of Analysis
## 1. Data Cleaning
- Load 3 SAS data sets for 2011, 2013, and 2015: Consumer, Health, and Diet
- Recode and merge data sets for each year
- Merge 3 years
- Create training and testing data for prediction accuracy

## 2. Data Exploration
- Analyze distribution of perceived health
- Analyze the frequency of missing/incomplete data

## 3. Run Models (Random Forest, Logistic Regression, and MLP)
- Recode health into 2 categories for simplified version
- Set up training and validation data sets to figure out best parameters
- (Write MLP model and) Run specified model

## 4. Visualization
- Figure out parameters that gave best predictive accuracy
- Calculate accuracy
- Show heatmap of predicted vs. actual health

# Files
- 507 Project (Complete).ipynb: code for analysis
- 507 final.pdf: full report
