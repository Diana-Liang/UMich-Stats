# Categorizing by Music Genre

Figuring out music genre is notoriously difficult especially since their definitions are fluid. We tested a multitude of modeling techniques to _find the model with the best predictive accuracy for music genre_. Only random forests and adaboosting are included here. Since the data set included both continuous variables such as energy and categorical variables such as key, _random forests and adaboosting_ seemed a good place to start since the two are flexible enough to handle a mixture of variable types. The added bonus was measuring variable importance to see which if any of the input variables dominated the association with genre.

After analyzing the performance of all our models, random forests performed the best with MLP (multi-layer perceptron) and KNN (K nearest neighbors) as close seconds. These performance results showed that the more flexible methods were better able to predict since these methods did not require assumptions about the distribution of any of the variables. While most of the genres were correctly predicted, some genres proved more difficult such as movie vs. opera, reggae vs. reggaeton, and jazz vs. blues.

# Steps of Analysis
# 1. Data Cleaning and Exploration
- Load CSV data
- Filter out tracks with multiple and unpopular genres
- Create training and testing data sets
- Standardize
- Plot associations between input variables with ggpairs

# 2. Run Random Forest and Adaboosting Models
- Run model with various parameters
- Choose model with best performance

# 3. Visualization
- Plot variable importance
- Find prediction accuracy

# Files
- 503_Project.Rmd: code for analysis
- Stats 503 Final Report.pdf: full report
