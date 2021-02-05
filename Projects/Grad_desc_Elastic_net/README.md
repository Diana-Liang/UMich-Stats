# Elastic Net Gradient Descent

Elastic net is a powerful tool as a balance between variable selection and meaningful estimates that can quickly run through the hundreds of thousands of input variables that often comes from genetics research. To better understand the elastic net implementation in R, we _wrote our own elastic net algorithms from base R using gradient and coordinate descent_. Only the gradient descent algorithm is included here.

In addition to a regular vectorized gradient descent, we also start with a simple ridge regression solution to solve for the elastic net faster and implemented a soft threshold to limit when gradient differences are no longer reliable by inherent computational errors. A CV function was included to mimic the available elastic net package and aid in choosing parameters.

We analyzed the performance of our algorithms against one another and against the most common elastic net package to compare estimates and efficiency. Overall, we believe that the current elastic net package in R utilizes a different elastic net equation since it optimizes to different estimates given the same parameters while our algorithms corroborate each other using the same equation.

# Steps of Analysis
## 1. Create dummy data
- Set actual solution
- Generate data with noise
## 2. Set up for running algorithms
- Set up different parameters to test
- Set up dataframes for estimates and efficiency measurements
## 3. Run algorithms
- Make sure each algorithm runs with error messages and updates
- Extract estimates and efficiency measurements to put into tables
## 4. Include CV Test Case

## 5. Visualize with ggplot
- Make estimates facet grid
  - Pivot estimates longer
  - Merge multiple algorithm outputs
- Plot efficiency measurements
- Plot run time

# Files
- grad_desc_vector.R: code for gradient descent elastic net algorithm
- universal_cv.R: code for best parameters by CV
- universal_analysis.R: code for analyzing algorithm performance
- BIOSTAT 615 Group 1 Report.pdf: full report
