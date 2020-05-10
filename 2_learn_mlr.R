# Learn how to do machine learning in R using the mlr package

# Run these two lines:
library(mlr)
library(randomForest)
library(mlbench) 

# Read in some data
data('BostonHousing2')

# Have a look at the data 
str(BostonHousing2)
bh2 = BostonHousing2[,-c(1, 2, 6)] # Drop some of the bad categories

# The first step in mlr is to create a 'task' specifying the model type and the data
bh_task = makeRegrTask(id = "bh", data = bh2, target = "medv")

# Create a training and test set
n = getTaskSize(bh_task)
set.seed(123)
test_set = sample(n, size = n/4)
train_set = (1:n)[-test_set]

# To fit the model you have to create a 'learner'
bh_rf = makeLearner('regr.randomForest')

# Fit the learner to the task
mod_rf = train(bh_rf, bh_task, subset = train_set)

# Now get predictions on the test set
pred_rf = predict(mod_rf,
                  task = bh_task,
                  subset = test_set)

# You can extract bits of this via
getPredictionTruth(pred_rf)
getPredictionResponse(pred_rf)

# Get the performance
mlr::performance(pred_rf, measures = rmse)

# Classification example --------------------------------------------------

data("BreastCancer")
str(BreastCancer)
# 699 cases and 11 variables
# Consider:
# y = Class (benign vs malignant)
# X = all other variables (minus ID)
bc2 = BreastCancer[,-c(1,7)] # Col 7 has missing values

# Set up the task
bc_task = makeClassifTask(id = "bc", data = bc2, target = "Class")
bc_task

# Note here that there's quite a class imbalance - see exercise

# Let's run a Cross validation version on these:
class_learn = makeLearner("classif.randomForest",
                          predict.type = "prob", # Get predicted probabilities
                          fix.factors.prediction = TRUE) # Don't know why this isn't the default - stops factor levels from being dropped during training

# Set up 5-fold CV
desc = makeResampleDesc("CV", iters = 5)
# See also crossval which does the same thing (with less flexibility)

# Now run the CV
class_rf = resample(class_learn,
                    task = bc_task,
                    resampling = desc,
                    measures = list(mmce, auc))
# Also get the overall classification error at the end - neat

# What else can I do? -----------------------------------------------------

# Go and look at the mlr documentation at: https://mlr.mlr-org.com
# Find the full list of learners at https://mlr3learners.mlr-org.com
# mlr cheat sheet: https://cheatsheets.mlr-org.com/mlr.pdf
# Learn about the newer mlr3: https://mlr3.mlr-org.com
# Check out the mlr3 gallery https://mlr3gallery.mlr-org.com
# mlr3 book: https://mlr3book.mlr-org.com

# Exercise ----------------------------------------------------------------

# Find another data set (perhaps look in mlbench) and start predicting using different models. Borrow the code above to predict something interesting and try out different learners. See if you can find out how to compare different types of learners. Perhaps you could even use the h2o package to fit some more complex deep learning models!