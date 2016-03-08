setwd("/home/rook/R/kaggle")
trainData <- read.csv("news_popularity_training.csv", stringsAsFactors = T)
testData <- read.csv("news_popularity_test.csv", stringsAsFactors = T)


# ---------------------------------------#
#        Kaggle XGBoost Function         #
#----------------------------------------#
#' Andy Limber's Kaggle xgboost function
#' 
#' Runs xgboost algorithm on training and test set
#' 
#' @param train A dataframe containing the training data.
#' @param test A dataframe containing the test data.
#' @param nr Integer. The maximum number of interations.
#' @param eta Double. Controls the learning rate, 0 < eta < 1, determines the level 
#' of contribution of each tree.
#' @parma gamma Integer. Minimum loss reduction required to make a futher cut on a 
#' leaf node, a larger value for this parameter translates to a more 
#' conservative algorithm. 
#' @param mcw Integer. Minimum child weight: minimum sum of instance weight needed 
#' to form a child node.
#' @param ss Double. Subsample: ratio of the training instance.
#' @param colsbt Double. Column sample by tree: subsample ratio of columns for 
#' constructing each tree.
#' @param npt Integer. Number of parallel trees: an experimental parameter, 
#' number of trees to grow per interation.
#' @param Xtest A logical that indicates whether you want to perform some
#' cross validation. Default is FALSE. If TRUE, will print an accuracy value.
#' @param CSV A logical that indicates whether a CSV of predictions should be
#' saved. Default is TRUE.
#'
#'@return returns the testBoost dataframe
#'@import xgboost
#'@import assertthat

# package 
alXGB <-function(train, test, nr= 700, eta = 0.01, gamma = 1, mcw = 1, ss = 1, 
                 colsbt = 1, npt = 1, Xtest = FALSE, CSV = FALSE, seed = 123){
  set.seed(seed) 
  
  not_empty(test); not_empty(train);
  
  #throw an error if there are text columns
  assert_that(!("url" %in% names(train))) 
  assert_that(!("url" %in% names(test)))
  
  if(Xtest){
    assert_that(noNA(test$popularity)) # making sure we have values
    assert_that(not_empty(test$popularity))
  } 
  
  # Need to prepare data for use in xgboost
  trainPop <- as.numeric(train$popularity) - 1 # "target" variable MUST start from 0
  testPop <- as.numeric(test$popularity) - 1
  
  train$popularity = NULL # as we do not feed a formula, must blank the target column
  test$popularity = NULL
  
  # xgboost ONLY takes numerical values and matrix data types
  train <- as.matrix(apply(train, 2, as.numeric))
  test <- as.matrix(apply(test, 2, as.numeric))
  
  # To optimise need to look at the parameters of xgboost - look at xgb.train
  testBoost <- xgboost(data = train, label = trainPop,
                       nrounds = nr, eta = eta, subsample = ss, min_child_weight = mcw,
                       number_parallel_tree = npt, colsample_bytree = colsbt, 
                       objective = "multi:softprob", num_class = 5)
  
  xgbpred <- predict(testBoost, test) # this gives us a list of probabilities
  probs <- t(matrix(xgbpred, nrow=5, ncol=length(xgbpred)/5)) # transform to a matrix 
  
  predLabs <- apply(probs, 1, which.max) # get the label - lucky that things are in the right order
  
  acc <- mean(ifelse(predLabs == (testPop + 1), 1, 0)) # ch-ch-check it
  
  return(testBoost)

}
