# ------------------------------------
# Feature Engineering and Prediction
# ------------------------------------

library(tm)
library(SnowballC)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(textcat)
library(randomForest)
library(e1071)
library(glmnet)
library(tibble)
library(RColorBrewer)


set.seed(123)
load('../../data/final_df_with_gpt_3.5.RData')