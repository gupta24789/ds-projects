# Load the Library 
library(caret)
library(doParallel)
library(mlr)
library(dplyr)

# Load the datasets
train <- read.csv("new_train.csv",header = TRUE)
test <- read.csv("new_test.csv",header = TRUE)

item_id <- test$Item_Identifier
outlet_id <- test$Outlet_Identifier
item_id <- data.frame(item_id)
outlet_id <- data.frame(outlet_id)

train <- select(train,-Item_Identifier,-Outlet_Identifier)
test <- select(test,-Item_Identifier,-Outlet_Identifier)

######################################
# Feature Selection using mlr package
######################################


d.train <- makeRegrTask(data = train,target = "Item_Outlet_Sales")
impVar <- generateFilterValuesData(task = d.train,method=c("information.gain","chi.squared"))
plotFilterValues(impVar)

# List of variable by its decreasing value based on iinformation gain
impVar$data$name[order(impVar$data$information.gain,decreasing = TRUE)]


#############################################################################################
#
#                     Data Modeling using caret
#
#############################################################################################


# create train and cross validation data

index <- createDataPartition(train$Item_Outlet_Sales,p=0.7,list=FALSE)
training <- train[index,]
cvdata <-  train[-index,]


#################################### lm model #######################################################

features <- impVar$data$name[order(impVar$data$information.gain,decreasing = TRUE)][c(1,2,3,4,5,7)]

predictor <- training[,features]
target <-  data.frame(Item_Outlet_Sales = training[,"Item_Outlet_Sales"])
cvnew <- cvdata[,features]
test <- test[,features]

# How many parameter we tune for lm model 
# modelLookup("lm")

ctrl <- trainControl(method="repeatedcv",number = 10,repeats = 5,seeds = c(1:51))

set.seed(100)
cl <- makeCluster(3); registerDoParallel(cl)
lm_mod <- caret::train(x = predictor,y = target$Item_Outlet_Sales,method="lm",
                       trControl=ctrl,
                       tuneLength = 10   #secify how many random values used to find the intercept
                       )
stopCluster(cl); registerDoSEQ();
lm_mod


cvpred <- predict(lm_mod,newdata = cvnew)
caret::RMSE(cvpred,cvdata$Item_Outlet_Sales)

# RMSE in training data: 1139.664
# RMSE in cv data      : 1126.243
# RMSE in test data    : 

# Generate the csv for lm
predicted <- predict(lm_mod,test)
df <- data.frame(Item_Identifier=item_id$item_id,
                 Outlet_Identifier=outlet_id$outlet_id,
                 Item_Outlet_Sales=predicted)
write.csv(df,file = "lm.csv",row.names = F)

####################### gbm model ######################################################################

features <- impVar$data$name[order(impVar$data$information.gain,decreasing = TRUE)][c(1,2,3,4,5,7)]

predictor <- training[,features]
target <-  data.frame(Item_Outlet_Sales = training[,"Item_Outlet_Sales"])
cvnew <- cvdata[,features]
test <- test[,features]

ctrl <- trainControl(method="repeatedcv",
                     number = 10,
                     repeats = 5,
                     allowParallel = T
                     )

set.seed(100)
grid  <- expand.grid(n.trees = c(50,60,70,80,90,100),
                     interaction.depth = seq(3,7,by = 1),
                     shrinkage = c(0.1,0.01,0.001),
                     n.minobsinnode = c(5,10,15,20,25))

cl <- makeCluster(3); 
registerDoParallel(cl)
gbm_mod <- caret::train(x = predictor,y = target$Item_Outlet_Sales,method="gbm",
                       trControl=ctrl,
                       tuneGrid = grid,
                       verbose=FALSE
)
stopCluster(cl); 
registerDoSEQ();
gbm_mod$results$RMSE[order(gbm_mod$results$RMSE,decreasing = FALSE)][1:5]
gbm_mod$bestTune
print(gbm_mod)

cvpred <- predict(gbm_mod,newdata = cvnew)
caret::RMSE(cvpred,cvdata$Item_Outlet_Sales)

# RMSE in training data: 1086.409
# RMSE in cv data      : 1068.858
# RMSE in test data    : 

# Generate the csv for lm
predicted <- predict(gbm_mod,test)
df <- data.frame(Item_Identifier=item_id$item_id,
                 Outlet_Identifier=outlet_id$outlet_id,
                 Item_Outlet_Sales=predicted)
write.csv(df,file = "gbm.csv",row.names = F)

####################### rf model ######################################################################

features <- impVar$data$name[order(impVar$data$information.gain,decreasing = TRUE)][c(1,2,3,4,5,7)]

predictor <- training[,features]
target <-  data.frame(Item_Outlet_Sales = training[,"Item_Outlet_Sales"])
cvnew <- cvdata[,features]
test <- test[,features]

ctrl <- trainControl(method="repeatedcv",
                     number = 10,
                     repeats = 5,
                     allowParallel = TRUE)

set.seed(100)

cl <- makeCluster(3); 
registerDoParallel(cl)

grid <- expand.grid(mtry=seq(1,3,by=0.5))
rf_mod <- caret::train(x = predictor,y = target$Item_Outlet_Sales,
                        method="rf",
                        trControl=ctrl,
                        tuneGrid=grid
)

stopCluster(cl); 
registerDoSEQ();
rf_mod

cvpred <- predict(rf_mod,newdata = cvnew)
caret::RMSE(cvpred,cvdata$Item_Outlet_Sales)

# RMSE in training data: 1086.747
# RMSE in cv data      : 1098.725
# RMSE in test data    : 

# Generate the csv for lm
predicted <- predict(rf_mod,test)
df <- data.frame(Item_Identifier=item_id$item_id,
                 Outlet_Identifier=outlet_id$outlet_id,
                 Item_Outlet_Sales=predicted)
write.csv(df,file = "rf.csv",row.names = F)

###############################################################################################
#
#                     Data Modeling using H2o
#
###############################################################################################

library(h2o)

# intialize h2o with all cores and 5g memory
h2o.init(nthreads = -1,max_mem_size = "5g")


features <- impVar$data$name[order(impVar$data$information.gain,decreasing = TRUE)][c(1,2,3,4,5,7)]

predictor <- training[,features]
target <-  data.frame(Item_Outlet_Sales = training[,"Item_Outlet_Sales"])
cvnew <- cvdata[,features]
test <- test[,features]

# send the data to h20 cluster 
predictor <- cbind(predictor,Item_Outlet_Sales = target$Item_Outlet_Sales)
train.hex <- as.h2o(predictor,destination_frame = "train.hex")
cvnew <- cbind(cvnew,Item_Outlet_Sales = cvdata$Item_Outlet_Sales)
cv.hex <- as.h2o(cvnew,destination_frame = "cv.hex")
test.hex <- as.h2o(test,destination_frame = "test.hex")

# Make model using GBM model
names(train.hex)
gbm_h2o_mod <- h2o.gbm(x = c(1:6),y = c(7),
                       training_frame = train.hex,
                       validation_frame = cv.hex)

h2o.performance(gbm_h2o_mod)   # give training set error
h2o.performance(gbm_h2o_mod,valid = T)  # gives validation set error


# Tune the GBM model 
# First find out the min and max depth 

hyper_params <- list(
  max_depth = seq(10,20,by = 2)
)

grid <- h2o.grid(
  algorithm = "gbm",
  grid_id = "depth_grid",
  hyper_params = hyper_params,
  search_criteria = list(strategy = "Cartesian"),
  
  x = c(1:6),
  y = c(7),
  training_frame = train.hex,
  validation_frame = cv.hex,
  
  ntrees = 10000,
  learn_rate = 0.05,
  learn_rate_annealing = 0.99,
  sample_rate = 0.8,
  col_sample_rate = 0.8,
  seed = 1234,
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "MSE",
  score_tree_interval = 10
)

grid

sortedGrid <- h2o.getGrid("depth_grid", sort_by="MSE")
sortedGrid

topDepths = sortedGrid@summary_table$max_depth[1:5]
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))
minDepth
maxDepth


hyper_params = list(
  max_depth = seq(minDepth,maxDepth,1),
  sample_rate = seq(0.2,1,0.01),
  col_sample_rate = seq(0.2,1,0.01),
  col_sample_rate_per_tree = seq(0.2,1,0.01),
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),
 # min_rows = 2^seq(0,log2(nrow(train))-1,1),
  nbins = 2^seq(4,10,1),
  nbins_cats = 2^seq(4,12,1),
  min_split_improvement = c(0,1e-8,1e-6,1e-4),
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")
)

search_criteria = list(
  strategy = "RandomDiscrete",
  max_runtime_secs = 3600,
  max_models = 100,
  seed = 1234,
  stopping_rounds = 5,
  stopping_metric = "MSE",
  stopping_tolerance = 1e-3
)

grid <- h2o.grid(
  hyper_params = hyper_params,
  search_criteria = search_criteria,
  algorithm = "gbm",
  grid_id = "final_grid",
  
  x = c(1:6),
  y = c(7),
  training_frame = train.hex,
  validation_frame = cv.hex,
  
  ntrees = 10000,
  learn_rate = 0.05,
  learn_rate_annealing = 0.99,
  max_runtime_secs = 3600,
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE",
  score_tree_interval = 10,
  seed = 1234
)

sortedGrid <- h2o.getGrid("final_grid", sort_by = "MSE")
sortedGrid

for (i in 1:5) {
  gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
  print(h2o.performance(gbm, valid = TRUE))
}


gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
h2o.performance(gbm)

# RMSE in training data:729.678
# RMSE in cv data      :
# RMSE in test data    : 


# Generate the csv for h20.gbm
predicted <- as.data.frame(predict(gbm,test.hex))
df <- data.frame(Item_Identifier=item_id$item_id,
                 Outlet_Identifier=outlet_id$outlet_id,
                 Item_Outlet_Sales=predicted$predict)
write.csv(df,file = "h2o.gbm.csv",row.names = F)


############################ h2o.rf ########################################


rf_h2o_mod <- h2o.randomForest(x = c(1:6),y = c(7),
                               training_frame = train.hex,
                               validation_frame = cv.hex,
                               seed = 1000)

h2o.performance(rf_h2o_mod)
h2o.performance(rf_h2o_mod,valid = TRUE)


# RMSE in training data:1119.18
# RMSE in cv data      :1150.544
# RMSE in test data    : 


# Tune Random Forest in h2o 

hyper_params <- list(
  max_depth = seq(10,20,2),
  stopping_rounds = 2,
  stopping_tolerance = 0.001,
  ntrees = 200
)

grid.rf <- h2o.grid(
  hyper_params =hyper_params,
  algorithm = "randomForest",
  
  x = c(1:6),
  y = c(7),
  training_frame = train.hex,
  validation_frame = cv.hex,

  grid_id = "final_rf_model"
  
)

sortedGrid.rf <- h2o.getGrid(grid_id = "final_rf_model",sort_by = "MSE")
top5depth <- sortedGrid.rf@summary_table$max_depth[1:5]
minDepth <- min(as.numeric(top5depth))
maxDepth <- max(as.numeric(top5depth))
minDepth
maxDepth

hyper_params = list(
  max_depth = seq(minDepth,maxDepth,1),
  sample_rate = seq(0.2,1,0.01),
  col_sample_rate_per_tree = seq(0.2,1,0.01),
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),
  # min_rows = 2^seq(0,log2(nrow(train))-1,1),
  nbins = 2^seq(4,10,1),
  nbins_cats = 2^seq(4,12,1),
  min_split_improvement = c(0,1e-8,1e-6,1e-4),
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")
)

search_criteria = list(
  strategy = "RandomDiscrete",
  max_runtime_secs = 3600,
  max_models = 100,
  seed = 1234,
  stopping_rounds = 5,
  stopping_metric = "MSE",
  stopping_tolerance = 1e-3
)

grid.rf <- h2o.grid(
  hyper_params = hyper_params,
  algorithm = "randomForest",
  
  x = c(1:6),
  y = c(7),
  training_frame = train.hex,
  validation_frame = cv.hex,
  
  ntrees = 10000,
  max_runtime_secs = 3600,
  stopping_rounds = 5,
  stopping_tolerance = 1e-4, 
  stopping_metric = "MSE",
  score_tree_interval = 10,
  seed = 1234,
  search_criteria = search_criteria,
  grid_id = "final_rf_model1"
  
)


sortedGrid.rf <- h2o.getGrid("final_rf_model1", sort_by = "MSE")
sortedGrid.rf

rf <- h2o.getModel(sortedGrid.rf@model_ids[[1]])
h2o.performance(rf)
h2o.performance(rf,valid = T)

# RMSE in training data:1105.748
# RMSE in cv data      :1129.538
# RMSE in test data    : 
