#install and load the package
# library(devtools)
# install_github("h2oai/h2o-3/h2o-r/ensemble/h2oEnsemble-package")

library(data.table)

#load data using fread
train <- fread("train.csv", stringsAsFactors = T)
test <- fread("test.csv", stringsAsFactors = T)

#No. of rows and columns in Train
dim(train)


#No. of rows and columns in Test
dim(test)

#Look the data 
str(train)


# Base model using mean prediction
# RMSE on test data : 4982.34
sub_mean <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID, 
                       Purchase = mean(train$Purchase))
write.csv(sub_mean, file = "first_sub.csv", row.names = F)


# summarise both the data 
summary(train)
summary(test)

# Product_category_2 has different max value in train and test data set 

# combine two data set 
test[,Purchase:=mean(train$Purchase)]
combin <- rbind(train,test)


##################################################################
#
#       Data Exploration
#
################################################################

## univariate analysis

# gender variable
prop.table(table(combin$Gender))

# age variable
prop.table(table(combin$Age))

# city variable
prop.table(table(combin$City_Category))


# stay in current year varibale
prop.table(table(combin$Stay_In_Current_City_Years))

# unique values in id variable
length(unique(combin$User_ID))

# unique value in product id variable
length(unique(combin$Product_ID))


# Missing values
colSums(is.na(combin))


### Inferences we make from univarite analysis 
#
# We need to encode Gender variable into 0 and 1 (good practice).
# We'll also need to re-code the Age bins.
# Since there are three levels in City_Category, we can do one-hot encoding.
# The "4+" level of Stay_in_Current_Years needs to be revalued.
# The data set does not contain all unique IDs. This gives us enough hint for feature engineering.
# Only 2 variables have missing values. In fact, a lot of missing values,
# which could be capturing a hidden trend. We'll need to treat them differently.


## Bivariate analysis 

library(ggplot2)

# Age vs Gender
ggplot(combin, aes(Age, fill = Gender)) + geom_bar()

#Age vs City_Category
ggplot(combin, aes(Age, fill = City_Category)) + geom_bar()

#Age vs Stay_In_Current_City_Years
ggplot(combin, aes(Age, fill = Stay_In_Current_City_Years)) + geom_bar()

# to analyze categarical variable we make cross tables
library(gmodels)
CrossTable(combin$Occupation, combin$City_Category)
CrossTable(combin$Age,combin$Gender)


##create a new variable for missing values
combin[,Product_Category_2_NA := ifelse(is.na(combin$Product_Category_2),1,0)]
combin[,Product_Category_3_NA := ifelse(is.na(combin$Product_Category_3),1,0)]


#impute missing values( replace missing value with any arbitary value -999)
mean(combin$Product_Category_2,na.rm = T)
mean(combin$Product_Category_3,na.rm = T)
combin[,Product_Category_2 := ifelse(is.na(Product_Category_2), "9.8",  Product_Category_2)]
combin[,Product_Category_3 := ifelse(is.na(Product_Category_3), "12.6",  Product_Category_3)]

#set column level
levels(combin$Stay_In_Current_City_Years)[levels(combin$Stay_In_Current_City_Years) ==  "4+"] <- "4"

#recoding age groups
levels(combin$Age)[levels(combin$Age) == "0-17"] <- 0
levels(combin$Age)[levels(combin$Age) == "18-25"] <- 1
levels(combin$Age)[levels(combin$Age) == "26-35"] <- 2
levels(combin$Age)[levels(combin$Age) == "36-45"] <- 3
levels(combin$Age)[levels(combin$Age) == "46-50"] <- 4
levels(combin$Age)[levels(combin$Age) == "51-55"] <- 5
levels(combin$Age)[levels(combin$Age) == "55+"] <- 6

#convert age to numeric
combin$Age <- as.numeric(combin$Age)

#convert Gender into numeric
combin[, Gender := as.numeric(as.factor(Gender)) - 1]

################## Feature Engg. ##################
# Higher user count suggests that a particular user has purchased products multiple times
# High product count suggests that a product has been purchased many a times, 
# which shows its popularity

#User Count
combin[, User_Count := .N, by = User_ID]

#Product Count
combin[, Product_Count := .N, by = Product_ID]


# calculate the mean purchase price of a product. Because, lower the purchase price, 
# higher will be the chances of that product being bought or vice versa
# create another variable, average purchase price by user i.e. how much purchase is made by a user.

#Mean Purchase of Product
combin[, Mean_Purchase_Product := mean(Purchase), by = Product_ID]

#Mean Purchase of User
combin[, Mean_Purchase_User := mean(Purchase), by = User_ID]

# Hot encoding for city_category
library(dummies)
combin <- dummy.data.frame(combin, names = c("City_Category"), sep = "_")


#check classes of all variables
sapply(combin, class)

#converting Product Category 2 & 3
combin$Product_Category_2 <- as.integer(combin$Product_Category_2)
combin$Product_Category_3 <- as.integer(combin$Product_Category_3)


##########################################################################
#
#                 Model Building
#
###########################################################################

#Divide into train and test
c.train <- combin[1:nrow(train),]
c.test <- combin[-(1:nrow(train)),]

# As discussed above Product_Category_1 in train has some noise. Let's remove it as well by 
# selecting all rows in Product_Category_1 upto 18, thereby dropping rows which has category 
# level 19 & 20.

c.train <- c.train[c.train$Product_Category_1 <= 18,]

# load the h20 library
library(h2o)


# To launch the h2o cluster( nthread = -1 represents its uses all the cores)
localH2O <- h2o.init(nthreads = -1,max_mem_size="5g")

#to check h2o 
h2o.init()

# Transfer the data from R to h2o instance. It can be accomplished using as.h2o command.
train.h2o <- as.h2o(c.train)
test.h2o <- as.h2o(c.test)

# Using column index, we need to identify variables to be used in modeling as follows:
#check column index number
colnames(train.h2o)

#dependent variable (Purchase)
y.dep <- 14

#independent variables (dropping ID variables)
x.indep <- c(3:11,15:20)

################################################################################################
#
#                               Data Modeling 
#
################################################################################################

# GBM 
# RMSE 2516.311
gbm.model <- h2o.gbm(x = x.indep,
                     y= y.dep,
                     training_frame = train.h2o
                     )
h2o.performance(gbm.model)

#Random Forest
# RMSE : 3115.595
system.time(
  rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, 
                                    mtries = 3, max_depth = 4, seed = 1122)
)
h2o.performance(rforest.model)

# GLM
# RMSE : 3248.094
regression.model <- h2o.glm( y = y.dep, x = x.indep, training_frame = train.h2o, family = "gaussian")
h2o.performance(regression.model)

#deep learning models
# RMSE : 2486.956
system.time(
  dlearning.model <- h2o.deeplearning(y = y.dep,
                                      x = x.indep,
                                      training_frame = train.h2o,
                                      epoch = 60,
                                      hidden = c(100,100),
                                      activation = "Rectifier",
                                      seed = 1122
  )
)
h2o.performance(dlearning.model)

###############################################################################################
#
#                 GBM Model Tuning
#
###############################################################################################
# GBM gives the best result in above model. Hence tune the GBM parameter

# There are 5 important parameters
# 1. ntree
# 2. maxdepth
# 3. learning_rate and learn_rate_annealing
# 4. sample_rate and col_sample_rate
# 5. sample_rate_per_class (in case of imbalaced dataset)

# First step to find the max_depth and min_depth

hyper_params = list( max_depth = seq(10,20,2) )

grid <- h2o.grid(

    hyper_params = hyper_params,
    search_criteria = list(strategy = "Cartesian"),
    algorithm="gbm",
    grid_id="depth_grid",

    x = x.indep,
    y = y.dep,
    training_frame = train.h2o,

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

# ###################################################

# RMSE : 1894.226
# RMSE on test dataset : 2474.628756

hyper_params = list(

    max_depth = seq(minDepth,maxDepth,1),

  sample_rate = seq(0.2,1,0.01),

  col_sample_rate = seq(0.2,1,0.01),

  col_sample_rate_per_tree = seq(0.2,1,0.01),

  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),

  min_rows = 2^seq(0,log2(nrow(train))-1,1),

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

  x = x.indep,
  y = y.dep,
  training_frame = train.h2o,

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

# Compare the model
h2o.performance(gbm)
h2o.performance(gbm.model)
h2o.varimp(gbm)

# Generate the csv
predict.gbm <- as.data.frame(h2o.predict(gbm, test.h2o))
sub_dlearning <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID, Purchase = predict.gbm$predict)
write.csv(sub_dlearning, file = "gbm.csv", row.names = F)

###################################################################################################
#
#                       deep learning tuning
#
####################################################################################################

h2o.deeplearning.1 <- function(...,
                               hidden = c(50,50),
                               activation = "TanhWithDropout",
                               epochs = 500,
                               seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           seed = seed)
h2o.deeplearning.2 <- function(...,
                               hidden = c(30,30,30),
                               activation = "Tanh",
                               epochs = 500,
                               seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           seed = seed)
h2o.deeplearning.3 <- function(...,
                               hidden = c(13,13,13,13),
                               activation = "Rectifier",
                               epochs = 500,
                               max_w2 = 50,
                               seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           seed = seed)
h2o.deeplearning.4 <- function(...,
                               hidden = c(30,30,30),
                               activation = "Rectifier",
                               epochs = 500,
                               max_w2 = 50,
                               seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           seed = seed)
h2o.deeplearning.5 <- function(...,
                               hidden = c(30,30,30),
                               activation = "TanhWithDropout",
                               epochs = 500,
                               max_w2 = 10,
                               seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           seed = seed)
h2o.deeplearning.6 <- function(...,
                               hidden = c(13,13,13,13),
                               activation = "TanhWithDropout",
                               epochs = 500,
                               max_w2 = 10,
                               seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           seed = seed)
h2o.deeplearning.7 <- function(...,
                               hidden = c(13,13,13,13),
                               activation = "TanhWithDropout",
                               epochs = 500,
                               max_w2 = 10,
                               rate = 0.5,
                               seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           seed = seed)
h2o.deeplearning.8 <- function(...,
                               hidden = c(14,14,14,14),
                               activation = "TanhWithDropout",
                               epochs = 500,
                               max_w2 = 10,
                               rate = 0.5,
                               seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           seed = seed)
h2o.deeplearning.9 <- function(...,
                               hidden = c(20,20),
                               activation = "Tanh",
                               epochs = 500,
                               seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           seed = seed)
h2o.deeplearning.10 <- function(...,
                                hidden = c(20,20),
                                activation = "Rectifier",
                                epochs = 500,
                                seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           seed = seed)
h2o.deeplearning.11 <- function(...,
                                hidden = c(13,13,13,13),
                                activation = "TanhWithDropout",
                                epochs = 500,
                                seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           adaptive_rate = TRUE,
                           rho = 0.9,
                           epsilon = 1e-8,
                           rate = 0.5,
                           input_dropout_ratio = 0.0, # fraction of inputs dropout
                           hidden_dropout_ratios = c(0.2,0.0,0.0,0.0), # fraction for nodes dropout
                           seed = seed)
h2o.deeplearning.12 <- function(...,
                                hidden = c(10,10,10,10),
                                activation = "TanhWithDropout",
                                epochs = 500,
                                seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           adaptive_rate = TRUE,
                           rho = 0.9,
                           epsilon = 1e-8,
                           rate = 0.5,
                           input_dropout_ratio = 0.0, # fraction of inputs dropout
                           hidden_dropout_ratios = c(0.2,0.0,0.0,0.0), # fraction for nodes dropout
                           seed = seed)
h2o.deeplearning.13 <- function(...,
                                hidden = c(8,8,8,8),
                                activation = "TanhWithDropout",
                                epochs = 500,
                                seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           adaptive_rate = TRUE,
                           rho = 0.9,
                           epsilon = 1e-8,
                           rate = 0.5,
                           input_dropout_ratio = 0.0, # fraction of inputs dropout
                           hidden_dropout_ratios = c(0.2,0.0,0.0,0.0), # fraction for nodes dropout
                           seed = seed)
h2o.deeplearning.14 <- function(...,
                                hidden = c(8,8,8,8),
                                activation = "TanhWithDropout",
                                epochs = 500,
                                seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           adaptive_rate = TRUE,
                           rho = 0.9,
                           epsilon = 1e-8,
                           rate = 0.5,
                           input_dropout_ratio = 0.2, # fraction of inputs dropout
                           hidden_dropout_ratios = c(0.2,0.0,0.0,0.0), # fraction for nodes dropout
                           seed = seed)
h2o.deeplearning.15 <- function(...,
                                hidden = c(15,15,15,15),
                                activation = "TanhWithDropout",
                                epochs = 500,
                                seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           adaptive_rate = TRUE,
                           rho = 0.9,
                           epsilon = 1e-8,
                           rate = 0.5,
                           input_dropout_ratio = 0.0, # fraction of inputs dropout
                           hidden_dropout_ratios = c(0.2,0.0,0.0,0.0), # fraction for nodes dropout
                           seed = seed)
h2o.deeplearning.16 <- function(...,
                                hidden = c(15,15,15,15),
                                activation = "TanhWithDropout",
                                epochs = 500,
                                seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           adaptive_rate = TRUE,
                           rho = 0.9,
                           epsilon = 1e-8,
                           rate = 0.5,
                           max_w2 = 10,
                           input_dropout_ratio = 0.0, # fraction of inputs dropout
                           hidden_dropout_ratios = c(0.2,0.0,0.0,0.0), # fraction for nodes dropout
                           seed = seed)
h2o.deeplearning.17 <- function(...,
                                hidden = c(10,10,10,10),
                                activation = "TanhWithDropout",
                                epochs = 500,
                                seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           adaptive_rate = TRUE,
                           rho = 0.9,
                           epsilon = 1e-8,
                           rate = 0.5,
                           max_w2 = 10,
                           input_dropout_ratio = 0.0, # fraction of inputs dropout
                           hidden_dropout_ratios = c(0.2,0.0,0.0,0.0), # fraction for nodes dropout
                           seed = seed)
h2o.deeplearning.18 <- function(...,
                                hidden = c(30,30,30),
                                activation = "TanhWithDropout",
                                epochs = 500,
                                seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           adaptive_rate = TRUE,
                           rho = 0.9,
                           epsilon = 1e-8,
                           rate = 0.5,
                           max_w2 = 10,
                           input_dropout_ratio = 0.0, # fraction of inputs dropout
                           hidden_dropout_ratios = c(0.2,0.0,0.0), # fraction for nodes dropout
                           seed = seed)
h2o.deeplearning.19 <- function(...,
                                hidden = c(50,50,50),
                                activation = "TanhWithDropout",
                                epochs = 500,
                                seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           adaptive_rate = TRUE,
                           rho = 0.9,
                           epsilon = 1e-8,
                           rate = 0.5,
                           max_w2 = 10,
                           input_dropout_ratio = 0.0, # fraction of inputs dropout
                           hidden_dropout_ratios = c(0.2,0.0,0.0), # fraction for nodes dropout
                           seed = seed)
h2o.deeplearning.20 <- function(...,
                                hidden = c(100,100),
                                activation = "TanhWithDropout",
                                epochs = 500,
                                seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           adaptive_rate = TRUE,
                           rho = 0.9,
                           epsilon = 1e-8,
                           rate = 0.5,
                           max_w2 = 10,
                           input_dropout_ratio = 0.0, # fraction of inputs dropout
                           hidden_dropout_ratios = c(0.2,0.0), # fraction for nodes dropout
                           seed = seed)
h2o.deeplearning.21 <- function(...,
                                hidden = c(50,50),
                                activation = "TanhWithDropout",
                                epochs = 500,
                                seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           adaptive_rate = TRUE,
                           rho = 0.9,
                           epsilon = 1e-8,
                           rate = 0.5,
                           max_w2 = 10,
                           input_dropout_ratio = 0.0, # fraction of inputs dropout
                           hidden_dropout_ratios = c(0.2,0.0), # fraction for nodes dropout
                           seed = seed)
h2o.deeplearning.22 <- function(...,
                                hidden = c(10,10,10,10,10),
                                activation = "TanhWithDropout",
                                epochs = 500,
                                seed = 1)
  h2o.deeplearning.wrapper(...,
                           hidden = hidden,
                           activation = activation,
                           nesterov_accelerated_gradient = TRUE,
                           adaptive_rate = TRUE,
                           rho = 0.9,
                           epsilon = 1e-8,
                           rate = 0.5,
                           max_w2 = 10,
                           input_dropout_ratio = 0.0, # fraction of inputs dropout
                           hidden_dropout_ratios = c(0.2,0.0,0.0,0.0,0.0), # fraction for nodes dropout
                           seed = seed)


learner <- c(
  "h2o.deeplearning.2",
  "h2o.deeplearning.2",
  #"h2o.deeplearning.3",
  #"h2o.deeplearning.4",
  #"h2o.deeplearning.9",
  "h2o.deeplearning.11",
  "h2o.deeplearning.12",
  "h2o.deeplearning.15",
  "h2o.deeplearning.16",
  #"h2o.deeplearning.17",
  "h2o.deeplearning.18",
  "h2o.deeplearning.19",
  "h2o.deeplearning.20"#,
  #"h2o.deeplearning.21"
)

# define the metalearner
metalearner <- "h2o.glm.wrapper"


dl_fit <- h2o.ensemble(x = x.indep, y = y.dep, 
                    training_frame = train.h2o, 
                    family = family, 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5, shuffle = TRUE))


# RMSE : 2933 on test data set 
# generate predictions on the test set
h2o.ensemble_performance(dl_fit,newdata = test.h2o)
pred <- predict(dl_fit, test.h2o)
prediction12 <- as.data.frame(pred$pred)
dl_learning <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID, Purchase = prediction12$predict)
write.csv(dl_learning, file = "dl_ensemble.csv", row.names = F)













