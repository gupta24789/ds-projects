################################################################################################
# Objective : Given various features, the aim is to build a predictive model to determine      #
#             the income level for people in US. The income levels are binned at below 50K     #
#             and above 50K                                                                    #
################################################################################################

# Load the required library
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(mice)
library(mlr)
library(caret)

##############################################################################################
#Read the datasets
train <- fread("train.csv",na.strings = c(""," ","?","NA",NA))
test <- fread("test.csv",na.strings = c(""," ","?","NA",NA))

#Explore the data
str(train)
head(train)
prop.table(table(train$income_level))
unique(train$income_level)
unique(test$income_level)


#encoding is deifferent for target variable, change the encoding
train[,income_level:=as.character(ifelse(train$income_level=="-50000",0,1))]
test[,income_level:=as.character(ifelse(test$income_level=="-50000",0,1))]
str(train)
str(test)

#separate the numerical and categorical predictors
num_train <- select_if(train,is.numeric)
cat_train <- select_if(train,is.character)

num_test <- select_if(test,is.numeric)
cat_test <- select_if(test,is.character)


#remove original train and test to save memory
rm(train,test)

############################################################################################
#Visiualise numerical variables
num_tr <- function(a){
 ggplot(num_train,aes(x=a,y=..density..))+
    geom_histogram(fill="blue",color="red",bins = 100)  +
    geom_density()
  
  ggplotly()
}

#call the function 
num_tr(num_train$age)
num_tr(log(num_train$capital_losses))


num_train[,income_level:=factor(cat_train$income_level)]

ggplot(num_train,aes(x=age,y=wage_per_hour))+
  geom_point(aes(color=income_level))

num_train[,income_level:=NULL]


# Visualize categorical variables
cat_train <- cat_train[,names(cat_train):=lapply(cat_train,factor)]
cat_test <- cat_test[,names(cat_test):=lapply(cat_test,factor)]

cat_tr <- function(a){
  ggplot(cat_train,aes(x=a))+
    geom_bar(position = "dodge",aes(fill=income_level)) +
    theme(axis.text.x = element_text(angle = 60,hjust = 1))
}

cat_tr(cat_train$class_of_worker)
cat_tr(cat_train$education)

prop.table(table(cat_train$marital_status,cat_train$income_level),1)
prop.table(table(cat_train$class_of_worker,cat_train$income_level),1)

############################################################################################
### Missing value for numerical data
#train
colSums(is.na(num_train))  
#test
colSums(is.na(num_test))

# Missing value for categarica data 
#train
colSums(is.na(cat_train))
#test
colSums(is.na(cat_test))

# calculate the propability of missing values
# consider only those column which has missing value less than 5%
missTreat <- (colSums(is.na(cat_train))/nrow(cat_train)) * 100
invalidCol <- missTreat < 5

missTest <- (colSums(is.na(cat_test))/nrow(cat_test)) * 100
invalidTest <- missTest < 5

cat_train <- subset(cat_train,select = invalidCol)
cat_test <- subset(cat_test,select = invalidTest)

# replce NA by "Unavailable"
cat_train <- cat_train[,names(cat_train):=lapply(cat_train,as.character)]
for(i in seq_along(cat_train)) set(cat_train,j=i,i=which(is.na(cat_train[[i]])),value="Unavailable")
cat_train <- cat_train[,names(cat_train):=lapply(cat_train,factor)]
str(cat_train)

cat_test <- cat_test[,names(cat_test):=lapply(cat_test,as.character)]
for(i in seq_along(cat_test)) set(cat_test,j=i,i=which(is.na(cat_test[[i]])),value="Unavailable")
cat_test <- cat_train[,names(cat_test):=lapply(cat_test,factor)]
str(cat_test)

#gives the levels present in predictors
levels(cat_train$class_of_worker)
levels(cat_test$class_of_worker)

#############################################################################################
### Data Manipulation
#combine factor levels with less than 5% values
for(i in names(cat_train)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_train[[i]])) < p))
  levels(cat_train[[i]])[levels(cat_train[[i]]) %in% ld] <- "Other"
}

  
#test
for(i in names(cat_test)){
    p <- 5/100
    ld <- names(which(prop.table(table(cat_test[[i]])) < p))
    levels(cat_test[[i]])[levels(cat_test[[i]]) %in% ld] <- "Other"
}

# compare the levels in train and test
summarizeColumns(cat_train)[,"nlevs"]
summarizeColumns(cat_test)[,"nlevs"]


# remove highly correlated variables
usefulVar <- findCorrelation(x=cor(num_train),cutoff = 0.7)
num_train <- num_train[,-usefulVar,with=FALSE]
num_test <- num_test[,-usefulVar,with=FALSE]



num_train[,.N,age][order(age)]
num_train[,.N,wage_per_hour][order(-N)]

#bin age variable 0-30 31-60 61 - 90
num_train[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,
                       labels = c("young","adult","old"))]
num_train[,age := factor(age)]


num_test[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,
                    labels = c("young","adult","old"))]
num_test[,age := factor(age)]

#Bin numeric variables with Zero and MoreThanZero
num_train[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero",
                                   "MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_train[,capital_gains := ifelse(capital_gains == 0,"Zero",
                                   "MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_train[,capital_losses := ifelse(capital_losses == 0,"Zero",
                                    "MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_train[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero",
                                          "MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

num_test[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero",
                                  "MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_test[,capital_gains := ifelse(capital_gains == 0,"Zero",
                                  "MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_test[,capital_losses := ifelse(capital_losses == 0,"Zero",
                                   "MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_test[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero",
                                         "MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]


##################################################################################################
## Machine Learning 

d.train <- cbind(num_train,cat_train)
d.test <- cbind(num_test,cat_test)

#library(mlr)
train.task <- makeClassifTask(data=d.train,target = "income_level")
test.task <- makeClassifTask(data=d.test,target = "income_level")

#remove zero variance features
train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)

#get variable impotance chart
var_imp <- generateFilterValuesData(train.task,method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE)
var_imp$data$name

#undersampling 
system.time(train.under <- undersample(train.task,rate = 0.1)) #keep only 10% of majority class
table(getTaskTargets(train.under))

#oversampling
system.time(train.over <- oversample(train.task,rate=15)) #make minority class 15 times
table(getTaskTargets(train.over))

#SMOTE
system.time(train.smote <- smote(train.task,rate = 5,nn = 3))
table(getTaskTargets(train.smote))

#naive Bayes
naive_learner <- makeLearner("classif.naiveBayes",predict.type = "response")
naive_learner$par.vals <- list(laplace = 1)

#10fold CV - stratified
folds <- makeResampleDesc("CV",iters=10,stratify = TRUE)

#cross validation function
fun_cv <- function(a){
  crv_val <- resample(naive_learner,a,folds,measures = list(acc,tpr,tnr,fpr,fp,fn))
  crv_val$aggr
}

library(doParallel)
registerDoParallel(cores = 4)

fun_cv (train.task) 
fun_cv(train.under) 
fun_cv(train.over)
fun_cv(train.smote)

#train and predict
nB_model <- mlr::train(learner = naive_learner,task = train.smote)
nB_predict <- predict(nB_model,test.task)

#evaluate
nB_prediction <- nB_predict$data$response
dCM <- confusionMatrix(d.test$income_level,nB_prediction)
dCM

#calculate F measure
precision <- dCM$byClass['Pos Pred Value']
recall <- dCM$byClass['Sensitivity']

f_measure <- 2*((precision*recall)/(precision+recall))
f_measure 

#########################################################################################
#xgboost
set.seed(2002)
xgb_learner <- makeLearner("classif.xgboost",predict.type = "response")
xgb_learner$par.vals <- list(
  objective = "binary:logistic",
  eval_metric = "error",
  nrounds = 150,
  print.every.n = 50
)

#define hyperparameters for tuning
xg_ps <- makeParamSet( 
  makeIntegerParam("max_depth",lower=3,upper=10),
  makeNumericParam("lambda",lower=0.05,upper=0.5),
  makeNumericParam("eta", lower = 0.01, upper = 0.5),
  makeNumericParam("subsample", lower = 0.50, upper = 1),
  makeNumericParam("min_child_weight",lower=2,upper=10),
  makeNumericParam("colsample_bytree",lower = 0.50,upper = 0.80)
)

#define search function
rancontrol <- makeTuneControlRandom(maxit = 5L) #do 5 iterations

#5 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 5L,stratify = TRUE)

#tune parameters
train.task1 <- createDummyFeatures(train.task)
xgb_tune <- tuneParams(learner = xgb_learner, task = train.task1, resampling = set_cv, 
                         measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = xg_ps, 
                         control = rancontrol)

#set optimal parameters
xgb_new <- setHyperPars(learner = xgb_learner, par.vals = xgb_tune$x)

#train model
xgmodel <- mlr::train(xgb_new, train.task1)

#test model
test.task1 <- createDummyFeatures(test.task)
predict.xg <- predict(xgmodel, test.task1)

#make prediction
xg_prediction <- predict.xg$data$response

#make confusion matrix
xg_confused <- confusionMatrix(d.test$income_level,xg_prediction)
xg_confused

precision <- xg_confused$byClass['Pos Pred Value']
recall <- xg_confused$byClass['Sensitivity']

f_measure <- 2*((precision*recall)/(precision+recall))
f_measure

#top 20 features
filtered.data <- filterFeatures(train.task,method = "information.gain",abs = 20)
#train
filtered.data1 <- createDummyFeatures(filtered.data)
xgb_boost <- mlr::train(xgb_new,filtered.data1)

predict.xg$threshold

#xgboost AUC
xgb_prob <- setPredictType(learner = xgb_new,predict.type = "prob")

#train model
xgmodel_prob <- mlr::train(xgb_prob,train.task1)

#predict
predict.xgprob <- predict(xgmodel_prob,test.task1)

#predicted probabilities
predict.xgprob$data[1:10,]

df <- generateThreshVsPerfData(predict.xgprob,measures = list(fpr,tpr))
plotROCCurves(df)

#set threshold as 0.4
pred2 <- setThreshold(predict.xgprob,0.4)
confusionMatrix(d.test$income_level,pred2$data$response)


#set threshold as 0.3
pred3 <- setThreshold(predict.xgprob,0.30)
confusionMatrix(d.test$income_level,pred3$data$response)
