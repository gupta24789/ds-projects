library(ggplot2)
library(caret)
library(doSNOW)
library(rpart.plot)
library(dplyr)

# Loan the data 
train <- read.csv("train.csv",header = T,na.strings = c(""," ","NA",NA))
test <- read.csv("test.csv",header = T,,na.strings = c(""," ","NA",NA))

# make a new variable in test data set to allow train and test
test$Loan_Status <- "None"
data.combined <- rbind(train,test)

# Make all variable names in lower case
names(data.combined) <- tolower(names(data.combined))

# Explore the data set
str(data.combined)

# Remove unnessary variable i.e. Loan_ID
data.combined$loan_id <- NULL

# Check for missing values
colSums(is.na(data.combined))

###################################################################################
#
#                              Missing value treatment  
# Gender, dependents, self_employed, loanamount, loan_amount_term, credit_history
#
##################################################################################
loan_amount_term.credit <- data.combined[which(is.na(data.combined$credit_history)),"loan_amount_term"]
df.missing.credit <- data.combined[(data.combined$loan_amount_term %in% loan_amount_term.credit) & !is.na(data.combined$credit_history),
                   c("loan_amount_term","credit_history","loan_status")]

df.missing.credit <- df.missing.credit %>% 
  group_by(loan_amount_term,credit_history) %>% 
  dplyr::summarise(count=n()) %>% 
  arrange(desc(credit_history)) %>% 
  dplyr::slice(1)

View(df.missing.credit)

# As we can see all value must be 1
data.combined$credit_history <- as.character(data.combined$credit_history)
data.combined[is.na(data.combined$credit_history),"credit_history"] <- "1"
data.combined$credit_history <- as.factor(data.combined$credit_history)
levels(data.combined$credit_history)


library(mice)
micemod <- mice(data.combined,maxit = 10)
imputed.data.combined <- complete(micemod,1)
colSums(is.na(imputed.data.combined))

###################################################################################
#
#                              Data Analysis  
#
##################################################################################


#
# Analysis of Gender variable
#
# As we can seen, 2% data is missing in Gender we can handle it later


str(imputed.data.combined$gender)
table(imputed.data.combined$gender)
prop.table(table(imputed.data.combined$gender)) * 100

ggplot(imputed.data.combined[1:614,],aes(x=gender,fill= loan_status))+
  geom_bar()+
  labs(x="gender",y="count",title="Plot for Gender variable")



#
# Analysis of Married variable
#
# Only 0.3% data is missing so we can clean this data by using mode imputation 


str(imputed.data.combined$married)
levels(imputed.data.combined$married)
table(imputed.data.combined$married)
prop.table(table(imputed.data.combined$married)) * 100


ggplot(imputed.data.combined[1:614,],aes(x=married,fill=loan_status))+
  geom_bar()+
  labs(x="married",y="count",title="Plot for Married predictor")


#
# Analysis of Dependents variable
# 
# encodind is required for "3+"

str(imputed.data.combined$dependents)
levels(imputed.data.combined$dependents)
prop.table(table(imputed.data.combined$dependents)) * 100

imputed.data.combined$dependents <- as.character(imputed.data.combined$dependents)
imputed.data.combined[which(imputed.data.combined$dependents=="3+"),"dependents"] <- "3"
imputed.data.combined$dependents <- as.factor(imputed.data.combined$dependents)

ggplot(imputed.data.combined[1:614,],aes(x=dependents,fill=loan_status))+
  geom_bar()+
  labs(x="dependents",y="count",title="Plot for denpendents predictor")


#
# Analysis of education
#


str(imputed.data.combined$education)
levels(imputed.data.combined$education)

ggplot(imputed.data.combined[1:614,],aes(x=education,fill=loan_status))+
  geom_bar()+
  labs(x="education",y="count",title="Plot for Education predictor")


ggplot(imputed.data.combined[1:614,],aes(x=education,fill=loan_status))+
  geom_bar()+
  facet_grid(.~gender)+
  labs(x="education",y="count",title="Plot for Education predictor")


#
# Analysis of self_employment
#
# 5% data is missing impute it later

str(imputed.data.combined$self_employed)
table(imputed.data.combined$self_employed)
prop.table(table(imputed.data.combined$self_employed))

ggplot(imputed.data.combined[1:614,],aes(x=self_employed,fill=loan_status))+
  geom_bar()+
  labs(x="self_employed",y="count",title="Plot for self_employed predictor")


ggplot(imputed.data.combined[1:614,],aes(x=self_employed,fill=loan_status))+
  geom_bar()+
  facet_grid(.~gender)+
  labs(x="self_employed",y="count",title="Plot for self_employed predictor")


#
# Analysis of applicantincome
#
# outlier present use caping method

str(imputed.data.combined$applicantincome)
anyNA(imputed.data.combined$applicantincome)

ggplot(imputed.data.combined[1:614,],aes(x=applicantincome,fill=loan_status))+
  geom_density()+
  labs(x="applicantincome",y="count",title="Plot for applicantincome predictor")


imputed.data.combined[imputed.data.combined$applicantincome>40000,"applicantincome"] <- 40000

# If we can use the log transformation on applicantincome
ggplot(imputed.data.combined[1:614,],aes(x=log(applicantincome),fill=loan_status))+
  geom_density()+
  labs(x="applicantincome",y="count",title="Plot for applicantincome predictor")


#
# Analysis of coapplicantincome
#
# outlier treatment is required, used caping

str(imputed.data.combined$coapplicantincome)
anyNA(imputed.data.combined$coapplicantincome)

ggplot(imputed.data.combined[1:614,],aes(x=coapplicantincome,fill=loan_status))+
  geom_density()+
  labs(x="coapplicantincome",y="count",title="Plot for coapplicantincome predictor")

imputed.data.combined[imputed.data.combined$coapplicantincome>15000,"coapplicantincome"] <- 15000

# If we use log transformation with coapplicantincome, it genrates non-finite values
ggplot(imputed.data.combined[1:614,],aes(x=log(coapplicantincome),fill=loan_status))+
  geom_density()+
  labs(x="coapplicantincome",y="count",title="Plot for coapplicantincome predictor")


# scatter plot for applicantincome and coapplicantincome
ggplot(imputed.data.combined[1:614,],aes(x=coapplicantincome,y=coapplicantincome))+
  geom_point(aes(col=loan_status))+
  labs(x="coapplicantincome",y="count",title="Plot for coapplicantincome and applicant predictor")


#
# Analysis of loanamount
#
# 27 missing value present, impute it later
# outlier treatment is also nesseceary

summary(imputed.data.combined$loanamount)

ggplot(imputed.data.combined[1:614,],aes(x=log(loanamount),fill=loan_status))+
  geom_density()+
  labs(x="loanamount",y="count",title="Plot for loanamount predictor")


#
# Analysis of loan_amount_term
# 
# 20 missing value, inpute it later 

summary(imputed.data.combined$loan_amount_term)


ggplot(imputed.data.combined[1:614,],aes(x=loan_amount_term,fill=loan_status))+
  geom_density()+
  labs(x="loan_amount_term",y="count",title="Plot for loan_amount_term predictor")

# If we treat loan_amount_term as factor
ggplot(imputed.data.combined[1:614,],aes(x=factor(loan_amount_term),fill=loan_status))+
  geom_bar()+
  labs(x="loan_amount_term",y="count",title="Plot for loan_amount_term predictor")

# As we can see it seems to be predictive i will convert it into factor after fetaure engg
prop.table(table(factor(imputed.data.combined$loan_amount_term)))


#
# Analysis of credit_history 
#
# 79 missing value, impute it later 

summary(imputed.data.combined$credit_history)

ggplot(imputed.data.combined[1:614,],aes(x=credit_history,fill=loan_status))+
  geom_density()+
  labs(x="credit_history",y="count",title="Plot for credit_history predictor")


# As we can see it contains only 0 and 1. Hence make it factor
imputed.data.combined$credit_history <- as.factor(imputed.data.combined$credit_history)

ggplot(imputed.data.combined[1:614,],aes(x=credit_history,fill=loan_status))+
  geom_bar()+
  labs(x="credit_history",y="count",title="Plot for credit_history predictor")


ggplot(imputed.data.combined[1:614,],aes(x=credit_history,fill=loan_status))+
  geom_bar()+
  facet_grid(.~gender)+
  labs(x="credit_history",y="count",title="Plot for credit_history predictor")


#
# Analysis of property_area
#

table(imputed.data.combined$property_area)
levels(imputed.data.combined$property_area)

ggplot(imputed.data.combined[1:614,],aes(x=property_area,fill=loan_status))+
  geom_bar()+
  labs(x="property_area",y="count",title="Plot for property_area predictor")


ggplot(imputed.data.combined[1:614,],aes(x=property_area,fill=loan_status))+
  geom_bar()+
  facet_grid(.~gender)+
  labs(x="property_area",y="count",title="Plot for property_area predictor")






###################################################################################
#
#                              Feature Engg.  
#
###################################################################################

# create monthly totalincome by summing applicantincome ans coapplicantincome

imputed.data.combined$monthlytotalincome <- imputed.data.combined$applicantincome + 
  imputed.data.combined$coapplicantincome

ggplot(imputed.data.combined[1:614,],aes(x=monthlytotalincome,fill=loan_status))+
  geom_density()+
  labs(x="monthlytotalincome",y="density",title="monthly total income plot")


ggplot(imputed.data.combined[1:614,],aes(x=log(monthlytotalincome),fill=loan_status))+
  geom_density()+
  labs(x="monthlytotalincome",y="density",title="monthly total income plot")



imputed.data.combined$loan_amount_term <- as.numeric(as.character(imputed.data.combined$loan_amount_term))
# create monthly EMI
R = 0.08
imputed.data.combined$EMI <- (( imputed.data.combined$loanamount * R * 
                                  (1+R)^(imputed.data.combined$loan_amount_term) ))/((1+R)^(imputed.data.combined$loan_amount_term-1))


ggplot(imputed.data.combined[1:614,],aes(x=EMI,fill=loan_status))+
  geom_density()+
  labs(x="EMI",y="density",title="EMI")

ggplot(imputed.data.combined[1:614,],aes(x=log(EMI),fill=loan_status))+
  geom_density()+
  labs(x="EMI",y="density",title="EMI")

imputed.data.combined$loan_amount_term <- as.factor(as.character(imputed.data.combined$loan_amount_term))
###################################################################################
#
#                              Check for correlation 
#
###################################################################################
str(imputed.data.combined)
imputed.data.combined$monthlytotalincome <- log(imputed.data.combined$monthlytotalincome)
imputed.data.combined$EMI <- log(imputed.data.combined$EMI)
num.data <- select_if(imputed.data.combined,is.numeric)
cor(num.data)

num.data$loanamount <- NULL
num.data$monthlytotalincome <- NULL

cat.data <- select_if(imputed.data.combined,is.factor)
cat.data <- data.frame(apply(cat.data,2,as.character))
cat.data <- data.frame(apply(cat.data,2,as.factor))
str(cat.data)

imputed.data.combined <- cbind(num.data,cat.data)
###################################################################################
#
#                              Feature Extraction using MLR package  
#
###################################################################################
str(imputed.data.combined)

library(mlr)

train.task <- mlr::makeClassifTask(data = imputed.data.combined,target = "loan_status")
impFeatures <- mlr::generateFilterValuesData(train.task,method = c("information.gain",
                                                                   "chi.squared","randomForest.importance"))
plotFilterValues(impFeatures)



###################################################################################
#
#                              Data Modeling 
#
###################################################################################
features <- c("credit_history","property_area","EMI") 
training <- imputed.data.combined[1:614,features]
target <-   as.factor(as.character(imputed.data.combined[1:614,"loan_status"]))
testing <- imputed.data.combined[615:981,features]

set.seed(100)
cv.3.index <- createMultiFolds(target,k = 10,times = 10)
ctrl.3 <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 10,
                       index = cv.3.index)


# rf model
set.seed(100)
# 79.16% accuracy on test data set
rf.1 <- caret::train(x=training,y=target,
                     method="rf",
                     tuneLength=5,
                     trControl=ctrl.3)

rf.1
predicted <- predict(rf.1,testing)
submit.df <- data.frame(Loan_ID=test$Loan_ID,Loan_Status=predicted)
write.csv(submit.df,file = "rf.csv",row.names = F)


# rpart model
# 77.7% accuracy on test data set
set.seed(100)
# 77.7% accuracy on test data set
rp.1 <- caret::train(x=training,y=target,
                     method="rpart",
                     tuneLength=5,
                     trControl=ctrl.3)


rp.1
summary(rp.1)
varImp(rp.1)
plot(varImp(rp.1))


# C5 model
set.seed(100)
# 77.7% accuracy on test data set
c5.1 <- caret::train(x=training,y=target,
                     method="C5.0",
                     tuneLength=5,
                     trControl=ctrl.3)


c5.1
summary(c5.1)
varImp(c5.1)


# gbm model
set.seed(100)
# 77.7% accuracy on test data set
gbm.1 <- caret::train(x=training,y=target,
                      method="gbm",
                      tuneLength=5,
                      trControl=ctrl.3)


gbm.1
summary(gbm.1)
varImp(gbm.1)


#xgboost
#
xg.combo.data <- select(imputed.data.combined,-loan_status)
dummies <- caret::dummyVars(~.,data = xg.combo.data)
xg.data <- predict(dummies,xg.combo.data)
xg.target <- as.factor(ifelse(target=="Y",1,0))

xg.training <- xg.data[1:614,]
xg.testing <- xg.data[615:981,]

set.seed(100)
xgT.1 <- caret::train(x=xg.training,y=xg.target,
                      method="xgbTree",
                      tuneLength=5,
                      trControl=ctrl.3)

varImp(xgT.1)
summary(xgT.1)
varImp(xgT.1)
###################################################################################
#
#                              Compare the models  
#
###################################################################################
comparison <- resamples(list(
  RPART=rp.1,
  RF=rf.1,
  C5=c5.1,
  GBM=gbm.1,
  XGTree=xgT.1
))

summary(comparison)
bwplot(comparison)

###################################################################################
#
#                              Genrate CSV  
#
###################################################################################

# Other than xg boost
predicted <- predict(c5.1,testing)
submit.df <- data.frame(Loan_ID=test$Loan_ID,Loan_Status=predicted)
write.csv(submit.df,file = "c5.csv",row.names = F)

# for xg boost
predicted <- predict(xgL.1,xg.testing)
predicted <- ifelse(predicted==1,"Y","N")
submit.df <- data.frame(Loan_ID=test$Loan_ID,Loan_Status=predicted)
write.csv(submit.df,file = "xgL.csv",row.names = F)

###################################################################################
#
#                              Emsembling of models  
#
###################################################################################

predicted.rf <- predict(rf.1,testing)
predicted.rp <- predict(rp.1,testing)
predicted.xg <- predict(xgL.1,xg.testing)

df <-  data.frame(cbind(RF=predicted.rf,RP=predicted.rp,XG=predicted.xg))
df <- data.frame(apply(df,2,as.factor))

Mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}
df$result <- apply(df,1,Mode)


predicted <- ifelse(df$result=="1","N","Y")
submit.df <- data.frame(Loan_ID=test$Loan_ID,Loan_Status=predicted)
write.csv(submit.df,file = "ensemble.csv",row.names = F)