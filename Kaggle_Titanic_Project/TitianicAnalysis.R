#
# Titanic data set from Kaggle
#

# Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# convert predictors to lower case
names(train) <- tolower(names(train))
names(test) <- tolower(names(test))

# Add a "Survived" variable to the test set to allow for combining data sets
test$survived <- "None"

# Combine data sets
data.combined <- rbind(train, test)


# A bit about R data types (e.g., factors)
str(data.combined)

# analysis of survival
data.combined$survived <- as.factor(data.combined$survived)
table(data.combined$survived)

# analysis of pclass
data.combined$pclass <- as.factor(data.combined$pclass)
table(data.combined$pclass)

# Load up ggplot2 package to use for visualizations
library(ggplot2)

# Hypothesis - Rich passenger survived at a higer rate
ggplot(data.combined[1:891,], aes(x = pclass, fill = factor(survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 

#
# Analysis of name 
#
str(data.combined$name)
data.combined$name <- as.character(data.combined$name)
head(data.combined$name)

# Load stringr for searching
library(stringr)

str(data.combined$name)
name.splits <- str_split(data.combined$name, ",")
name.splits[1]
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
name.splits[[1]][2]
titles <- sapply(name.splits,"[",2)
titles[1:3]

data.combined$title <- titles
data.combined$title <- as.character(data.combined$title)

View(data.combined[!titles %in% c("Mr.","Mrs.","Miss.","Master."),c("age","name","sex")])

data.combined[titles %in% c("Don.","Rev.","Dr.","Major.","Sir.","Col.","Capt.","Jonkheer.") & 
                data.combined$sex=="male","title"] <- "Mr."

data.combined[titles %in% c("Mme.","Ms.","Lady.","Mlle.","the","Dr.") & 
                data.combined$sex=="female","title"] <- "Miss."

data.combined[titles =="Dona.","title"] <- "Mrs."

data.combined$title <- as.factor(data.combined$title)

ggplot(data.combined[1:891,],aes(x=title,fill=survived))+
  geom_bar()+
  labs(x="title",y="total count")+
  facet_grid(.~pclass)



#
# Analysis of sex
#

# what is the contriobution of male and female in train and test?
table(data.combined$sex)

# visualize relation between sex, survival and pclass
ggplot(data.combined[1:891,], aes(x = sex, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")


# Analysis of age

summary(data.combined$age)
summary(data.combined[1:891,"age"])

# Take a look at survival rates broken out by sex, pclass, and age
ggplot(data.combined[1:891,], aes(x = age, fill = survived)) +
  facet_wrap(~sex + pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")


# Validate that "Master." is a good proxy for male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$age)

# We know that "Miss." is more complicated, let's examine further
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$age)


# Analysis of sibsp
str(data.combined$sibsp)
summary(data.combined$sibsp)

# Treat the sibsp vaiable as a factor and visualize
data.combined$sibsp <- as.factor(data.combined$sibsp)
ggplot(data.combined[1:891,],aes(x=sibsp,fill=survived))+
  geom_bar()+
  labs(x="sibsp",y="count")+
  facet_grid(.~pclass)

# Analysis of parch
str(data.combined$parch)
summary(data.combined$parch)

# Treat the parch vaiable as a factor and visualize
data.combined$parch <- as.factor(data.combined$parch)
ggplot(data.combined[1:891,],aes(x=parch,fill=survived))+
  geom_bar()+
  labs(x="parch",y="count")+
  facet_grid(.~pclass)


# Let's try some feature engineering. What about creating a family size feature?
temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch <- c(train$parch, test$parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)


# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Analysis of ticket 
str(data.combined$ticket)
data.combined$ticket <- as.character(data.combined$ticket)

ticket.first.char <- as.factor(substr(data.combined$ticket,1,1))
ticket.first.char[1:4]

data.combined$ticket.first.char <- ticket.first.char
str(data.combined$ticket.first.char)

# First, a high-level plot of the data
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")


# Ticket seems like it might be predictive, drill down a bit
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(fill = "Survived")

# Lastly, see if we get a pattern when using combination of pclass & title
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")


# Analysis of fare
str(data.combined$fare)


# Can't make fare a factor, treat as numeric & visualize with histogram
ggplot(data.combined, aes(x = fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)


# Let's check to see if fare has predictive power
ggplot(data.combined[1:891,], aes(x = fare, fill = survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("fare") +
  ylab("Total Count") +
  ylim(0,50) + 
  labs(fill = "Survived")



# Load the library dplyr to combine the same ticket
library(dplyr)

data.combined.fare <- group_by(data.combined,ticket)
data.combined.fare <- summarise(data.combined.fare,count=n())
View(data.combined.fare)

extractCount <- function(ticket){
  data.combined.fare[ticket == data.combined.fare$ticket,"count"]
}

ticket.count <- sapply(data.combined$ticket,extractCount)
data.combined$ticket.count <- as.numeric(ticket.count)

str(data.combined)
data.combined$avg.fare <- data.combined$fare/data.combined$ticket.count


# Analysis of cabin variable
str(data.combined$cabin)
data.combined$cabin <- as.character(data.combined$cabin)
data.combined[data.combined$cabin=="","cabin"] <- "U"

# extract the first char
data.combined$cabin.first.char <- as.factor(substr(data.combined$cabin,1,1))
levels(data.combined$cabin.first.char)
str(cabin.first.char)
table(data.combined$cabin.first.char)


# High level plot
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")


# Plot the cabin first char to see predictive power
ggplot(data.combined[1:891,],aes(x=cabin.first.char,fill=survived))+
  geom_bar()+
  facet_grid(.~pclass)+
  labs(x="cabin.first.char",y="count")

# Does this feature improve upon pclass + title?
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# What about passenger with multiple cabins?
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")


# Does survivability depend on where you got onboard the Titanic?
str(data.combined$embarked)
levels(data.combined$embarked)
table(data.combined$embarked)

# Treatment of missing value using mode
data.combined$embarked <- as.character(data.combined$embarked)
data.combined[data.combined$embarked=="","embarked"] <- "S"
data.combined$embarked <- as.factor(data.combined$embarked)

# Plot data for analysis
ggplot(data.combined[1:891,], aes(x = embarked, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


######################################
# 
# Data Modeling 
# 
######################################



library(randomForest)

# Train a Random Forest with the default parameters using pclass & title
rf.train.1 <- data.combined[1:891, c("pclass", "title")]
rf.label <- as.factor(train$survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)


# Train a Random Forest using pclass, title, & sibsp
rf.train.2 <- data.combined[1:891, c("pclass", "title", "sibsp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)


# Train a Random Forest using pclass, title, & parch
rf.train.3 <- data.combined[1:891, c("pclass", "title", "parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)



# Train a Random Forest using pclass, title, sibsp, parch
rf.train.4 <- data.combined[1:891, c("pclass", "title", "sibsp", "parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)



# Train a Random Forest using pclass, title, & family.size
rf.train.5 <- data.combined[1:891, c("pclass", "title", "family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)



# Train a Random Forest using pclass, title, sibsp, & family.size
rf.train.6 <- data.combined[1:891, c("pclass", "title", "sibsp", "family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)



# Train a Random Forest using pclass, title, parch, & family.size
rf.train.7 <- data.combined[1:891, c("pclass", "title", "parch", "family.size")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)


# we get the lower error rate for model 5 i.e  rf.5
# we the accuracy of 83.16 for train dataset

# Subset our test records and features
test.submit.df <- data.combined[892:1309, c("pclass", "title", "family.size")]

# Make predictions
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)

write.csv(submit.df, file = "RF_SUB_20170710_1.csv", row.names = FALSE)


# Our submission scores 0.79426, but the OOB predicts that we should score 0.8159.
# Let's look into cross-validation using the caret package to see if we can get
# more accurate estimates
library(caret)
library(doSNOW)


# We use statified cross validation
set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv.10.folds)


# Make cluster 
cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)

# Set seed for reproducibility and train
set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.1)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.1


# The above is only slightly more pessimistic than the rf.5 OOB prediction, but 
# not pessimistic enough. Let's try 5-fold CV repeated 10 times.
set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                       index = cv.5.folds)

cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)

set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.2)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.2


# 5-fold CV isn't better. Move to 3-fold CV repeated 10 times. 
set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                       index = cv.3.folds)

cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 64, trControl = ctrl.3)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.3



# Let's use a single decision tree to better understand what's going on with our
# features. Obviously Random Forests are far more powerful than single trees,
# but single trees have the advantage of being easier to understand.

# Install and load packages
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# let's use 3-fold CV repeated 10 times
# Create utility function
rpart.cv <- function(seed, training, labels, ctrl) {
  cl <- makeCluster(3, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  # Leverage formula interface for training
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30, 
                    trControl = ctrl)
  
  #Shutdown cluster
  stopCluster(cl)
  
  return (rpart.cv)
}

# Grab features
features <- c("pclass", "title", "family.size")
rpart.train.1 <- data.combined[1:891, features]

# Run CV and check out results
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

# Plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


# The plot bring out some interesting lines of investigation. Namely:
#      1 - Titles of "Mr." and "Other" are predicted to perish at an 
#          overall accuracy rate of 83.9 %.
#      2 - Titles of "Master.", "Miss.", & "Mrs." in 1st & 2nd class
#          are predicted to survive at an overall accuracy rate of 95.0%.
#      3 - Titles of "Master.", "Miss.", & "Mrs." in 3rd class with 
#          family sizes equal to 5, 6, 8, & 11 are predicted to perish
#          with 100% accuracy.
#      4 - Titles of "Master.", "Miss.", & "Mrs." in 3rd class with 
#          family sizes not equal to 5, 6, 8, or 11 are predicted to 
#          survive with 60.1% accuracy.

# One missing value, take a look
data.combined[is.na(data.combined$avg.fare), ]

# Get records for similar passengers and summarize avg.fares
indexes <- with(data.combined, which(pclass == "3" & title == "Mr." & family.size == 1 &
                                       ticket != "3701"))
similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$avg.fare)

# Use median since close to mean and a little higher than mean
data.combined[is.na(data.combined$avg.fare), "avg.fare"] <- 7.840


cor(data.combined$ticket.count,data.combined$avg.fare)

# OK, let's see if our feature engineering has made any difference
features <- c("pclass", "title", "family.size", "ticket.count", "avg.fare")
rpart.train.3 <- data.combined[1:891, features]

# Run CV and check out results
rpart.3.cv.1 <- rpart.cv(94622, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

# Plot
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

#
# Rpart scores 0.80383
#
# Subset our test records and features
test.submit.df <- data.combined[892:1309, features]

# Make predictions
rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type = "class")
table(rpart.3.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rpart.3.preds)

write.csv(submit.df, file = "RPART_SUB_20170710_1.csv", row.names = FALSE)



#
# Random forest scores 0.80861
#
features <- c("pclass", "title", "ticket.count", "avg.fare")
rf.train.temp <- data.combined[1:891, features]

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp


test.submit.df <- data.combined[892:1309, features]

# Make predictions
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)

write.csv(submit.df, file = "RF_SUB_20170710_1.csv", row.names = FALSE)




################################  END ##################################################

























