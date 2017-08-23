# Big Mart Sales III
# Objective: To predict the Item_Outlet_Sales for different Outlets
# Regression Problem


# Load the library
library(ggplot2)
library(dplyr)
library(plyr)

# Load the dataset
train <- read.csv("train.csv",header = TRUE,na.strings = c(NA,"NA","?"," ",""))
test <- read.csv("test.csv",header = TRUE,na.strings = c(NA,"NA","?"," ",""))


# Explore the datasets
str(train)
str(test)
dim(train)
dim(test)

# Combine the train and test datasets
test$Item_Outlet_Sales <- NA
combi <- rbind(train,test)

# Features in the train dataset
names(train)


# Check for missing value 
# Item_Weight and Outlet_Size has the missing value 
colSums(is.na(combi))


#######################################################################################
#
#                           Data Analysis
#
#######################################################################################

###########################
# Item_Identifier Analysis
###########################

str(combi$Item_Identifier)
length(unique(combi$Item_Identifier))

# Item_Identifier is less in number when compared to number of observation 
# Item_Identifier can be useful to derive some useful inferences 
# Item_Identifier has 1559 value and there are 14204 observation


##########################
# Item_Weight Analysis
###########################

# Each item has specific weight,use this inference to treat missing value in Item_Weight column
# Item_weight has same weight for same item_identifier 

combiNoMiss <- combi[!is.na(combi$Item_Weight),c(1,2)] 
combiNoMiss <- unique(combiNoMiss)
#View(combiNoMiss)


combi$Item_Weight <- ifelse(is.na(combi$Item_Weight),
                            combiNoMiss$Item_Weight[match(combi$Item_Identifier,combiNoMiss$Item_Identifier)],
                            combi$Item_Weight)

colSums(is.na(combi))
summary(combi$Item_Weight)


ggplot(combi[1:nrow(train),],aes(Item_Weight)) + geom_density(col="blue") + 
  labs(x ="Item Weight", y ="density", title = "density plot for Item weight")



####################################
# Item_Fat_Content Analysis 
####################################

levels(combi$Item_Fat_Content)

# LF, low fat and Low Fat has same meaning, hence change the encooding 
# simliarly reg and regular has same meaning,hence change the encoding 

combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,
                                  c("LF"="Low Fat","low fat"="Low Fat","reg"="Regular"))

levels(combi$Item_Fat_Content)

ggplot(combi[1:nrow(train),],aes(Item_Fat_Content)) + geom_bar() + 
  labs( x= "Item fat content ", y =" frequncy",title=" Plot for Item_Fat_Content")




###############################
# Item_Visibility Analysis
###############################

summary(combi$Item_Visibility)

# Some Item Visibility has zero value.
# But if there is some product in outlet to sales it has some visibility.
# Replace the zero visibility by the median of its corresponding item_id

ggplot(combi[1:nrow(train),],aes(Item_Visibility)) +geom_density(col="red")

# Visibility Per Outlet
ItemVisByOutlet <-   combi %>% 
       select(Outlet_Identifier,Item_Visibility) %>% 
       group_by(Outlet_Identifier) %>% 
       summarise(TotalVis = sum(Item_Visibility))
#View(ItemVisByOutlet)


combi[combi$Item_Visibility == 0 , "Item_Visibility"] <- NA

combiNoMiss.itemVis <- combi[!is.na(combi$Item_Visibility),c("Item_Visibility","Item_Identifier")]

combiNoMiss.itemVis <- combiNoMiss.itemVis %>% group_by(Item_Identifier) %>% 
                            summarise(VisMedian = median(Item_Visibility))


#View(combiNoMiss.itemVis)

combi$Item_Visibility <- ifelse(is.na(combi$Item_Visibility),
                                      combiNoMiss.itemVis$VisMedian[match(combi$Item_Identifier,
                                      combiNoMiss.itemVis$Item_Identifier)],combi$Item_Visibility)


# Check the item visibility distribution across all outlets
ItemVisByOutlet <-   combi %>% 
  select(Outlet_Identifier,Item_Visibility) %>% 
  group_by(Outlet_Identifier) %>% 
  summarise(TotalVis = sum(Item_Visibility))
#View(ItemVisByOutlet)

# All outlet must have 100% visibility 
for (outName in levels(combi$Outlet_Identifier)) {
  combi[ which(combi$Outlet_Identifier == outName),]$Item_Visibility <-
    combi[ which(combi$Outlet_Identifier == outName),]$Item_Visibility *
    100/ItemVisByOutlet[ which(ItemVisByOutlet$Outlet_Identifier == outName),]$TotalVis
}

# View(combi %>% 
#        select(Outlet_Identifier,Item_Visibility) %>% 
#        group_by(Outlet_Identifier) %>% 
#        summarise(TotalVis = sum(Item_Visibility)))




#######################
# Item_Type Analysis
#######################
str(combi$Item_Type)
levels(combi$Item_Type)


# Extract first two char and three char from item_identier

combi$first.two.char <- factor(substr(combi$Item_Identifier,start = 1,stop = 2))
combi$first.three.char <- factor(substr(combi$Item_Identifier,start = 1,stop = 3))

str(combi$first.two.char)
str(combi$first.three.char)


#######################
# Item_MRP Analysis
#######################

summary(combi$Item_MRP)

ggplot(combi[1:nrow(train),],aes(Item_MRP)) + geom_histogram()


########################################
# Outlet_Establishment_Year Analaysis 
#######################################

#It will Not play any important role 
#But if we subtract the number of year from 2013  
#may be it will give some useful data

combi$year <- 2013 - combi$Outlet_Establishment_Year

combi$year <- factor(combi$year)
ggplot(combi[1:nrow(train),],aes(year)) + geom_bar()


combi$Outlet_Establishment_Year <-  NULL


#######################
# Outlet_Size Analysis
#######################

str(combi$Outlet_Size)

# Outlet_Size depends on Outlet_type, Outlet_Location_Type.

combi.outlet <- combi %>% select(Outlet_Size,Outlet_Type,Outlet_Location_Type,Outlet_Identifier) %>% 
  group_by(Outlet_Location_Type,Outlet_Type,Outlet_Size) %>% 
  summarise(count=n())

#View(combi.outlet)

# As we can seen froom above inference all missing place sholud be fill with small

combi[is.na(combi$Outlet_Size),"Outlet_Size"] <- "Small"

levels(combi$Outlet_Size)


#################
# Outlet_Type
#################

levels(combi$Outlet_Type)

ggplot(combi[1:nrow(train),],aes(Outlet_Type)) + geom_bar() +
  labs(x = "Outlet type " , y= "count", title =" Outlet type")



ggplot(combi[1:nrow(train),],aes(Outlet_Location_Type)) + geom_bar() +
  labs(x = "Outlet_Location_Type" , y= "count", title =" Outlet_Location_Type")


ggplot(combi[1:nrow(train),],aes(Outlet_Type)) + geom_bar(aes(fill=Outlet_Location_Type)) +
  labs(x = "Outlet type " , y= "count", title =" Outlet type")

ggplot(combi[1:nrow(train),],aes(Outlet_Type)) + geom_bar(aes(fill=Outlet_Size)) +
  labs(x = "Outlet type " , y= "count", title =" Outlet type")

# From the above analysis It is clear that 
# Spermarket 2 and Supermarket 3 has Tier 3 location  
# Both are medium in size 


combi$Outlet_Type <- revalue(combi$Outlet_Type,c("Supermarket Type2"="Other Supermarket",
                                                 "Supermarket Type3"="Other Supermarket"))

levels(combi$Outlet_Type)



########################
# Outlet_Size Analysis
########################

ggplot(combi[1:nrow(train),],aes(Outlet_Size)) + geom_bar() + 
  labs(x =" Outlet_Size",y="count",title = "Plot for Outlet Size")



#######################
# Outlet_Identifier
######################

levels(combi$Outlet_Identifier)


ggplot(combi[1:nrow(train),],aes(x= Outlet_Identifier,y = Item_Outlet_Sales)) + 
  geom_boxplot() + labs( x= "Outlets",y="Sales")

# Outlet10 and Outlet19 has least sales and Outlet27 has the maximum sales

# 
# Sales Analysis for Outlet19 and Outlet10 

#View(combi %>%  filter(Outlet_Identifier %in% c("OUT027")))


ggplot(combi[1:nrow(train),],aes(x= Outlet_Identifier)) + 
  geom_bar(aes(fill = Outlet_Size)) + 
  labs( x= " OOutlet_identifier",y ="Outlet_Size")

ggplot(combi[1:nrow(train),],aes(x= Outlet_Identifier)) + 
  geom_bar(aes(fill = Outlet_Location_Type))+
  labs( x= " OOutlet_identifier",y ="Outlet_Location_Type")

ggplot(combi[1:nrow(train),]) + 
  geom_bar(aes(x= Outlet_Identifier,fill = Outlet_Size),position = "dodge") +
  geom_bar(aes(x= Outlet_Identifier,fill= Outlet_Location_Type),position = "dodge")



#######################################
#
#  create new test and train file 
#
######################################

new_train <- combi[1:nrow(train),]
new_test <- combi[nrow(train)+1:nrow(test),]


write.csv(new_train,file = "new_train.csv",row.names = FALSE)
write.csv(new_test,file = "new_test.csv",row.names = FALSE)










