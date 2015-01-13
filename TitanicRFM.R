# Open data frame containing the training data
titanic = read.csv(file="train.csv")
View(titanic)

library(reshape)
library(reshape2)
library(mosaic)



##########################################################################
##########################  CLEANING THE DATA ############################
##########################################################################


## Steps to create Age with Gender ###
#In order to fill in the ages, we're going to assume that the missing ages are randomly distributed
#In order to not skew the existing distribution we determined the percentage of people in each
#age group.
#0-10,  10-20,  20-30, 30-40,  40-50, 50-on
#.0896, .161,   .322 ,  .217,   .120,  .0896
#This section creates a dataset with the length equal to the number of unknown ages. Since ~8% of the
#passengers in the training data were between the age of 0-10, for 8% of the unknowns we filled in an 
#age of 5.  
newData <- data.frame(c(1:177))
names(newData) <- "age"
age_len = length(newData$age)
age_len
n_5 = round(.0896*age_len)
n_15 = round(.161*age_len)
n_25 = round(.322*age_len)
n_35 = round(.217*age_len)
n_45 = round(.120*age_len)
n_g50 = round(.0896*age_len)

for (i in c(1:n_5))
{newData$age[i]= 5}
beg = n_5+1
end = n_5 + n_15
for (k in c(beg:end))
{newData$age[k]= 15}
beg = end + 1
end = end + n_25

for (i in c(beg:end))
{newData$age[i]= 25}
beg = end + 1
end = end + n_35

for (i in c(beg:end))
{newData$age[i]= 35}
beg = end + 1
end = end + n_45

for (i in c(beg:end))
{newData$age[i]= 45}
beg = end + 1
end = end + n_g50

for (i in c(beg:end+1))
{newData$age[i]= 65}

for (k in c(1:length(newData$age)))
{for (i in c(1:length(titanic$Age))){
  if (is.na(titanic$Age[i])){
    titanic$Age[i] <- newData$age[k]
    break()}
}   
}
#First we create a set of pivot tables in order to determine which variables appear to correlate with survival 

# Make two tables: Male & Female
titanicMale <- titanic[titanic$Sex=="male",]
titanicFemale <- titanic[titanic$Sex=="female",]

#Makes pivot tables based on the age group divided by decade
titanic$age_group <- cut(titanic$Age, breaks = c(0, 10, 20, 30, 40, 50,60,70,80,100))
ages <- dcast(titanic, age_group ~ Survived)
ages$pSurvived <- ages$"1" / (ages$"1" + ages$"0")
View(ages)


# Make a pivot table for Age with Male
titanicMale$age_group <- cut(titanicMale$Age, breaks = c(0, 10, 20, 30, 40, 50,60,70,80,100))
ageDataMale <- dcast(titanicMale, age_group ~ Survived)
ageDataMale$pSurvived <- ageDataMale$"1" / (ageDataMale$"1" + ageDataMale$"0")

# Make a pivot table for Age with Female
titanicFemale$age_group <- cut(titanicFemale$Age, breaks = c(0, 10, 20, 30, 40, 50,60,70,80,100))
ageDataFemale <- dcast(titanicFemale, age_group ~ Survived)
ageDataFemale$pSurvived <- ageDataFemale$"1" / (ageDataFemale$"1" + ageDataFemale$"0")


### Steps to create pivot table of Survival vs. Fare Price for each gender ###

# Create pivot table of Survival rate vs. Fare (in bins of width $10) for Women
titanicMale$fare_price <- cut(titanicMale$Fare, breaks = c(-0.01, 10, 20, 30, 40, 1000))
fareMale <- dcast(titanicMale, fare_price ~ Survived)
fareMale$pSurvived <- fareMale$"1" / (fareMale$"1" + fareMale$"0")
View(fareMale)

# Create pivot table of Survival rate vs. Fare (in bins of width $10) for Men
titanicFemale$fare_price <- cut(titanicFemale$Fare, breaks = c(-0.01, 10, 20, 30, 40, 1000))
fareFemale <- dcast(titanicFemale, fare_price ~ Survived)
fareFemale$pSurvived <- fareFemale$"1" / (fareFemale$"1" + fareFemale$"0")
View(fareFemale)


#R code for cleaning the data and entering null values 
# Change Female to 1 and Male to 0 
titanic$Gender <- ifelse((titanic$Sex == "male"), 0, 1)

# Delete the unneeded columns
titanic$Name <- NULL
titanic$PassengerId <- NULL
titanic$Ticket <- NULL
titanic$Cabin <- NULL
titanic$Embarked <- NULL
titanic$Sex <- NULL
titanic$age_group <- NULL

### Entering missing fares (fares with 0) by plugging in the mean fare based on its Pclass
# Creating three new dataset based on different class
titanicFare <- titanic[titanic$Fare!=0,]  # dataset with no 0s in the Fare column
Class1 <- titanicFare[titanicFare$Pclass=="1",]
Class2 <- titanicFare[titanicFare$Pclass=="2",]
Class3 <- titanicFare[titanicFare$Pclass=="3",]

# load the mosaic package
require(mosaic)

# Figure out the mean fare for each class
Pclass1Mean = mean(titanic$Fare, data = Class1)
Pclass2Mean = mean(titanic$Fare, data = Class2)
Pclass3Mean = mean(titanic$Fare, data = Class3)

# Loop through the whole dataset and find the Fare with price 0, then enter in numbers based on the average fare of its class

for (i in c(1:length(titanic$Fare)))
{
  if (titanic$Fare[i] == 0 && titanic$Pclass[i] == "1")
    titanic$Fare[i] = Pclass1Mean
  if (titanic$Fare[i] == 0 && titanic$Pclass[i] == "2")
    titanic$Fare[i] = Pclass2Mean
  if (titanic$Fare[i] == 0 && titanic$Pclass[i] == "3")
    titanic$Fare[i] = Pclass3Mean  
}



##########################################################################
##########################  MODELING THE DATA ############################
##########################################################################

# Generating random forest, obtaining statistical summary, visualize cross-validation results
#using rpart
library(rpart)

#fitting a regression model for the random forests based on our model parameters

fit <- rpart(Survived ~ Age + Fare + Gender + SibSp + Parch + Pclass, method="anova", data=titanic)

printcp(fit) # display the results of the complexity parameters
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots of R-squared values
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results    

# plot tree
# ovals are interior nodes and boxes are leaves
quartz("Quartz", width = 9, height = 6.5, pointsize=8.5)
plot(fit, uniform=TRUE, main = "Titanic Survival")
text(fit, use.n=TRUE, all=TRUE, cex = .9, fancy = TRUE, bg = "#4FC1FF", fwidth=.8, fheight=.9)

##########################################################################
#########################  TESTING THE MODEL #############################
##########################################################################
test = read.csv(file="test.csv")

#Fills in the missing Ages for the test data 

newData <- data.frame(c(1:87))
names(newData) <- "age"
age_len = length(newData$age)
age_len
n_5 = round(.0896*age_len)
n_15 = round(.161*age_len)
n_25 = round(.322*age_len)
n_35 = round(.217*age_len)
n_45 = round(.120*age_len)
n_g50 = round(.0896*age_len)

for (i in c(1:n_5))
{newData$age[i]= 5}
beg = n_5+1





end = n_5 + n_15
for (k in c(beg:end))




{newData$age[k]= 15}
beg = end + 1
end = end + n_25

for (i in c(beg:end))
{newData$age[i]= 25}
beg = end + 1
end = end + n_35

for (i in c(beg:end))
{newData$age[i]= 35}
beg = end + 1
end = end + n_45

for (i in c(beg:end))
{newData$age[i]= 45}
beg = end + 1
end = end + n_g50

for (i in c(beg:end+1))
{newData$age[i]= 65}

for (k in c(1:length(newData$age)))
{for (i in c(1:length(test$Age))){
  if (is.na(test$Age[i])){
    test$Age[i] <- newData$age[k]
    break()}
}   
}
test$Gender <- ifelse((test$Sex == "male"), 0, 1)
test$Name <- NULL
test$PassengerId <- NULL
test$Ticket <- NULL
test$Cabin <- NULL
test$Embarked <- NULL
test$Sex <- NULL

test$Survived <- NA
predicted <- data.frame(predict(fit, test))
predicted <- ifelse(predicted < 0.5, predicted <- 0, predicted <- 1)
test$Survived <- predicted

titanictest = write.csv(test, file = "titanictest.csv")

