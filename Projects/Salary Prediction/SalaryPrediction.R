adult <- read.csv("Projects/Salary Prediction/adult_sal.csv",stringsAsFactors = T) #Reading the data frame from csv file
head(adult)

library(dplyr)
adult <- select(adult,-X) ## Dropping the X column that is unnecessary

head(adult)
str(adult)
summary(adult)

employer_type_table <- table(adult$type_employer)

adult$type_employer <- as.character(adult$type_employer)
## Combining the suitable employer types together

employer_combiner <- function(employer_type){
  for (i in 1:length(employer_type)) {
    if (employer_type[i]=="Never-worked" || employer_type[i]=="Without-pay" ) {
      employer_type[i] = "Unemployed"
    }
    if (employer_type[i]=="Self-emp-inc" || employer_type[i]=="Self-emp-not-inc" ) {
      employer_type[i] = "self-emp"
    }
    if (employer_type[i]=="Local-gov" || employer_type[i]=="State-gov" ) {
      employer_type[i] = "SL-gov"
    }
  }
  return(employer_type)
}

adult$type_employer <- employer_combiner(adult$type_employer)
table(adult$type_employer)

## Looking at marital column and combining suitable datas

table(adult$marital)

marital_combiner <- function(marital){
  for (i in 1:length(marital)) {
    if (marital[i]=="Divorced" || marital[i]=="Separated" || marital[i]=="Widowed") {
      marital[i] = "Not-Married"
    }
    if (marital[i]=="Married-AF-spouse" || marital[i]=="Married-civ-spouse" || marital[i]=="Married-spouse-absent") {
      marital[i] = "Married"
    }
  }
  return(marital)
}

adult$marital <- marital_combiner(adult$marital)
table(adult$marital)

## Looking at country column and combining similar datas

table(adult$country)
countries <- adult$country
countries <- distinct(as.data.frame(countries))

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

adult$country <- sapply(adult$country,group_country)

table(adult$country)

str(adult)

adult$type_employer <- as.factor(adult$type_employer)
adult$marital <- as.factor(adult$marital)
adult$country <- as.factor(adult$country)

str(adult)

library(Amelia)

missmap(adult,main = "Missing Map",col = c("yellow","black"),legend = FALSE)

adult[adult=="?"] <- NA
table(adult$occupation)

adult <- na.omit(adult)


## Exploring and visualizing the data

library(ggplot2)

pl_age_salary <- ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color="black",binwidth = 1)

print(pl_age_salary)

pl_hrperweek <- ggplot(adult,aes(hr_per_week)) + geom_histogram(bins = 20,binwidth = 3)

print(pl_hrperweek)

## Renaming the country column
colnames(adult)[14] <- "region"

pl_region_salary <- ggplot(adult,aes(region)) + geom_bar(aes(fill=income),position = "dodge")
print(pl_region_salary)

head(adult)

library(caTools)

## Splitting into train and test datasets

set.seed(101)

sample <- sample.split(adult$income,SplitRatio = 0.7)

train <- subset(adult,sample==TRUE)
test <- subset(adult,sample==FALSE)

str(adult)

## Building the model

log.model <- glm(income~.,family = binomial(logit),data = train)
summary(log.model)

new.model <- step(log.model)
summary(new.model)

## Predictions...

test$predicted.income.log = predict(log.model, newdata = test, type = "response")
head(test)
table(test$income, test$predicted.income.log >0.5)

## Calculating the accuracy

accuracy_log <- (6369+1420) / (6369+551+875+1420) 
accuracy_log
