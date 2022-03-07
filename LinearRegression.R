df <- read.csv("student-mat.csv",sep = ';') ## Reading the data from .csv file with ';' separator

## Checking the data

head(df)
any(is.na(df))

## Exploring the data

library(ggplot2)
library(ggthemes)
library(dplyr)

num.cols <- sapply(df, is.numeric)

cor.data <- cor(df[,num.cols]) # Grabbing correlations


library(corrgram)
library(corrplot)

print(corrplot(cor.data,method = 'color')) ## Creating Correlation Plot with 'corrplot'

print(corrgram(df,order = TRUE,lower.panel = panel.shade,upper.panel = panel.pie,text.panel = panel.txt))

## Getting ideas about 'G3' column

g3_histogram <- ggplot(df,aes(x=G3)) + geom_histogram(bins = 20,alpha=0.5,fill='blue')
print(g3_histogram)


## Splitting the data into train and test sets

library(caTools)

set.seed(101)

sample <- sample.split(df$G3,SplitRatio = 0.7)  # 70% for training - 30% for testing

train <- subset(df,sample == TRUE)
test <- subset(df,sample == FALSE)


#model <- lm(y ~ x1+x2,data)


model <- lm(G3~.,train)

res <- residuals(model)   ## Residuals are the difference between the actual data point and prediction
res <- as.data.frame(res)

res_histogram <- ggplot(res,aes(res)) + geom_histogram(fill='blue',alpha=0.5,bins = 30)
print(res_histogram)

plot(model)

## Testing

G3.predictions <- predict(model,test)

results <- cbind(G3.predictions,test$G3)
colnames(results) <- c('predicted','actual')
results <- as.data.frame(results)
print(head(results))

to_zero <- function(x){  ##Getting rid of negative predictions
  if(x<0){
    return(0)
  }else{
    return(x)
  }
}

results$predicted <- sapply(results$predicted,to_zero)

# And the linear regression metric equations

print('SSE')
SSE <- sum((results$predicted - results$actual)^2)
print(SSE)

print('SST')
SST <- sum((mean(df$G3) - results$actual)^2)
print(SST)

print('R2')
R2 <- 1-SSE/SST
print(R2)

