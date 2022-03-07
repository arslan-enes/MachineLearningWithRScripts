bike <- read.csv('Projects/Bike Sharing Demand Kaggle Challenge/bikeshare.csv')

head(bike)

library(ggplot2)

pl <- ggplot(bike,aes(x=temp,y=count)) + geom_point(aes(color=temp),alpha=0.5)
print(pl)

bike$datetime <- as.POSIXct(bike$datetime)

pl2 <- ggplot(bike,aes(x=datetime,y=count)) + geom_point(aes(color=temp),alpha=0.5) + scale_color_gradient(low = 'blue',high = 'red') 
print(pl2)

library(dplyr)

cor.data <- cor(select(bike,temp,count))
print(cor.data)

pl3 <- ggplot(bike,aes(x=factor(season),y=count)) + geom_boxplot(aes(color=factor(season)))
print(pl3)

hours <- format(as.POSIXct(bike$datetime),"%H")
hours <- as.numeric(hours)
bike <- cbind(bike,hours)

pl4 <- ggplot(filter(bike,workingday==1),aes(x=hours,y=count)) + 
              geom_point(aes(color=temp),position = position_jitter(w=1,h=0)) + 
              scale_color_gradientn(colors=c('blue','green','yellow','red'))

print(pl4)

pl5 <- ggplot(filter(bike,workingday==0),aes(x=hours,y=count)) + 
  geom_point(aes(color=temp),position = position_jitter(w=1,h=0)) + 
  scale_color_gradientn(colors=c('blue','green','yellow','red'))

print(pl5)


temp.model <- lm(count~temp,bike)
summary(temp.model)

count.predictions <- predict(temp.model,bike)

results <- cbind(bike$temp,count.predictions)
colnames(results) <- c("Temperature","Prediction")
results <- as.data.frame(results)
print(head(results))

temp.test <- data.frame(temp=c(25))
predict(temp.model,temp.test)

model <- lm(count~.-datetime -casual -registered - atemp,bike)

count.predictions.model <- predict(model,bike)

results <- cbind(count.predictions.model,count.predictions,bike$count)
colnames(results) <- c("Final Model Predictions","Temp Model Predictions","Actual Results")
results <- as.data.frame(results)
print(head(results,20))
