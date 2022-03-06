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