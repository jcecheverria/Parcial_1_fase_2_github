
# Modelo sencillo para implementar en el Dashboard

#### LIBRERIAS ####
library(dplyr)
library(randomForest)
library(caTools)

options(scipen=999)

#### DATA ####
houses <- read.csv("data/kc_house_data.csv")
str(houses)


#### GRAPHS ####
barplot(table(houses$yr_built))
plot(houses$yr_built,houses$price)
plot(houses$bedrooms,houses$price)
plot(houses$condition,houses$price)
plot(houses$grade,houses$price)
plot(houses$bathrooms,houses$price)


#### DATA PREP ####
set.seed(123)

houses$grade <- as.factor(houses$grade)
houses$bedrooms <- as.factor(houses$bedrooms)

split = sample.split(houses,SplitRatio = 0.8)
train = subset(houses, split == T) 
test = subset(houses, split == F)
 
#### MODEL TRAINING ####
rf_model = randomForest(price ~ bedrooms + bathrooms + sqft_living + yr_built + sqft_lot + grade,train,ntrees=1000)
print(rf_model)

### MODEL EVALUATION ####
rmse <- function(actual,predicted){
  se <- (actual - predicted)**2
  rmse <- sqrt(mean(se))
  return(rmse)
}

rmse(test$price,predict(rf_model,test))
importance(rf_model)

# Individual Test
rf_model <- readRDS("rf_model.rds")
bedroom_lvls <- as.vector(levels(houses$bedrooms))
grade_lvls <- as.vector(levels(houses$grade))

df_test <- data.frame(bedrooms= factor(3,levels=bedroom_lvls),
                      bathrooms=as.numeric(2.25),
                      sqft_living=as.integer(2570),
                      yr_built=as.integer(1951),
                      sqft_lot=as.integer(7242),
                      grade=factor(7,levels=grade_lvls))
predict(rf_model,df_test)


#### SAVING MODEL ####
saveRDS(rf_model, "rf_model.rds")
