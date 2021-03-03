#LINEAR REGRESSION

View(mtcars)
# mpg is my target variable
m1 <- lm(mpg ~ ., data = mtcars)
m1

#read summary of built model
summary(m1)

#optimize the model
#we dropped least significant variables

m2 <- lm(mpg ~ wt+am+qsec+hp+disp+drat, data = mtcars)
summary(m2)


m3 <- lm(mpg ~ wt+am+qsec+hp+disp, data = mtcars)
summary(m3)


m4 <- lm(mpg ~ wt+am+qsec+hp, data = mtcars)
summary(m4)


m5 <- lm(mpg ~ wt+am+qsec, data = mtcars)
summary(m5)

#best model

mf <- lm(mpg ~ wt+am+qsec+hp+disp, data = mtcars)
summary(m3)

prd1 <- predict(mf,mtcars)
prd1

act_pred <- data.frame(mtcars$mpg ,prd1)
act_pred


# y variable is not required for prediction

mtcars1 <- mtcars[,c(2:11)]
View(mtcars1)

prd1 <- predict(mf,mtcars1)
prd1

act_pred <- data.frame(mtcars$mpg ,prd1)
act_pred

#viewing errors
error <- mtcars$mpg - prd1
error

MSE <- mean((error)^2)
MSE

RMSE <- MSE ^.5
RMSE

MAPE <- mean(abs(error*100/ mtcars$mpg))
MAPE

Acc = 100-MAPE
Acc

#practice
View(state.x77)

#y variable name had inbetween space
colnames(state.x77)[4] = "lifeexp"
View(state.x77)

#dataframe was in mattrix
state<- data.frame(state.x77)

#build model
st1 <- lm(lifeexp ~ ., data = state)
summary(st)
st2 <- lm(lifeexp ~ Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area, data = state)
summary(st2)
st3 <- lm(lifeexp ~ Population+Income+Illiteracy+Murder+HS.Grad+Frost, data = state)
summary(st3)
st4 <- lm(lifeexp ~ Population+Murder+HS.Grad+Frost, data = state)
summary(st4)

prd <- predict(st4,state)
prd

act_pred <- data.frame(state$lifeexp ,prd)
act_pred

error <- state$lifeexp - prd
error

MSE <- mean((error)^2)
MSE

RMSE <- MSE ^.5
RMSE

MAPE <- mean(abs(error*100/ state$lifeexp))
MAPE

Acc = 100-MAPE
Acc

#practice on external data
#lungcap data (target variable lung cap)
#convert data into numeric (standardize data) 
#sampling
#build model with sampling(train data)
#test data(MSE,RMSE,MAP,MAPE)



lc <- read.csv('C:/Users/danga/Documents/import/LungCapData.csv')
View(lc)

lc$Smoke <- (as.numeric(as.factor(lc$Smoke)))
lc$Gender <- (as.numeric(as.factor(lc$Gender)))
lc$Caesarean <- (as.numeric(as.factor(lc$Caesarean)))

View(lc)

lcsam = sample(2 , nrow(lc) ,replace = TRUE , prob = c(.8, .2))
lcsam
length(lcsam)

lctr <- lc[lcsam ==1 , ]
lcte <- lc[lcsam ==2 , ]

m1 <- lm(LungCap ~ .,data = lctr)
summary(m1)

m2 <- lm(LungCap ~ Age+Height+Smoke+Gender,data = lctr)
summary(m2)

#prediction of train data
pr1 <- predict(m1,lctr)
pr1

ac_pr1 <- data.frame(lctr ,pr1)
ac_pr1

errortr <- lctr$LungCap - pr1
errortr
#mean of your error term shuld be close to zero
mean(errortr)
hist(errortr)
plot(errortr)

MSE <- mean((errortr)^2)
MSE

RMSE <- MSE ^.5
RMSE

MAPE <- mean(abs(errortr*100/ lctr$LungCap))
MAPE

Acc = 100-MAPE
Acc

#prediction of test data
pr2 <- predict(m1,lcte)
pr2

ac_pr2 <- data.frame(lcte ,pr2)
ac_pr2

errorte <- lcte$LungCap - pr2
errorte
#mean of your error term shuld be close to zero
mean(errorte)
hist(errorte)
plot(errorte)

MSE <- mean((errorte)^2)
MSE

RMSE <- MSE ^.5
RMSE

MAPE <- mean(abs(errorte*100/ lcte$LungCap))
MAPE

Acc = 100-MAPE
Acc





################################################################################

#---------------------------------------------------------------------------#


il <- read.csv('C:/Users/danga/Documents/import/insuranceLiner.csv')
View(il)
dim(il)
colSums(is.na(il))

il$sex <- as.numeric(as.factor(il$sex))
il$smoker <- as.numeric(as.factor(il$smoker))
il$region <- as.numeric(as.factor(il$region))

il = il[, c(1,3,4,5,6,7)] 
View(il)



sample_il = sample(2 , nrow(il) ,replace = TRUE , prob = c(.8, .2))
il_train <- il[sample_il ==1, ]
il_test  <- il[sample_il ==2, ]

View(il_train)
View(il_test)
dim(il_test)
dim(il_train)

#model

model_li <- lm(charges ~ ., data = il_train)
summary(model_li)



pred_li_train <- predict(model_li ,il_train)

error_li_train <- il_train$charges - pred_li_train
error_li_train

mean(error_li_train)
mean

hist(error_li_train)
plot(error_li_train)
boxplot(error_li_train)
plot(il_train$charges , pred_li_train)

#befor remove outliers find MAPE


pred_li_test <- predict(model_li ,il_test)

error_li_test <- il_test$charges - pred_li_test
error_li_test

MAPE_li_test <- mean(abs(error_li_test*100/ il_test$charges))
MAPE_li_test

# finding outliers in original data

abc <- boxplot(il$charges)
abc

il <- il[il$charges <= 34472.841, ]

dim(il)


sample_il = sample(2 , nrow(il) ,replace = TRUE , prob = c(.8, .2))
il_train <- il[sample_il ==1, ]
il_test  <- il[sample_il ==2, ]



model_li <- lm(charges ~ ., data = il_train)
summary(model_li)



pred_li_train <- predict(model_li ,il_train)

error_li_train <- il_train$charges - pred_li_train
error_li_train

mean(error_li_train)
mean




pred_li_test <- predict(model_li ,il_test)

error_li_test <- il_test$charges - pred_li_test
error_li_test

MAPE_li_test <- mean(abs(error_li_test*100/ il_test$charges))
MAPE_li_test


hist(error_li_test)
plot(error_li_test)
boxplot(error_li_test)
plot(il_test$charges , pred_li_test)


#

abc <- boxplot(il$charges)
abc

il <- il[il$charges <= 25678.7781, ]

il <- il[il$charges <= 24180.933, ]
abc <- boxplot(il$charges)
abc


il <- il[il$charges <= 23568.272, ]
abc <- boxplot(il$charges)
abc



il <- il[il$charges <= 23401.306, ]
abc <- boxplot(il$charges)
abc


sample_il = sample(2 , nrow(il) ,replace = TRUE , prob = c(.8, .2))
il_train <- il[sample_il ==1, ]
il_test  <- il[sample_il ==2, ]



model_li <- lm(charges ~ ., data = il_train)
summary(model_li)



pred_li_train <- predict(model_li ,il_train)

error_li_train <- il_train$charges - pred_li_train
error_li_train

mean(error_li_train)
mean




pred_li_test <- predict(model_li ,il_test)

error_li_test <- il_test$charges - pred_li_test
error_li_test

MAPE_li_test <- mean(abs(error_li_test*100/ il_test$charges))
MAPE_li_test









#property price train
#sales price is target variable
#try building model working good on assumption

#hign adjusted Rsquares


#to identify nulls
#colsums(pa)[colsums(is.na(pa))*100/1459 > 40]
#


#ppt <- read.csv('C:/Users/danga/Documents/import/Property_Price_Train.csv')
colSums(is.na(ppt))
colSums(ppt)[colsums(is.na(ppt))*100/1459 > 40]

ppt <- read.csv('C:/Users/danga/Documents/import/Property_Price_Train.csv',na.strings = "")

colSums(is.na(ppt))

ppt = ppt[    , c(2:6,8:72,76:81)] 

colSums(is.na(ppt))


ppt$Building_Class = as.numeric(as.factor(ppt$Building_Class))
ppt$Zoning_Class   = as.numeric(as.factor(ppt$Zoning_Class))
ppt$Road_Type      = as.numeric(as.factor(ppt$Road_Type))
ppt$Property_Shape = as.numeric(as.factor(ppt$Property_Shape))
ppt$Land_Outline   = as.numeric(as.factor(ppt$Land_Outline))
ppt$Utility_Type   = as.numeric(as.factor(ppt$Utility_Type))
ppt$Lot_Extent     = as.numeric(as.factor(ppt$Lot_Extent))
ppt$Lot_Configuration = as.numeric(as.factor(ppt$Lot_Configuration))
ppt$Property_Slope    = as.numeric(as.factor(ppt$Property_Slope))
ppt$Neighborhood      = as.numeric(as.factor(ppt$Neighborhood))
ppt$Condition1        = as.numeric(as.factor(ppt$Condition1))
ppt$Condition2        = as.numeric(as.factor(ppt$Condition2))
ppt$House_Type        = as.numeric(as.factor(ppt$House_Type))
ppt$House_Design      = as.numeric(as.factor(ppt$House_Design))
ppt$Roof_Design       = as.numeric(as.factor(ppt$Roof_Design))
ppt$Roof_Quality      = as.numeric(as.factor(ppt$Roof_Quality))
ppt$Exterior1st       = as.numeric(as.factor(ppt$Exterior1st))
ppt$Exterior2nd       = as.numeric(as.factor(ppt$Exterior2nd))
ppt$Brick_Veneer_Type = as.numeric(as.factor(ppt$Brick_Veneer_Type))
ppt$Exterior_Material = as.numeric(as.factor(ppt$Exterior_Material))
ppt$Exterior_Condition = as.numeric(as.factor(ppt$Exterior_Condition))
ppt$Foundation_Type    = as.numeric(as.factor(ppt$Foundation_Type))
ppt$Basement_Height    = as.numeric(as.factor(ppt$Basement_Condition))
ppt$Basement_Condition = as.numeric(as.factor(ppt$Basement_Condition))
ppt$Exposure_Level     = as.numeric(as.factor(ppt$Exposure_Level))
ppt$BsmtFinType1       = as.numeric(as.factor(ppt$BsmtFinType1))
ppt$BsmtFinType2       = as.numeric(as.factor(ppt$BsmtFinType2))
ppt$Heating_Type       = as.numeric(as.factor(ppt$Heating_Type))
ppt$Heating_Quality    = as.numeric(as.factor(ppt$Heating_Quality))
ppt$Air_Conditioning   = as.numeric(as.factor(ppt$Air_Conditioning))
ppt$Electrical_System  = as.numeric(as.factor(ppt$Electrical_System))


