setwd('C:/Users/Shivanika/Desktop/MDA/Syllabus/Statistical Modelling I/Project/Final')

install.packages("corrr")
install.packages("aplpack")
install.packages("data.table")
install.packages("corrplot")
install.packages("tree")
install.packages("vip")
install.packages("car")

library(ggcorrplot)
library(corrr)
#library(aplpack)
library(data.table)
library(lmtest)
library(MASS)
library(tree)
library(vip)
library(car)

# Load data
bike_data = read.csv("SeoulBikeData.csv",
                     header = TRUE,
                     sep = ",",
                     quote = "\"",
                     comment.char = "",
                     stringsAsFactors = TRUE,
                     check.names = F)

# Change column names
colnames(bike_data)[which(names(bike_data) == "Rented Bike Count")] <- "RentedBikeCount"
colnames(bike_data)[which(names(bike_data) == "Temperature(\xb0C)")] <- "Temperature"
colnames(bike_data)[which(names(bike_data) == "Humidity(%)")] <- "Humidity"
colnames(bike_data)[which(names(bike_data) == "Wind speed (m/s)")] <- "WindSpeed"
colnames(bike_data)[which(names(bike_data) == "Visibility (10m)")] <- "Visibility"
colnames(bike_data)[which(names(bike_data) == "Dew point temperature(\xb0C)")] <- "DewPointTemp"
colnames(bike_data)[which(names(bike_data) == "Solar Radiation (MJ/m2)")] <- "SolarRad"
colnames(bike_data)[which(names(bike_data) == "Rainfall(mm)")] <- "Rainfall"
colnames(bike_data)[which(names(bike_data) == "Snowfall (cm)")] <- "Snowfall"
colnames(bike_data)[which(names(bike_data) == "Functioning Day")] <- "FunctioningDay"


View(bike_data)

# Check for Null Values
sum(is.na(bike_data))

# Split data into train and test
set.seed(20)
n = nrow(bike_data)
idx_tr <- sample(n,round(0.8*n),replace=FALSE)
# Removing the date column from test and train data
train <- bike_data[idx_tr,-1]
test <- bike_data[-idx_tr,-1]

train_copy = copy(train)

# Correlation Matrix of y with numeric predictors
bike.subset <- subset(bike_data, select = c(RentedBikeCount, Hour, Temperature, Humidity, WindSpeed, 
                                            Visibility, DewPointTemp, SolarRad,Snowfall,Rainfall))

pairs(~., data = bike.subset, main = "Scatterplot Matrix")

# Correlation Heatmap
library(corrplot)
library(RColorBrewer)
CR <-cor(train[,c(-11,-12,-13)])
corrplot(CR, type="lowe", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"), main = "Correlation Heatmap")
View(CR)

# Normalise data to create boxplots of variables
train_copy$Hour<-scale(train_copy$Hour)
train_copy$Temperature<-scale(train_copy$Temperature)
train_copy$Humidity<-scale(train_copy$Humidity)
train_copy$WindSpeed<-scale(train_copy$WindSpeed)
train_copy$Visibility<-scale(train_copy$Visibility)
train_copy$DewPointTemp<-scale(train_copy$DewPointTemp)
train_copy$SolarRad<-scale(train_copy$SolarRad)
train_copy$Rainfall<-scale(train_copy$Rainfall)
train_copy$Snowfall<-scale(train_copy$Snowfall)

norm_num = subset(train_copy, select = c(Hour, Temperature, Humidity, WindSpeed, 
                                              Visibility, DewPointTemp, SolarRad,Snowfall, Rainfall))

# Boxplot
boxplot(norm_num, xlab="Normalised Variables", main = "Boxplot of Numeric Predictors")

# Create a full model
full_model = lm(RentedBikeCount ~ ., data = train)
summary(full_model)

# Check for influential points
count_influential = sum(cooks.distance(full_model) > 4 / nrow(train))
count_influential

# Outliers in Train data
out_ind <- which(abs(rstandard(full_model))>2)
count_out <- sum(abs(rstandard(full_model))>2)
count_out

# No invalid outliers. Do not delete

# In the summary output of the full model, visibility has a high p-value
# It might not be significant
# Check if visibility is significant using anova

# H0 : Beta for Visibility is 0
# H1 : Beta for Visibility is not 0

full_noVis = lm(RentedBikeCount ~ .-Visibility, data = train)
summary(full_noVis)

anova(full_model,full_noVis)
# p-value for Visibility (0.971) is greater than 0.05
# Do not reject H0 and conclude that visibilty is  not important
# Rather than manualy picking variables, we decided to use selection procedures in R to pick the most important variables

# Perform variable selection using forward, backward, stepwise methods using AIC and BIC as the criterion to compare

# Model 1 : Forward selection - AIC
null_model = lm(RentedBikeCount ~ 1, data = train)
for_model_aic = step(null_model, scope = RentedBikeCount ~ Hour + Temperature + Humidity + 
                   WindSpeed + Visibility + DewPointTemp + SolarRad + Rainfall + Snowfall + 
                   Seasons + Holiday + FunctioningDay, direction = "forward", trace = 0)
summary(for_model_aic)

# Model 2 : Forward selection - BIC
n = nrow(train)
for_model_bic = step(null_model, scope = RentedBikeCount ~ Hour + Temperature + Humidity + 
                       WindSpeed + Visibility + DewPointTemp + SolarRad + Rainfall + Snowfall + 
                       Seasons + Holiday + FunctioningDay, direction = "forward", trace = 0, k = log(n))
summary(for_model_bic)

# Model 3 : Backward Selection - AIC
back_model_aic = step(full_model, direction = "backward", trace = 0)
summary(back_model_aic)

# Model 4 : Backward Selection - BIC
n = nrow(train)
back_model_bic = step(full_model, direction = "backward", trace = 0, k = log(n))
summary(back_model_bic)

# Model 5 : Stepwise Selection - AIC
null_model = lm(RentedBikeCOunt ~ 1, data = train)
step_aic = step(null_model, scope = RentedBikeCount ~ Hour + Temperature + Humidity + 
                  WindSpeed + Visibility + DewPointTemp + SolarRad + Rainfall + Snowfall + 
                  Seasons + Holiday + FunctioningDay, direction = "both", trace = 0)
summary(step_aic)
plot(step_aic)

# All assumptions are violated


# Model 6 : Stepwise Selection - BIC
n = nrow(train)
step_bic = step(full_model, direction = "both", trace = 0, k = log(n))
summary(step_bic)

# Go ahead with Stepwise selection using AIC as the criteria
base_model = step_aic
summary(base_model)

# We see that Temperature and DewPointTemp are highly correlated. Let'se try to drop Dew and check its significance
base_model_NoDew = lm(RentedBikeCount ~ Temperature + Hour + FunctioningDay + Humidity + 
                        Seasons + Rainfall + SolarRad + Holiday + 
                        Snowfall + WindSpeed, data = train)

anova(base_model, base_model_NoDew)
# p-value is very low (0.001214)
# DewPointTemp is significant, even though it is corr with Temperature

# Try to drop Temperature and compare
base_model_NoTemp = lm(RentedBikeCount ~ DewPointTemp + Hour + FunctioningDay + Humidity + 
                        Seasons + Rainfall + SolarRad + Holiday + 
                        Snowfall + WindSpeed, data = train)

# p-value is very low (0.002262)
# Temp is significant, even though it is corr with DewPoint

anova(base_model, base_model_NoTemp)

model_before_interac = lm(RentedBikeCount ~ Temperature + Hour + FunctioningDay + Humidity + 
                            Seasons + Rainfall + SolarRad + Holiday + 
                            Snowfall+ DewPointTemp + WindSpeed, data = train)

# Check if this model meets assumptions
plot(model_before_interac)

# Try Boxcox on this model
eps = 1e-2
n = nrow(train)
mu = seq(min(train$RentedBikeCount) + eps, max(train$RentedBikeCount), length = n)

model_eps = lm((RentedBikeCount + mu) ~ Temperature + Hour + FunctioningDay + Humidity + 
                 Seasons + Rainfall + SolarRad + Holiday + 
                 Snowfall+ DewPointTemp + WindSpeed, data = train)
boxcox(model_eps)
boxcox(model_eps, lambda = seq(0.5, 1.0, by = 0.05))

lambda = 0.75
box_model <- lm((((RentedBikeCount + mu)^(lambda)-1)/(lambda)) ~ Temperature + Hour + FunctioningDay + Humidity + 
                  Seasons + Rainfall + SolarRad + Holiday + 
                  Snowfall+ DewPointTemp + WindSpeed, data = train)
summary(box_model)
plot(box_model)

bptest(box_model)

# It does not. 
# Boxcox does not help either
# Check other transformations

log_trans = lm(sqrt(RentedBikeCount+1) ~ Temperature + Hour + FunctioningDay + Humidity + 
                 Seasons + Rainfall + SolarRad + Holiday + 
                 Snowfall+ DewPointTemp + WindSpeed, data = train)
summary(log_trans)
plot(log_trans)

# This log model does not meet assumptions

# Lets try to add interactions


# Create a model with interactions
model_all_int = lm(RentedBikeCount ~ Temperature + Hour + FunctioningDay + Humidity + 
                     Seasons + Rainfall + SolarRad + Holiday + 
                     Snowfall+ DewPointTemp + WindSpeed + Temperature:Humidity + 
                     Visibility:Hour + Seasons:WindSpeed + SolarRad:Temperature, data = train)
summary(model_all_int)

# Test usng anova if any of the interactions are significant
anova(base_model, model_all_int)


# We see that interaction terms are significant but Snowfall or Windspeed are no longer significant. 
# Perform an anova test of the models - Interactions with and without snowfall

model_all_int_NoSnowNoWind = lm(RentedBikeCount ~ Temperature + Hour + FunctioningDay + Humidity + 
                                  Seasons + Rainfall + SolarRad + Holiday + 
                                  DewPointTemp + Temperature:Humidity + 
                                  Visibility:Hour + Seasons:WindSpeed + SolarRad:Temperature, data = train)
summary(model_all_int_NoSnowNoWind)
anova(model_all_int,model_all_int_NoSnowNoWind)

# p-value is high (0.3757), meaning that B coefficients for Snowfall and WindSpeed are not significant

# The significant two-way interactions are found

# Moving on to three-way interactions
# Interaction 1 : Temperature : Humidity : SolarRadiation
model_ThreeWay = lm(RentedBikeCount ~ Temperature + Hour + FunctioningDay + Humidity + 
                      Seasons + Rainfall + SolarRad + Holiday + 
                      DewPointTemp + Temperature:Humidity + 
                      Visibility:Hour + Seasons:WindSpeed + SolarRad:Temperature 
                    + Temperature:Humidity:SolarRad, data = train)
summary(model_ThreeWay)
vif(model_ThreeWay)

# We see that even though all the interactions are significant, 
# The vif values for Temperature, Seasons and DewPoint is very high

# We create a model which does not have the variables having extremely high vif values

model_ThreeWay_NoVIF = lm(RentedBikeCount ~ Temperature + Hour + FunctioningDay + Humidity 
                             + Rainfall + SolarRad + Holiday + 
                            Visibility:Hour + Seasons:WindSpeed 
                          + Temperature:Humidity:SolarRad, data = train)
summary(model_ThreeWay_NoVIF)
vif(model_ThreeWay_NoVIF)

# All the predictors are important and highest vif value = 5.229851

# Check if this model is meeting any assumptions

# 1. Linearity - Violated
plot(fitted.values(model_ThreeWay_NoVIF), residuals(model_ThreeWay_NoVIF), xlab = "Fitted Values", ylab = "Residuals",
     main = "Model Residuals v/s Fitted Vales")
abline(h = 0, col = 'red')

# 2. Equal Variance  - Violated
bptest(model_ThreeWay_NoVIF)
# p-value is 2.2e-16

# 3. Normality - Violated
qqnorm(resid(model_ThreeWay_NoVIF), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(model_ThreeWay_NoVIF), col = "dodgerblue", lwd = 2)

residuals <-resid(model_ThreeWay_NoVIF)
shapiro.test(residuals[0:5000])
# p-value = 2.2e-16

# --------------------------------------------------------------------

# Try Boxcox Transformation on model_ThreeWay_NoVIF

eps = 1e-2
n = nrow(train)
mu = seq(min(train$RentedBikeCount) + eps, max(train$RentedBikeCount), length = n)

model_eps = lm((RentedBikeCount + mu )~ Temperature + Hour + FunctioningDay + Humidity 
               + Rainfall + SolarRad + Holiday + 
                 Visibility:Hour + Seasons:WindSpeed 
               + Temperature:Humidity:SolarRad, data = train)
boxcox(model_eps)
boxcox(model_eps, lambda = seq(0.5, 1.0, by = 0.05))

lambda = 0.75
box_model <- lm((((RentedBikeCount + mu)^(lambda)-1)/(lambda)) ~ Temperature + Hour + FunctioningDay + Humidity 
                + Rainfall + SolarRad + Holiday + 
                  Visibility:Hour + Seasons:WindSpeed 
                + Temperature:Humidity:SolarRad, data = train)
summary(box_model)
vif(box_model) # ok
plot(box_model)

# Equal Variance seems to be ok, lets verify using bptest
# Equal variance violated
bptest(box_model)

# Check normality
# Violated
residuals <-resid(box_model)
shapiro.test(residuals[0:5000])


# Boxcox transformation is not significant


# New Basline model to add higher degree fits : model_ThreeWay_NoVIF

model_ThreeWay_NoVIF = lm(RentedBikeCount ~ Temperature + Hour + FunctioningDay + Humidity 
                          + Rainfall + SolarRad + Holiday + 
                            Visibility:Hour + Seasons:WindSpeed 
                          + Temperature:Humidity:SolarRad, data = train)

new_base = model_ThreeWay_NoVIF
model_interactions = model_ThreeWay_NoVIF

# 1. Hour - categorical
# Transformation does not make sense
plot(train$Hour, train$RentedBikeCount)

# 2. Temperature (try square and cube)
lm_temp_sq = lm(RentedBikeCount ~ I(Temperature^2), data = train)
graphics.off()
plot(RentedBikeCount ~ Temperature, data = train, pch = 20, cex = 1.5, main = "Fitting Temperature^2")
xplot = seq(-17.8, 39.3, by = 0.1)
lines(xplot, predict(lm_temp_sq, newdata = data.frame(Temperature = xplot)),
      col = "darkorange", lwd = 3, lty = 1)

# Cube looks good. Go with cube
lm_temp_cube = lm(RentedBikeCount ~ I(Temperature^3), data = train)
graphics.off()
plot(RentedBikeCount ~ Temperature, data = train, pch = 20, cex = 1.5, main = "Fitting Temperature^3")
xplot = seq(-17.8, 39.3, by = 0.1)
lines(xplot, predict(lm_temp_cube, newdata = data.frame(Temperature = xplot)),
      col = "darkorange", lwd = 3, lty = 1)

# 3. Humidity - none of the interactions make sense
lm_hum = lm(RentedBikeCount ~ Humidity + I(Humidity^2)+ I(Humidity^3), data = train)
graphics.off()
plot(RentedBikeCount ~ Humidity, data = train, pch = 20, cex = 1.5, main = "Fitting Humidity")
xplot = seq(-10, 110, by = 0.1)
lines(xplot, predict(lm_hum, newdata = data.frame(Humidity = xplot)),
      col = "darkorange", lwd = 3, lty = 1)

# 4. Windspeed - no
lm_wind = lm(RentedBikeCount ~ WindSpeed+ I(WindSpeed^3), data = train)
graphics.off()
plot(RentedBikeCount ~ WindSpeed, data = train, pch = 20, cex = 1.5, main = "Fitting WindSpeed")
xplot = seq(0,8, by = 0.1)
lines(xplot, predict(lm_wind, newdata = data.frame(WindSpeed = xplot)),
      col = "darkorange", lwd = 3, lty = 1)

# 5. Visibility - we dropped this

# 6. DewPointTemp - we dropped this

# 7. SolarRadiation
lm_solar = lm(RentedBikeCount ~ SolarRad + I(SolarRad^2)+I(SolarRad^3), data = train)
graphics.off()
plot(RentedBikeCount ~ SolarRad, data = train, pch = 20, cex = 1.5, main = "Fitting SolarRad")
xplot = seq(0,10, by = 0.1)
lines(xplot, predict(lm_solar, newdata = data.frame(SolarRad = xplot)),
      col = "darkorange", lwd = 3, lty = 1)

# 8. Rainfall
lm_rain = lm(RentedBikeCount ~ I(exp(-Rainfall)), data = train)
graphics.off()
plot(RentedBikeCount ~ Rainfall, data = train, pch = 20, cex = 1.5, main = "Fitting Rainfall")
xplot = seq(0,25, by = 0.1)
lines(xplot, predict(lm_rain, newdata = data.frame(Rainfall = xplot)),
      col = "darkorange", lwd = 3, lty = 1)

# 9. Snowfall
lm_snow = lm(RentedBikeCount ~ I(exp(-Snowfall)), data = train)
graphics.off()
plot(RentedBikeCount ~ Snowfall, data = train, pch = 20, cex = 1.5, main = "Fitting Snowfall")
xplot = seq(0,8, by = 0.1)
lines(xplot, predict(lm_snow, newdata = data.frame(Snowfall = xplot)),
      col = "darkorange", lwd = 3, lty = 1)

summary(new_base)

# Create a model with higher order terms

model_transform1 = lm(RentedBikeCount ~ I(Temperature^3) + Hour + FunctioningDay + 
                       Humidity + Seasons + I(exp(-Rainfall)) + 
                       SolarRad +I(SolarRad^2)+I(SolarRad^3)+ Holiday + Temperature:Humidity + 
                       Visibility:Hour + Seasons:WindSpeed + SolarRad:Temperature + 
                       Temperature:Humidity:SolarRad, data = train)
# Adjusted R-squared:  0.5679 
# All predictors are significant
summary(model_transform1)

# Extremely high vif for Seasons, Solar^2, Solar^3
vif(model_transform1)

model_transform1_noVIF = lm(RentedBikeCount ~ I(Temperature^3) + Hour + FunctioningDay + 
                              Humidity  + I(exp(-Rainfall)) + 
                              SolarRad + Holiday + Temperature:Humidity + 
                              Visibility:Hour + Seasons:WindSpeed + SolarRad:Temperature + 
                              Temperature:Humidity:SolarRad, data = train)
summary(model_transform1_noVIF)
vif(model_transform1_noVIF)
# This is still ok

# This model has all the useful predictors and an acceptable vif value
model_transform1_noVIF

# ---------------------------------

# Check assumptions

model_transform1_noVIF = lm(RentedBikeCount ~ I(Temperature^3) + Hour + FunctioningDay + 
                              Humidity  + I(exp(-Rainfall)) + 
                              SolarRad + Holiday + Temperature:Humidity + 
                              Visibility:Hour + Seasons:WindSpeed + SolarRad:Temperature + 
                              Temperature:Humidity:SolarRad, data = train)

basline = model_transform1_noVIF
model_interacPoly = model_transform1_noVIF

# 1. Linearity - Violated
plot(fitted.values(basline), residuals(basline), xlab = "Fitted Values", ylab = "Residuals",
     main = "Model Residuals v/s Fitted Vales")
abline(h = 0, col = 'red')

# 2. Equal Variance  - Violated
bptest(basline)
# p-value is 2.2e-16

# 3. Normality - Violated
qqnorm(resid(basline), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(basline), col = "dodgerblue", lwd = 2)

residuals <-resid(basline)
shapiro.test(residuals[0:5000])

# Boxcox Transformation
eps = 1e-2
n = nrow(train)
mu = seq(min(train$RentedBikeCount) + eps, max(train$RentedBikeCount), length = n)

model_eps = lm((RentedBikeCount + mu) ~ I(Temperature^3) + Hour + FunctioningDay + 
                 Humidity  + I(exp(-Rainfall)) + 
                 SolarRad + Holiday + Temperature:Humidity + 
                 Visibility:Hour + Seasons:WindSpeed + SolarRad:Temperature + 
                 Temperature:Humidity:SolarRad, data = train)
boxcox(model_eps)
boxcox(model_eps, lambda = seq(0.5, 1.0, by = 0.05))

lambda = 0.75
box_model_final <- lm((((RentedBikeCount + mu)^(lambda)-1)/(lambda)) ~ I(Temperature^3) + Hour + FunctioningDay + 
                        Humidity  + I(exp(-Rainfall)) + 
                        SolarRad + Holiday + Temperature:Humidity + 
                        Visibility:Hour + Seasons:WindSpeed + SolarRad:Temperature + 
                        Temperature:Humidity:SolarRad, data = train)
summary(box_model_final)
plot(box_model_final)

# Assumption of linearity violated

# p-value has improved, but is still violating the assumption of equal variance
bptest(box_model_final)

residuals <-resid(box_model_final)
shapiro.test(residuals[0:5000])

# None of the assumptions are met

# Lets try to find the best performing model among these three and use a transformation on it


#Finding best model 

# AIC: chooses model_interactions
AIC(model_before_interac,model_interactions, model_interacPoly) 

# BIC: chooses model_interactions
BIC(model_before_interac,model_interactions, model_interacPoly) 

# Adjusted_R2: chooses model_interactions
summary(model_before_interac)$adj.r.squared
summary(model_interactions)$adj.r.squared
summary(model_interacPoly)$adj.r.squared

# R2: chooses model_interactions
summary(model_before_interac)$r.squared
summary(model_interactions)$r.squared
summary(model_interacPoly)$r.squared

n = nrow(train)

# RMSE - train
# Chooses model_interactions
sqrt(sum(resid(model_before_interac)^2)/n)
sqrt(sum(resid(model_interactions)^2)/n)
sqrt(sum(resid(model_interacPoly)^2)/n)


# RMSE - test
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

# chooses model_interactions
rmse(predict(model_before_interac, newdata = test), test$RentedBikeCount)
rmse(predict(model_interactions, newdata = test), test$RentedBikeCount)
rmse(predict(model_interacPoly, newdata = test), test$RentedBikeCount)

# PRESS (or RMSE with LOOCV)
# Chooses model_interactions
sqrt(sum((resid(model_before_interac)/(1-hatvalues(model_before_interac)))^2)/n)
sqrt(sum((resid(model_interactions)/(1-hatvalues(model_interactions)))^2)/n)
sqrt(sum((resid(model_interacPoly)/(1-hatvalues(model_interacPoly)))^2)/n)

# BEST PERFORMING MODEL IS MODEL_INTERACTIONS

# Lets try to use log transformation on it to see if it meets assumptions
# Try to fix it by transforming both y and x

# Response Transformation
model_log = lm(log(RentedBikeCount + 1) ~ Temperature + Hour + 
                 FunctioningDay + Humidity + Rainfall + SolarRad + 
                 Holiday + Visibility:Hour + Seasons:WindSpeed + 
                  Temperature:Humidity:SolarRad, data = train)
summary(model_log)
vif(model_log)
plot(model_log)
# All asumptions violated

# Performance metrics for this log transformed model

# AIC
AIC(model_log)

# BIC
BIC(model_log)

# Adjusted R squared
summary(model_log)$adj.r.squared

# R squared
summary(model_log)$r.squared

# RMSE - train
sqrt(sum(resid(model_log)^2)/n)

# RMSE - test
rmse(predict(model_log, newdata = test), test$RentedBikeCount)

# PRESS
sqrt(sum((resid(model_log)/(1-hatvalues(model_log)))^2)/n)

# ------------------------------

# Variable Importance

# 1. Base Model

start = lm(RentedBikeCount ~ ., data = train)
vi = vi(start, method = "firm")
vip(vi, geom = "col", aesthetics = list(size = 2.5))
vif(start) # inflated

# 2. Model 1 - Model Before Interactions
model1 = lm(RentedBikeCount ~ Temperature + Hour + FunctioningDay + 
              Humidity + Seasons + Rainfall + SolarRad + Holiday + Snowfall + 
              DewPointTemp + WindSpeed, data = train)
vi1 = vi(model1, method = "firm")
vip(vi1, geom = "col", aesthetics = list(size = 2.5))
vif(model1) # Temp is bad

# 3. Model 2 - Model with interactions
model2 = lm(RentedBikeCount ~ Temperature + Hour + FunctioningDay + 
              Humidity + Rainfall + SolarRad + Holiday + Visibility:Hour + 
              Seasons:WindSpeed + Temperature:Humidity:SolarRad, data = train)
vi2 = vi(model2, method = "firm")
vip(vi2, geom = "col", aesthetics = list(size = 2.5))
vif(model2) # ok

# 4. Model 3 - Model with Interactions + Higher Order terms
model3 = lm(RentedBikeCount ~ I(Temperature^3) + Hour + FunctioningDay + 
              Humidity + I(exp(-Rainfall)) + SolarRad + Holiday + Temperature:Humidity + 
              Visibility:Hour + Seasons:WindSpeed + SolarRad:Temperature + 
              Temperature:Humidity:SolarRad, data = train)
vi3 = vi(model3, method = "firm")
vip(vi3, geom = "col", aesthetics = list(size = 2.5))
vif(model3) # a little inflated

# 5. Model 4. Log transformation of Model 2
model4 = lm(log(RentedBikeCount + 1) ~ Temperature + Hour + 
              FunctioningDay + Humidity + Rainfall + SolarRad + Holiday + 
              Visibility:Hour + Seasons:WindSpeed + Temperature:Humidity:SolarRad, 
            data = train)
vi4 = vi(model4, method = "firm")
vip(vi4, geom = "col", aesthetics = list(size = 2.5))
vif(model4) # ok


# ------------------------------

# Try Lasso and Ridge Regression
# Lasso and Ridge regression

# 1. Lasso 
library(glmnet)
xtrain = model.matrix(RentedBikeCount~.,train)[,-1]
ytrain = train[,1]

xtest = model.matrix(RentedBikeCount~.,test)[,-1]
ytest = test[,1]


fit_lasso_cv = cv.glmnet(xtrain, ytrain, alpha = 1, trace = 0)
plot(fit_lasso_cv)
bestlam = fit_lasso_cv$lambda.min;bestlam
fit_lasso_best = glmnet(xtrain, ytrain, alpha = 1, lambda = bestlam)
coef(fit_lasso_best)
summary(fit_lasso_best)

pred <- predict(fit_lasso_best, s = bestlam, newx = xtrain)
final <- cbind(ytest, pred)
final

actual <- train$RentedBikeCount
preds <- pred
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq

# Check model assumptions for Lasso model with best lambda

# 1. Linearity - Violated
plot(fitted.values(fit_lasso_best), residuals(fit_lasso_best), xlab = "Fitted Values", ylab = "Residuals",
     main = "Model Residuals v/s Fitted Vales")

y_pr = predict(fit_lasso_best,newdata=data.frame(xtest), type="response")
residuals(fit_lasso_best)
abline(h = 0, col = 'red')

plot(fit_lasso_best, xvar = "lambda", label = TRUE, lwd = 4)
abline(v = log(bestlam))

# 2. Equal Variance  - Violated
bptest(basline)
# p-value is 2.2e-16

# 3. Normality - Violated
qqnorm(resid(basline), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(basline), col = "dodgerblue", lwd = 2)

residuals <-resid(basline)
shapiro.test(residuals[0:5000])

# 2. Ridge
fit_ridge_cv = cv.glmnet(xtrain, ytrain, alpha = 0, trace = 0)
plot(fit_ridge_cv)
bestlam = fit_ridge_cv$lambda.min;bestlam
fit_ridge_best = glmnet(xtrain, ytrain, alpha = 0, lambda = bestlam)
coef(fit_ridge_best)
summary(fit_ridge_best)

pred <- predict(fit_ridge_best, s = bestlam, newx = xtrain)
final <- cbind(ytest, pred)
final

actual <- train$RentedBikeCount
preds <- pred
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq

# Lasso did not drop any variable
cbind(coef(fit_lasso_best), coef(fit_ridge_best))

# ----------------------------------

# Tree

# Initial Unpruned tree

# Unpruned tree
unpruned = tree(RentedBikeCount ~ ., data = train)
summary(unpruned)

# Variables used are :
# "Temperature","Seasons","Hour","Humidity","FunctioningDay","SolarRad","DewPointTemp" 

# Number of terminal nodes:  13

# Plot unpruned tree
plot(unpruned)
text(unpruned, pretty = 0)
title(main = "Unpruned Regression Tree")

# TUNE 
# cv.tree for tuning alpha
# RMSE = Squared root of MSE
set.seed(35)
unpruned_cv = cv.tree(unpruned) 
unpruned_cv
# tree size = |T|
plot(unpruned_cv$size, sqrt(unpruned_cv$dev / nrow(train)), type = "b",
     xlab = "Tree Size", ylab = "RMSE (cross validated)")

# best = 8
pruned_tree_8 = prune.tree(unpruned, best = 8)
summary(pruned_tree_8)

plot(pruned_tree_8)
text(pruned_tree_8, pretty = 0)
title(main = "Pruned Regression Tree with 8 Terminal Nodes")



# RMSE of this tree

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
pruned_8_pred = predict(pruned_tree_11, newdata = train)

# training error
rmse(pruned_8_pred, train$RentedBikeCount)

# test error
pruned_8_pred_test = predict(pruned_tree_8, newdata = test)
rmse(pruned_8_pred_test, test$RentedBikeCount)




