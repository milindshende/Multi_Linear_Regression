toyotacorolla <- read.csv(file.choose()) # choose the toyotacorolla.csv data set
View(toyotacorolla)
colnames(toyotacorolla)
toyotacorolla<-toyotacorolla[,c(3,4,7,9,13,14,16,17,18)] #consider selective columns
sum(is.na(toyotacorolla))
dim(toyotacorolla)
str(toyotacorolla)
attach(toyotacorolla)
# Exploratory Data Analysis(60% of time)
# 1. Measures of Central Tendency
# 2. Measures of Dispersion
# 3. Third Moment Business decision
# 4. Fourth Moment Business decision
# 5. Probability distributions of variables
# 6. Graphical representations
  #  > Histogram,Box plot,Dot plot,Stem & Leaf plot, 
  #     Bar plot

summary(toyotacorolla)

### EDA of "Price" 
hist(toyotacorolla$Price)
boxplot(toyotacorolla$Price,horizontal = TRUE) # many outliers
barplot(toyotacorolla$Price)
library(moments)
skewness(toyotacorolla$Price) # 1.70
kurtosis(toyotacorolla$Price) # 6.72
qqnorm(toyotacorolla$Price)

### EDA check for transformation of "Price"
toyotacorolla_price1<-log(toyotacorolla$Price) # Result OK
toyotacorolla_price2<-sqrt(toyotacorolla$Price) 
toyotacorolla_price3<-(toyotacorolla$Price^2) 
toyotacorolla_price4<-log(sqrt(toyotacorolla$Price)) 
toyotacorolla_price5<-sqrt(log(toyotacorolla$Price)) 
boxplot(toyotacorolla_price5,horizontal = TRUE)
skewness(toyotacorolla_price5) # 0.73 , 1.20 , 3.01 , 0.73 , 0.68
kurtosis(toyotacorolla_price5) # 3.61 , 4.68 , 17.02 , 3.61 ,3.54
qqnorm(toyotacorolla_price5)

### EDA of "Age0804" 
hist(toyotacorolla$Age_08_04)
boxplot(toyotacorolla$Age_08_04,horizontal = TRUE) # outliers
barplot(toyotacorolla$Age_08_04)
library(moments)
skewness(toyotacorolla$Age_08_04) # -0.82
kurtosis(toyotacorolla$Age_08_04) # 2.91
qqnorm(toyotacorolla$Age_08_04)

### EDA check for transformation of "Age0804"
toyotacorolla_Age1<-log(toyotacorolla$Age_08_04) 
toyotacorolla_Age2<-sqrt(toyotacorolla$Age_08_04) 
toyotacorolla_Age3<-(toyotacorolla$Age_08_04^2)        # Result OK
toyotacorolla_Age4<-log(sqrt(toyotacorolla$Age_08_04)) 
toyotacorolla_Age5<-sqrt(log(toyotacorolla$Age_08_04))
toyotacorolla_Age6<-(toyotacorolla$Age_08_04^3)

boxplot(toyotacorolla_Age6,horizontal = TRUE)
skewness(toyotacorolla_Age6) # -2.58,-1.38,-0.18,-2.58,-4.76,0.22
kurtosis(toyotacorolla_Age6) # 12.84,4.74,1.96,12.84,45.47,1.90
qqnorm(toyotacorolla_Age6)

### EDA of "KM" 
hist(toyotacorolla$KM)
boxplot(toyotacorolla$KM,horizontal = TRUE) # many outliers
barplot(toyotacorolla$KM)
library(moments)
skewness(toyotacorolla$KM) # 1.01
kurtosis(toyotacorolla$KM) # 4.67
qqnorm(toyotacorolla$KM)

### EDA check for transformation of "KM"
toyotacorolla_km1<-log(toyotacorolla$KM) 
toyotacorolla_km2<-sqrt(toyotacorolla$KM) # Result ok
toyotacorolla_km3<-(toyotacorolla$KM^2)        
toyotacorolla_km4<-log(sqrt(toyotacorolla$KM)) 
toyotacorolla_km5<-sqrt(log(toyotacorolla$KM))

boxplot(toyotacorolla_km5,horizontal = TRUE)
skewness(toyotacorolla_km5) # -6.52,-0.11, 2.83, -6.52, -10.28
kurtosis(toyotacorolla_km5) # 63.87, 3.78, 14.08, 63.87, 123.55
qqnorm(toyotacorolla_km5)

### EDA of "HP" 
hist(toyotacorolla$HP)
boxplot(toyotacorolla$HP,horizontal = TRUE) # outliers
barplot(toyotacorolla$HP)
library(moments)
skewness(toyotacorolla$HP) # 0.95
kurtosis(toyotacorolla$HP) # 11.80
qqnorm(toyotacorolla$HP)

### EDA check for transformation of "HP"
toyotacorolla_HP1<-log(toyotacorolla$HP)  # Result ok
toyotacorolla_HP2<-sqrt(toyotacorolla$HP) 
toyotacorolla_HP3<-(toyotacorolla$HP^2)        
toyotacorolla_HP4<-log(sqrt(toyotacorolla$HP)) 
toyotacorolla_HP5<-sqrt(log(toyotacorolla$HP))

boxplot(toyotacorolla_HP5,horizontal = TRUE)
skewness(toyotacorolla_HP5) # -0.40, 0.16, 3.36, -0.40, -0.49
kurtosis(toyotacorolla_HP5) # 5.40, 7.53, 30.71, 5.40, 5.15
qqnorm(toyotacorolla_HP5)

### EDA of "CC" 
hist(toyotacorolla$cc)
boxplot(toyotacorolla$cc,horizontal = TRUE) # outliers
barplot(toyotacorolla$cc)
library(moments)
skewness(toyotacorolla$cc) # 27.40
kurtosis(toyotacorolla$cc) # 930.46
qqnorm(toyotacorolla$cc)

### EDA check for transformation of "CC"
toyotacorolla_cc1<-log(toyotacorolla$cc)  # Result ok
toyotacorolla_cc2<-sqrt(toyotacorolla$cc) 
toyotacorolla_cc3<-(toyotacorolla$cc^2)        
toyotacorolla_cc4<-log(sqrt(toyotacorolla$cc)) 
toyotacorolla_cc5<-sqrt(log(toyotacorolla$cc))

boxplot(toyotacorolla_cc5,horizontal = TRUE)
skewness(toyotacorolla_cc5) # 3.93, 13.15,37.38, 3.93, 3.34
kurtosis(toyotacorolla_cc5) # 68.48, 346.52, 1410.45, 68.48, 55.24 
qqnorm(toyotacorolla_cc5)

### EDA of "Doors" 
hist(toyotacorolla$Doors)
boxplot(toyotacorolla$Doors,horizontal = TRUE) # outliers
barplot(toyotacorolla$Doors)
library(moments)
skewness(toyotacorolla$Doors) # -0.07 - Result ok
kurtosis(toyotacorolla$Doors) # 1.12 - Result ok
qqnorm(toyotacorolla$Doors)

### EDA check for transformation of "Doors"
toyotacorolla_dr1<-log(toyotacorolla$Doors)  
toyotacorolla_dr2<-sqrt(toyotacorolla$Doors) 
toyotacorolla_dr3<-(toyotacorolla$Doors^2)        
toyotacorolla_dr4<-log(sqrt(toyotacorolla$Doors)) 
toyotacorolla_dr5<-sqrt(log(toyotacorolla$Doors))

boxplot(toyotacorolla_dr5,horizontal = TRUE)
skewness(toyotacorolla_dr5) # -0.12, -0.10,-0.03, -0.12, -0.15
kurtosis(toyotacorolla_dr5) # 1.17, 1.14, 1.10, 1.17, 1.21
qqnorm(toyotacorolla_dr5)

### EDA of "Gears" 
hist(toyotacorolla$Gears)
boxplot(toyotacorolla$Gears,horizontal = TRUE) # outliers
barplot(toyotacorolla$Gears)
library(moments)
skewness(toyotacorolla$Gears) # 2.28
kurtosis(toyotacorolla$Gears) # 40.56
qqnorm(toyotacorolla$Gears)

### EDA check for transformation of "Gears"
toyotacorolla_gr1<-log(toyotacorolla$Gears)  ## Result ok
toyotacorolla_gr2<-sqrt(toyotacorolla$Gears) 
toyotacorolla_gr3<-(toyotacorolla$Gears^2)        
toyotacorolla_gr4<-log(sqrt(toyotacorolla$Gears)) 
toyotacorolla_gr5<-sqrt(log(toyotacorolla$Gears))

boxplot(toyotacorolla_gr5,horizontal = TRUE)
skewness(toyotacorolla_gr5) # -0.60, 1.04, 3.83, -0.06, -1.98
kurtosis(toyotacorolla_gr5) # 69.04, 51.08, 31.85, 69.04, 87.00
qqnorm(toyotacorolla_gr5)

### EDA of "QtrlyTax" 
hist(toyotacorolla$Quarterly_Tax)
boxplot(toyotacorolla$Quarterly_Tax,horizontal = TRUE) # outliers
barplot(toyotacorolla$Quarterly_Tax)
library(moments)
skewness(toyotacorolla$Quarterly_Tax) # 1.99
kurtosis(toyotacorolla$Quarterly_Tax) # 7.27
qqnorm(toyotacorolla$Quarterly_Tax)

### EDA check for transformation of "QtrlyTax"
toyotacorolla_qt1<-log(toyotacorolla$Quarterly_Tax)  ## Result ok
toyotacorolla_qt2<-sqrt(toyotacorolla$Quarterly_Tax) 
toyotacorolla_qt3<-(toyotacorolla$Quarterly_Tax^2)        
toyotacorolla_qt4<-log(sqrt(toyotacorolla$Quarterly_Tax)) 
toyotacorolla_qt5<-sqrt(log(toyotacorolla$Quarterly_Tax))

boxplot(toyotacorolla_qt5,horizontal = TRUE)
skewness(toyotacorolla_qt5) # -0.73, 0.94, 2.92, -0.73, -1.18
kurtosis(toyotacorolla_qt5) # 7.07 5.97, 11.68, 7.07, 7.89
qqnorm(toyotacorolla_qt5)

### EDA of "weight" 
hist(toyotacorolla$Weight)
boxplot(toyotacorolla$Weight,horizontal = TRUE) # outliers
barplot(toyotacorolla$Weight)
library(moments)
skewness(toyotacorolla$Weight) # 3.10
kurtosis(toyotacorolla$Weight) # 22.29
qqnorm(toyotacorolla$Weight)

### EDA check for transformation of "weight"
toyotacorolla_wt1<-log(toyotacorolla$Weight)  ## Result ok
toyotacorolla_wt2<-sqrt(toyotacorolla$Weight) 
toyotacorolla_wt3<-(toyotacorolla$Weight^2)        
toyotacorolla_wt4<-log(sqrt(toyotacorolla$Weight)) 
toyotacorolla_wt5<-sqrt(log(toyotacorolla$Weight))

boxplot(toyotacorolla_wt5,horizontal = TRUE)
skewness(toyotacorolla_wt5) # 2.44, 2.75, 3.97, 2.44, 2.39
kurtosis(toyotacorolla_wt5) # 15.35, 18.42, 33.19, 15.35, 14.97
qqnorm(toyotacorolla_wt5)


# 7. Find the correlation b/n Output (Price) & Input -Scatter plot
pairs(toyotacorolla)

# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
cor(toyotacorolla)

# corelation coefficient & scatter plot together
install.packages("GGally")
install.packages("stringi")
library(GGally)
window()
ggpairs(compdata)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(toyotacorolla))

# Build model
#The Linear Model of interest with all the columns

model_tc0<- lm(Price~.,data = toyotacorolla)

summary(model_tc0)

sqrt(mean(model_tc0$residuals^2))
install.packages("carData")
library(car)
vif(model_compdata)

## vif>10 then there exists collinearity among all the variables 

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model_compdata)

# Deletion Diagnostics for identifying influential observations
influence.measures(model_compdata)
library(car)
## plotting Influential measures 
windows()
influenceIndexPlot(model_tc0,id.n=3) # index plots for infuence measures
window()
influencePlot(model_compdata,id.n=3) # A user friendly representation of the above

# Build another model # 1
# Based on Index plot results ; delete 81,222 record
model_tc1<-lm(Price~.,data=toyotacorolla[-c(81,222),])
summary(model_tc1)
vif(model_tc1)
sqrt(mean(model_tc1$residuals^2))
hist(model_tc1$residuals)
plot(model_tc1)
windows()
influenceIndexPlot(model_tc1,id.n=3)

# Build another model # 2
model_tc2<-lm(log(Price)~(Age_08_04^2)+sqrt(KM)+log(HP)+log(cc)+Doors+log(Gears)+log(Quarterly_Tax)+log(Weight),data = toyotacorolla)
summary(model_tc2)
#pred<-predict(model_tc2)
pred_tc2<-exp(predict(model_tc2))
err= Price - pred_tc2
sqrt(mean(err^2))
vif(model_tc2)
hist(err)

windows()
influenceIndexPlot(model_compdata_2,id.n=3)

# Build another model # 3 
model_tc3<-lm(log(Price)~.,data=toyotacorolla)
summary(model_tc3)
#r3<-predict(model_tc3)
pred_tc3<-exp(predict(model_tc3))
err_3= Price - pred_tc3
sqrt(mean(err_3^2))
vif(model_tc3)
hist(err_3)

# Build another model # 4 (remove rows 81,222)
model_tc4<-lm(log(Price)~.,data=toyotacorolla[-c(81,222),])
summary(model_tc4)
#r4<-predict(model_tc4)
pred_tc4<-exp(predict(model_tc4))
err_4= Price[-c(81,222)] - pred_tc4
sqrt(mean(err_4^2))
vif(model_tc4)
hist(err_4)


# Build another model # 5
model_tc5<-lm(Price~(Age_08_04^2)+sqrt(KM)+log(HP)+log(cc)+Doors+log(Gears)+log(Quarterly_Tax)+log(Weight),data = toyotacorolla)
summary(model_tc5)
sqrt(mean(model_tc5$residuals^2))
vif(model_tc5)


##################################

# Splitting the data into Traing & Testing
install.packages("caTools")
library(caTools)
toyotacorolla_final<-toyotacorolla[-c(81,222),]

split<-sample.split(toyotacorolla_final$Price,SplitRatio = 0.75)
split
table(split)
tc.train<-subset(toyotacorolla_final,split==TRUE)
tc.test<-subset(toyotacorolla_final,split==FALSE)

# Building model Train
model_tc_train<-lm(log(Price)~.,tc.train)
summary(model_tc_train) #R^2 = 0.8528
pred_tc_train<-exp(predict(model_tc_train))
err_train= tc.train$Price - pred_tc_train
sqrt(mean(err_train^2))

# Predicting on Test Data
predtest_tc<-exp(predict(model_tc_train,tc.test))
tc_test_errors<-tc.test$Price-predtest_tc

#calculate test RMSE
test_RMSE_tc<-sqrt(mean(tc_test_errors^2))
test_RMSE_tc

# As training & test RMSE is nerby ; we can finalise this model.
