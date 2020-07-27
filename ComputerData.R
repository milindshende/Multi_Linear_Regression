compdata <- read.csv(file.choose()) # choose the compdata.csv data set
View(compdata)
sum(is.na(compdata))
dim(compdata)
colnames(compdata)
compdata <- compdata[,-1] # Removing 1st column which is is sr.no.
str(compdata)
attach(compdata)
compdata$cd<- ifelse(compdata$cd=="no",0,1)
compdata$multi<- ifelse(compdata$multi=="no",0,1)
compdata$premium<- ifelse(compdata$premium=="no",0,1)
compdata$cd<-ifelse(compdata$cd=="no",0,1)
compdata$multi<-ifelse(compdata$multi=="no",0,1)
compdata$premium<-ifelse(compdata$premium=="yes",1,2)
# Exploratory Data Analysis(60% of time)
# 1. Measures of Central Tendency
# 2. Measures of Dispersion
# 3. Third Moment Business decision
# 4. Fourth Moment Business decision
# 5. Probability distributions of variables
# 6. Graphical representations
  #  > Histogram,Box plot,Dot plot,Stem & Leaf plot, 
  #     Bar plot

summary(compdata)

### EDA of "Price" 

hist(compdata$price)
boxplot(compdata$price,horizontal = TRUE)
barplot(compdata$price)
library(moments)
skewness(compdata$price)
kurtosis(compdata$price)
qqnorm(compdata$price)

### EDA check for transformation of "Price"

compdata_price<-log(compdata$price) ######## Result OK
boxplot(compdata_price,horizontal = TRUE)
skewness(compdata_price)
kurtosis(compdata_price)
qqnorm(compdata_price)

### EDA of "Speed" 

hist(compdata$speed)
boxplot(compdata$speed,horizontal = TRUE)
barplot(compdata$speed)
library(moments)
skewness(compdata$speed)
kurtosis(compdata$speed)
qqnorm(compdata$speed)

### EDA check for transformation of "Speed"
compdata_speed<-log(compdata$speed)      #### Result ok
boxplot(compdata_speed,horizontal = TRUE)
skewness(compdata_speed)
kurtosis(compdata_speed)
qqnorm(compdata_speed)

compdata_speed1<-sqrt(compdata$speed)            ### Result NOK
compdata_speed2<-(compdata$speed*compdata$speed) ### Result NOK
boxplot(compdata_speed1,horizontal = TRUE)
skewness(compdata_speed1)
kurtosis(compdata_speed1)
qqnorm(compdata_speed1)

### EDA of "hd" 

hist(compdata$hd)
boxplot(compdata$hd,horizontal = TRUE)
barplot(compdata$hd)
library(moments)
skewness(compdata$hd)
kurtosis(compdata$hd)
qqnorm(compdata$hd)

### EDA check for transformation of "hd"
compdata_hd<-log(compdata$hd)      #### Result ok
boxplot(compdata_hd,horizontal = TRUE)
skewness(compdata_hd)
kurtosis(compdata_hd)
qqnorm(compdata_hd)

compdata_speed1<-sqrt(compdata$hd)            ### Result NOK
compdata_speed2<-(compdata$hd*compdata$hd)    ### Result NOK
boxplot(compdata_speed2,horizontal = TRUE)
skewness(compdata_speed2)
kurtosis(compdata_speed2)
qqnorm(compdata_speed2)

### EDA of "ram" 

hist(compdata$ram)
boxplot(compdata$ram,horizontal = TRUE)
barplot(compdata$ram)
library(moments)
skewness(compdata$ram)
kurtosis(compdata$ram)
qqnorm(compdata$ram)

### EDA check for transformation of "ram"
compdata_ram<-log(compdata$ram)      #### Result ok
compdata_ram1<-sqrt(compdata$ram)
compdata_ram2<-(compdata$ram * compdata$ram)
boxplot(compdata_ram2,horizontal = TRUE)
skewness(compdata_ram2)
kurtosis(compdata_ram2)
qqnorm(compdata_ram2)

### EDA of "screen" 

hist(compdata$screen)
boxplot(compdata$screen,horizontal = TRUE)
barplot(compdata$screen)
library(moments)
skewness(compdata$screen) # 1.63
kurtosis(compdata$screen) # 4.85
qqnorm(compdata$screen)

### EDA check for transformation of "screen"
compdata_screen1<-log(compdata$screen)      #### Result ok
compdata_screen2<-sqrt(compdata$screen)
compdata_screen3<-(compdata$screen * compdata$screen)
compdata_screen4<-exp(compdata$screen)
boxplot(compdata_screen4,horizontal = TRUE)
skewness(compdata_screen4) # 1.53 , 1.58 , 1.73 , 2.62
kurtosis(compdata_screen4)  # 4.52 , 4.68 , 5.18 , 8.11
qqnorm(compdata_screen4)



# 7. Find the correlation b/n Output (Price) & Input -Scatter plot
pairs(compdata)

# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
cor(compdata)

# corelation coefficient & scatter plot together
install.packages("GGally")
install.packages("stringi")
library(GGally)
window()
ggpairs(compdata)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(compdata))

# The Linear Model of interest with all the columns

model_compdata<- lm(price~.,data = compdata)

summary(model_compdata)

sqrt(mean(model_compdata$residuals^2))
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
influenceIndexPlot(model_compdata,id.n=3) # index plots for infuence measures
window()
influencePlot(model_compdata,id.n=3) # A user friendly representation of the above

# Based on Index plot results ; delete 1441 , 1701 record
model_compdata_1<-lm(price~.,data=compdata[-c(1441,1701),])
summary(model_compdata_1)
vif(model_compdata_1)
sqrt(mean(model_compdata_1$residuals^2))
hist(model_compdata_1$residuals)
plot(model_compdata_1)
windows()
influenceIndexPlot(model_compdata_1,id.n=3)

# Build another model 
model_compdata_2<-lm(log(price)~log(speed)+log(hd)+log(ram)+log(screen)+cd+multi+premium+ads+trend,data=compdata)
summary(model_compdata_2)
pred_compdata<-predict(model_compdata_2)
pred_compdata_2<-exp(pred_compdata)
pred_compdata_2
err= price - pred_compdata_2
vif(model_compdata_2)
sqrt(mean(err^2))
hist(err)
plot(model_compdata_2)
windows()
influenceIndexPlot(model_compdata_2,id.n=3)

# Build another model 
model_compdata_3<-lm(log(price)~log(speed)+log(hd)+log(ram)+log(screen)+cd+multi+premium,data=compdata)
summary(model_compdata_3)
r3<-predict(model_compdata_3)
pred_compdata_3<-exp(r3)
pred_compdata_3
err_3= price - pred_compdata_3
vif(model_compdata_3)
sqrt(mean(err_3^2))
hist(err_3)

# Build another model 
model_compdata_4<-lm(log(price)~.,data=compdata)
summary(model_compdata_4)
r4<-predict(model_compdata_4)
pred_compdata_4<-exp(r4)
pred_compdata_4
err_4= price - pred_compdata_4
vif(model_compdata_4)
sqrt(mean(err_4^2))
hist(err_4)

# Build another model 
model_compdata_5<-lm(log(price)~log(speed)+log(hd)+log(ram)+log(screen),data=compdata)
summary(model_compdata_5)
r5<-predict(model_compdata_5)
pred_compdata_5<-exp(r5)
pred_compdata_5
err_5= price - pred_compdata_5
vif(model_compdata_5)
sqrt(mean(err_5^2))
hist(err_5)

# Build another model 
model_compdata_6<-lm(price~sqrt(speed)+sqrt(hd)+sqrt(ram)+sqrt(screen)+cd+multi+premium+ads+trend,data=compdata)
summary(model_compdata_6)
sqrt(mean(model_compdata_6$residuals^2))
r5<-predict(model_compdata_5)
pred_compdata_5<-exp(r5)
pred_compdata_5
err_5= price - pred_compdata_5
vif(model_compdata_5)
sqrt(mean(err_5^2))
hist(err_5)

##################################

# Splitting the data into Traing & Testing
install.packages("caTools")
library(caTools)

split<-sample.split(compdata$price,SplitRatio = 0.70)
split
table(split)
compdata.train<-subset(compdata,split==TRUE)
compdata.test<-subset(compdata,split==FALSE)

# Building model Train
model_compdata_train<-lm(price~.,compdata.train)
summary(model_compdata_train) #R^2 = 0.7768
train_RMSE_compdata<-sqrt(mean(model_compdata_train$residuals^2))
train_RMSE_compdata  # 275.78
vif(model_compdata_train)
plot(model_compdata_train)

# Predicting on Test Data

predtest_compdata<-predict(model_compdata_train,compdata.test)
predtest_compdata
compdata_test_errors<-compdata.test$price-predtest_compdata
compdata_test_errors

#calculate test RMSE
test_RMSE_compdata<-sqrt(mean(compdata_test_errors^2))
test_RMSE_compdata # 273.644

# As training & test RMSE is nerby ; we can finalise this model.
