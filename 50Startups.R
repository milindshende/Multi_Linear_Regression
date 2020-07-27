startups50 <- read.csv(file.choose()) # choose the 50Startups.csv data set
View(startups50)
sum(is.na(startups50))
dim(startups50)
colnames(startups50)
startups50 <- startups50[,-4] # Removing the 4th column which is is state & catagorial
attach(startups50)
# Exploratory Data Analysis(60% of time)
# 1. Measures of Central Tendency
# 2. Measures of Dispersion
# 3. Third Moment Business decision
# 4. Fourth Moment Business decision
# 5. Probability distributions of variables
# 6. Graphical representations
  #  > Histogram,Box plot,Dot plot,Stem & Leaf plot, 
  #     Bar plot

summary(startups50)

# 7. Find the correlation b/n Output (Profit) & (R&D,Admin, Mkt)-Scatter plot
pairs(startups50)

# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
cor(startups50)

# corelation coefficient & scatter plot together
install.packages("GGally")
install.packages("stringi")
library(GGally)
window()
ggpairs(startups50)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(startups50))

# The Linear Model of interest with all the columns (R&D,Admin,Mkt)

model_startups50 <- lm(Profit~.,data=startups50)

summary(model_startups50)
r1<-model_startups50$residuals

mean(model_startups50$residuals)
sqrt(mean(model_startups50$residuals^2))
vif(model_startups50)

# Multicollinearity check
# Model based on only Administration 
model_startups50Admin<-lm(Profit~Administration)
summary(model_startups50Admin) 

# Model based on only Marketing Spend
model_startups50Mkt<-lm(Profit~Marketing.Spend)
summary(model_startups50Mkt) 

# Model based on Admin and Mkt
model_startups50AdMkt<-lm(Profit~Administration+Marketing.Spend)
summary(model_startups50AdMkt) 

# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables
install.packages("carData")
install.packages("car")
library(car)
vif(model_startups50) # Original model
## vif>10 then there exists collinearity among all the variables 

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model_startups50)


## AV plot has given us an indication to delete "Admin" variable


# Deletion Diagnostics for identifying influential observations
influence.measures(model_startups50)
library(car)
## plotting Influential measures 
windows()
influenceIndexPlot(model_startups50,id.n=3) # index plots for infuence measures
influencePlot(model_startups50,id.n=3) # A user friendly representation of the above

# Based on Index plot results ; delete 46,47,49,50 record
model_startups50_1<-lm(Profit~R.D.Spend+Administration+Marketing.Spend,data=startups50[-c(46,47,49,50),])
summary(model_startups50_1)
vif(model_startups50_1)
sqrt(mean(model_startups50_1$residuals^2))
hist(model_startups50_1$residuals)
plot(model_startups50_1)

# Based on Index measures results ; delete 7,38,47,49,50 record
model_startups50_2<-lm(Profit~R.D.Spend+Administration+Marketing.Spend,data=startups50[-c(7,38,47,49,50),])
summary(model_startups50_2)
vif(model_startups50_2)
sqrt(mean(model_startups50_2$residuals^2))
hist(model_startups50_2$residuals)
plot(model_startups50_2)

# Deleting Admin column
model_startups50_3<-lm(Profit~R.D.Spend+Marketing.Spend,data=startups50)
summary(model_startups50_3)
influenceIndexPlot(model_startups50_3,id.n=3)
vif(model_startups50_3)
sqrt(mean(model_startups50_3$residuals^2))
hist(model_startups50_3$residuals)
plot(model_startups50_3)

# Deleting Admin column & 46,50
model_startups50_4<-lm(Profit~R.D.Spend+Marketing.Spend,data=startups50[-c(46,50),])
summary(model_startups50_4)
influenceIndexPlot(model_startups50_4,id.n=3)
vif(model_startups50_4)
sqrt(mean(model_startups50_4$residuals^2))
hist(model_startups50_4$residuals)
plot(model_startups50_4)

##################################

# Splitting the data into Traing & Testing
install.packages("caTools")
library(caTools)
startups50_final<-startups50[-c(46,47,49,50),]
startups50_final

split<-sample.split(startups50_final$Profit,SplitRatio = 0.80)
split
table(split)
startups50.train<-subset(startups50_final,split==TRUE)
startups50.test<-subset(startups50_final,split==FALSE)

# Building model Train
model_startups50_train<-lm(Profit~R.D.Spend+Administration+Marketing.Spend,startups50.train)
summary(model_startups50_train) #R^2 = 0.9528
traing_RMSE_startups50<-sqrt(mean(model_startups50_train$residuals^2))
traing_RMSE_startups50  # 7036.531
vif(model_startups50_train)
plot(model_startups50_train)

# Predicting on Test Data

predtest_startups50<-predict(model_startups50_train,startups50.test)
predtest_startups50
startups50_test_errors<-startups50.test$Profit-predtest_startups50
startups50_test_errors

#calculate test RMSE
test_RMSE_startups50<-sqrt(mean(startups50_test_errors^2))
test_RMSE_startups50 # 6602.60

# As training & test RMSE is nerby ; we can finalise this model.
