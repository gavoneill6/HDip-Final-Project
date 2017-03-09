rm(list=ls())
crime=read.csv('Crime - 2007-2014.csv',header=TRUE)

# this gives a list of all features and their percentage of missing values
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(crime,2,pMiss)

# Do this first! - drop the 2 variables with high proportions of missing values
crime = crime[,c(-19,-20)]

# plot of correlations
install.packages("corrplot")
library(corrplot)
# remove dimensions, categorical and unwanted variables
crimeCorr = crime[,c(-1,-2,-3,-4,-21)]
viewcorr = crimeCorr
M <- cor(viewcorr)
corrplot(M, method="circle")
corrplot.mixed(M)

# historgrams to check distribution & log transformation
hist(crime$No_of_Offences, prob=T,xlab="No of Offences", 
     ylab="Frequency",main='Histogram of No of Offences')
crime$No_of_Offences = log10(crime$No_of_Offences +1)

hist(crime$Population_Num, prob=T,xlab="Population", 
     ylab="Frequency",main='Histogram of Population Number')
crime$Population_Num = log10(crime$Population_Num)

hist(crime$Live_Register_Num, prob=T,xlab="Live Register Num", 
     ylab="Frequency",main='Histogram of Live Register Num')
crime$Live_Register_Num = log10(crime$Live_Register_Num)

hist(crime$Mean_Disposable_Income, prob=T,xlab="Mean Disp Income", 
     ylab="Frequency",main='Histogram of Mean Disposable Income')
crime$Mean_Disposable_Income = log10(crime$Mean_Disposable_Income)

hist(crime$Rent_Rates, prob=T,xlab="Rent Rates", 
     ylab="Frequency",main='Histogram of Rent Rates')
crime$Rent_Rates = log10(crime$Rent_Rates)

hist(crime$CPI_All_Items, prob=T,xlab="CPI", 
     ylab="Frequency",main='Histogram of CPI All Items')

hist(crime$GBP_per_Euro, prob=T,xlab="GPB per Eur", 
     ylab="Frequency",main='Histogram of GPB per Eur')
crime$GBP_per_Euro = log10(crime$GBP_per_Euro +1)

hist(crime$Consistent_Poverty_Rate, prob=T,xlab="Consistent Poverty Rate", 
     ylab="Frequency",main='Histogram of Consistent Poverty Rate')
crime$Consistent_Poverty_Rate = log10(crime$Consistent_Poverty_Rate)

hist(crime$Daily_Internet_Users, prob=T,xlab="Internet Users", 
     ylab="Frequency",main='Histogram of % Daily Internet Users')
crime$Daily_Internet_Users = log10(crime$Daily_Internet_Users)

hist(crime$Pop_In_Good_Health, prob=T,xlab="Pop in Good Health", 
     ylab="Frequency",main='Histogram of Pop in Good Health')
crime$Pop_In_Good_Health = log10(crime$Pop_In_Good_Health +1)


# try some different models
model1 = crime[,c(-1,-2,-3,-4,-11,-13,-14,-15,-16,-19,-20,-21,-22,-23)] # remove columns
model2 = model1[,c(-4,-6,-7,-8)]
model3 = model2[,c(-2)]

# outlier detection
source("http://goo.gl/UUyEzD")
outlierKD(model1, Pop_In_Good_Health)

# try Stepwise Regression
library(MASS)
fit <- lm(No_of_Offences~.,data=model1)
step <- stepAIC(fit, direction="both")
step$anova # display results

#MLR modelling
m1=lm(No_of_Offences~. -1,data=model1)
plot(model1$No_of_Offences,predict(m1,data=model1),xlab="Real Crime", ylab="Predicted Crime")
abline(0,1)
summary(m1)
plot(m1)
rms=sqrt((sum((crime$No_of_Offences-predict(m1,data=crime))^2))/length(crime$No_of_Offences))

m2=lm(No_of_Offences~. -1,data=model2)
plot(model2$No_of_Offences,predict(m2,data=model2),xlab="Real Crime", ylab="Predicted Crime")
abline(0,1)
summary(m2)
plot(m2)
rms=sqrt((sum((crime$No_of_Offences-predict(m2,data=crime))^2))/length(crime$No_of_Offences))

m3=lm(No_of_Offences~. -1,data=model3)
plot(model3$No_of_Offences,predict(m3,data=model3),xlab="Real Crime", ylab="Predicted Crime")
abline(0,1)
summary(m3)
plot(m3)
rms=sqrt((sum((crime$No_of_Offences-predict(m3,data=crime))^2))/length(crime$No_of_Offences))


# setting seed to reproduce results of random sampling
set.seed(100)
# row indices for training data
trainingRowIndex <- sample(1:nrow(model3), 0.8*nrow(model3))
trainingData <- model3[trainingRowIndex, ]  # model training data
testData  <- model3[-trainingRowIndex, ]   # test data

lmModel3 <- lm(No_of_Offences~. -1, data=trainingData)  # build the model
offencesPred <- predict(lmModel3, testData)  # predict No_of_Offences

summary(lmModel3)
AIC(lmModel3)

# check actuals vs predicted values
actuals_preds <- data.frame(cbind(actual=testData$No_of_Offences, predicted=offencesPred))
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)

# k-fold cross validation
install.packages('DAAG')
library(DAAG)
cvResults <- suppressWarnings(CVlm(model3, form.lm=No_of_Offences~. -1, m=10, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
attr(cvResults, 'ms')


# editing tools:
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
