##Reading dataset
cars<-read.csv("Car Mileage Dataset.csv")

##install.packages("rattle")
##library(rattle)
names(cars)
names(cars)<-normVarNames(names(cars))

str(cars)

is.numeric(cars$mpg)
is.numeric(cars$displacement)
is.numeric(cars$horsepower)
is.numeric(cars$weight)
is.numeric(cars$acceleration)


cars$horsepower<-as.numeric(cars$horsepower)
cars$weight<-as.numeric(cars$weight)
cars$cylinders<-as.factor(cars$cylinders)
cars$origin<-as.factor(cars$origin)
cars$model_year<-as.factor(cars$model_year)


replace(cars$horsepower, cars$horsepower=='?', mean(cars$horsepower) )

cars_num<- subset(cars,select = c(mpg, 
                                  displacement, 
                                  horsepower, 
                                  acceleration,
                                  weight))


par(mfrow=c(2,2))
boxplot(cars_num$displacement, main="Displacement Boxplot")
boxplot(cars_num$horsepower, main="Horsepower Boxplot")
boxplot(cars_num$acceleration, main="Acceleration Boxplot")
boxplot(cars_num$weight, main="Weight Boxplot")

##Imputation
replace(cars$horsepower, cars$horsepower=='?', mean(cars$horsepower) )
replace(cars_num$acceleration, cars_num$acceleration>1.5*IQR(cars_num$acceleration) 
        & cars_num$acceleration<1.5*IQR(cars_num$acceleration), mean(cars_num$acceleration) )

summary(cars_num$acceleration)
sd(cars_num$acceleration)

##Histogram/QQ Plots
hist(cars_num$mpg)
par(mfrow=c(2,2))
hist(cars_num$displacement, main="Displacement Histogram", col="red")
hist(cars_num$horsepower, main="Horsepower Histogram", col="red")
hist(cars_num$acceleration, main="Acceleration Histogram", col="red")
hist(cars_num$weight, main="Weight Histogram", col="red")


qqnorm(cars_num$mpg)
par(mfrow=c(2,2))
qqnorm(cars_num$displacement, main="Displacement QQ Plot")
qqnorm(cars_num$horsepower, main="Horsepower QQ Plot")
qqnorm(cars_num$acceleration, main="Acceleration QQ Plot")
qqnorm(cars_num$weight, main="Weight QQ Plot")

##Correlation plot
library(corrplot)
cor_cars<-cor(cars_num)
corrplot(cor_cars, method="number")

##Create dummy variable

library(caret)
d_cylinder<-dummyVars(mpg~cylinders, data=cars)
dummy_cyl<-(predict(d_cylinder, newdata=cars))

d_origin<-dummyVars(mpg~origin, data=cars)
dummy_org<-(predict(d_origin, newdata=cars))

d_year<-dummyVars(mpg~model_year, data=cars)
dummy_year<-(predict(d_year, newdata=cars))


data<-cbind(cars_num, dummy_org,dummy_year,dummy_cyl)
head(data)

##Creating training and testing data (70:30)

library(caret)
set.seed(100)
inTrain<-createDataPartition(y=data$mpg, p=0.7, list=FALSE)
train<-data[inTrain,]
test<-data[-inTrain,]

##Model fitting

model<-lm(mpg~.,data=train)
summary(model)

##library(MASS)
##Step wise reduction

step<-stepAIC(model, direction="both")
step

model_1<-lm(formula = mpg ~ displacement + acceleration + weight + origin.1 + 
              model_year.2003 + model_year.2004 + model_year.2005 + model_year.2006 + 
              model_year.2007 + model_year.2008 + model_year.2009 + model_year.2010 + 
              model_year.2011 + model_year.2014 + cylinders.3 + cylinders.6, 
            data = train)
summary(model_1)

vif(model_1)

##
model_2<-lm(formula = mpg ~  acceleration  +  weight + origin.1 +
              model_year.2003 +  model_year.2005 + model_year.2006 + 
              model_year.2007 + model_year.2008 + model_year.2009 + model_year.2010 + 
              model_year.2011 + model_year.2014 + cylinders.3 + cylinders.6, 
            data = train)
summary(model_2)

vif(model_2)

##
model_3<-lm(formula = mpg ~  acceleration  +  weight + origin.1 +
              model_year.2003 +  model_year.2005 + model_year.2006 + 
              model_year.2007 + model_year.2008 + cylinders.3 + cylinders.6, 
            data = train)
summary(model_3)

VIF(model_3)
vif(model_3)

##
model_4<-lm(formula = mpg ~  acceleration  +  weight +
              model_year.2003 +  model_year.2005 + model_year.2006 + 
               cylinders.6, 
            data = train)
summary(model_4)

VIF(model_4)
vif(model_4)
##
model_5<-lm(formula = mpg ~ weight +
              model_year.2003 +  model_year.2005 + model_year.2006 + 
              cylinders.6, 
            data = train)
summary(model_5)

VIF(model_5)
vif(model_5)

## Prediction
pred1_5<-predict(model_5, train)
pred2_5<-predict(model_5, test)

##MAPE Calculation

MAPE<-function(actual,predicted) {
  mean(abs(actual-predicted)/actual)
}

##Training MAPE
MAPE(train$mpg,pred1_5)
##Testing MAPE
MAPE(test$mpg,pred2_5)

#Correlation
cor(test$mpg,pred2_5)
cor(test$mpg,pred2_5)^2

##Regression diagnostics
##install.packages("car")
library(car)
VIF(model)

outlierTest(model)

qqPlot(model, main="Model QQ Plot")

ncvTest(model)

summary(model$residuals)

plot(model$residuals, main="Model Residual")

qqPlot(model_5$residuals, main="Residual QQ Plot")

hist(model$residuals, main="Residual Histogram")

##Density on Histogram
x <-model_5$residuals 
h<-hist(x, breaks=10, col="red", xlab="Residuals", 
        main="Residual Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

##Normality test
shapiro.test(model_5$residuals) 
  