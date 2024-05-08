
##Gasoline Dataset
GS=read.table(file="C:\\Users\\Lenovo\\Downloads\\gasoline.txt")
View(GS)
dim(GS)
GS1=na.omit(GS)
View(GS1)

#Fit Multiple linear regression model
names(GS1)
LM1=lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+I(x11), data=GS1) # Full model: including regressors in the dataset
LM1


#Summary / Contribution of independent variables
summary(LM1)

#X11 is not contributing significantly in the regression model

#Plot
par(mfrow=c(2,2))
plot(LM1)
#Coclusion= Residuals vs fitted graph is U shaped indicate that the assumption of constant variance is violated.
#Normal Q-Q plot indicate distribution does not come from normal distribution . May be come from Positively Skew distribution.

#Correlation
cor(GS1)
#There is high correlation between  regressor variables.For example X2 and X3
#Which lead to the multicollinearity

#Checking Multicolinearity
library(faraway)
vif(LM1)
#Conclusion= VIF value indicate that there is mullticollinearity in the model.

#Feature Selection
#Forward Selection
library(MASS)
NL=lm(y~1,data=GS1)   # Null Model/ Intercept model
NL
FS=step(NL,scope=list(upper=~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+I(x11),lower=~1 ),direction="forward")


#Backward Elimination
BE=step(LM1,scope=list(upper=~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+I(x11),lower=~1 ),direction="backward")


#Stepwise 
SS=step(LM1,scope=list(upper=~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+I(x11),lower=~1 ),direction="both")

#i)
#Forward Selection Model is y=17.339838-0.075588x1+1.494737x4
#Backward Elemination Model is y=17.339838+5.843495x5+0.180811x8-0.005115x10
#Stepwise Regression Model is y=17.339838+5.843495x5+0.180811x8-0.005115x10

#Conclusion=In Forward Selection Model x1 and x4 regressor are significantly contributed in the model while in Backward Elemination Model and Stepwise Regression Model x5,x8 and x10 regressor are significantly contributed in the model .

#
vif(FS)
vif(BE)
vif(SS)
#If we compare the vif in all three model then  the Forward Selection Model x1 and x4 regressor are preffered while in Backward Elemination Model and Stepwise Regression Model x5,x8 and x10 regressor are prefeered in the model .


####National Football League Dataset

NFL=read.table(file="C:\\Users\\Lenovo\\Downloads\\NationalFootballLeague.txt")
View(NFL)
dim(NFL)


#Fit Multiple linear regression model

names(NFL)
LM1=lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9, data=NFL) # Full model: including regressors in the dataset

LM1



#Summary / Contribution of independent variables
summary(LM1)

#X2 is  contributing significantly in the regression model at 1% level of significance while X8 is  contributing significantly in the regression model at 10% level of significance 


#Plot
par(mfrow=c(2,2))
plot(LM1)
#Coclusion= Residuals vs fitted graph  indicate that the assumption of constant variance is violated.
#Normal Q-Q plot indicate that the observations are from a distribution having tails thinner than normal distribution.

#Correlation
cor(NFL)
#There is high correlation between  regressor variables.For example X1 and X7
#Which lead to the multicollinearity


#Checking Multicolinearity
library(faraway)
vif(NFL)


#Feature Selection
#Forward Selection
library(MASS)
NL=lm(y~1,data=NFL)   # Null Model/ Intercept model
NL
FS=step(NL,scope=list(upper=~x1+x2+x3+x4+x5+x6+x7+x8+x9,lower=~1 ),direction="forward")


#Backward Elimination
BE=step(LM1,scope=list(upper=~x1+x2+x3+x4+x5+x6+x7+x8+x9,lower=~1 ),direction="backward")


#Stepwise
SS=step(LM1,scope=list(upper=~x1+x2+x3+x4+x5+x6+x7+x8+x9,lower=~1 ),direction="both")

#i)

#Forward Selection Model is y= -7.292e+00+3.631e-03x2+1.544e-01x7-3.895e-03x8-1.791e-03x9
#Backward Elemination Model is  y= -7.292e+00+3.631e-03x2+1.544e-01x7-3.895e-03x8-1.791e-03x9
#Stepwise Regression Model is  y= -7.292e+00+3.631e-03x2+1.544e-01x7-3.895e-03x8-1.791e-03x9

#Conclusion=In Forward Selection Model,Backward Elemination Model and Stepwise Regression Model x2,x7,x8 and x9 regressor are significantly contributed in the model .

#
vif(FS)
vif(BE)
vif(SS)
#If we compare the vif in all three model then  the Forward Selection Model ,Backward Elemination Model and Stepwise Regression Model  x2,x7,x8 and x9 regressor are prefeered in the model .











