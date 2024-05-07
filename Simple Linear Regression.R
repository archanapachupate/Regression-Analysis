###Simple Linear Regression
##GroundWater dataset

GW=read.table(file="C:\\Users\\Lenovo\\Desktop\\Archana Pachupate PC\\Praticals\\BSc 5 sem\\Paper 3\\1 st Pratical\\GroundWater (1).txt",header=TRUE)
GW
names(GW)   #Gives variable names in the data 
dim(GW)     # 1st value: No. of rows, 2nd value: No of Columns
attach(GW)  # To dettach data frame.

Y=Bicarbonate   # Response Variable
X=phWellWater   # Regressor Variable



####Correlation

plot(X,Y)   # There is negative relationship between X and Y.

cor(X,Y)    # There is considerable correlation between PH well water and Bicarbonate


### Regession Model 

# By using R command lm():

LM=lm(Y~X)     # Direct R command to fit linear regression model.
LM
summary(LM)    # Gives us estimated regression coefficients, their standard erros, tabulated values pvalue to check the significance of intercept and slope parameters. 


# Without uisng R command lm():
X=phWellWater  

Y=Bicarbonate  
xbar=mean(X)     # Mean of regressor variable
ybar=mean(Y)     # Mean of response variable


Sxy= sum((X-xbar)*(Y-ybar))    # Sxy
Sxy

Sxx=sum((X-xbar)*(X-xbar))     # Sxx
Sxx

beta1_hat=Sxy/Sxx              # Estimated value of beta1 (i.e. slope parameter)
beta1_hat


beta0_hat=ybar-beta1_hat*xbar  # Estimated value of beta0 (i.e. Intercept parameter)
beta0_hat

# Fitted Regression Model is: Yhat= 432.1472-37.76585*X

#Interpretation of slope parameter:
# If pH of well water increased by one unit, then Bicarbonate value is decreasing by 37.76585 units. 

# Overall Conclusion about the regression model:

#1) The Fitted Regression model is obtained as below:

#Yhat= 432.1472-37.76585*X

#2) From summary of regression model, we can conclude that; 
     


	#2.1)  To test, H0: beta0= 0 vs H1: beta0 != 0

		#As P-value(0.00465) < alpha(0.05), 
		# Decision: Reject Ho at 5% level of significance
		# Conclusion: Beta0 != 0 and it is contributing in the regression model.

	#2.2)  To test, H0: beta1= 0 vs H1: beta1 != 0

		#As P-value(0.04948) < alpha(0.05), 
		# Decision: Reject Ho at 5% level of significance
		# Conclusion: Beta1 != 0. It means, corresponding regressor variable is contributing significantly in the regression model. 

	#2.3) Multiple R-squared:  0.1153
		# Conclusion: Only 11.53 % variation in the response variable Bicarbonate is explained by the regressor variable pH well water. It means, we need to add few more variables in the model. 






###Pima dataset 
library(faraway)               # Calling package faraway for this R session.
  
PM=pima           		
dim(PM)            	   #  768 observations on female pima indians and 9 variables. 
apply(PM,2,class)
head(PM)                   #Gives first 6 observations in the dataset

##Numerical Summary of data

# While computing numerical summary, I have removed variable 'test' beacuse it is categorical variable. 
# test whether the patient shows signs of diabetes (coded 0 if negative, 1 if positive) 

summary(PM[,-9])   # Numerical Summary of Data

# Conclusion: 1) For variable insulin, it is observe that there is large difference between mean and median and Median< mean, it means that data on insulin is from positively skewed distribution. 
# 		  2) For other variables, distribution is more or less symmetric. We can confirm it with measures of skewness. 


var(PM[,-9]) # Variance- Covariance martix

# Graphical summary of data

plot(PM[,-9])   # There is positve relationship between triceps and BMI

# Conclusion: from scatterplots, it seems that most of the variables are positively related to each other but there is no high correlation among them. same can be confirmed by computing correlation values. 

###Correlation

cor(PM[,-9])   # Comment: There is significant correlation between follwoing pairs: 1)  glucose- Insulin, triceps- Insulin, triceps-bmi, pregnant-age. 


###Model fitting 

attach(PM)   # To dettach data frame.
x=triceps    # Regressor variable
y=insulin    # Response variable
LM=lm(y~x)   # Regresion model using 'lm()' command in R.
LM

# Fitting model witout using R command 'lm()'.
x=triceps    # Regressor variable
y=insulin    # Response variable


xbar=mean(x)    
ybar=mean(y)

Sxx=sum((x-xbar)*(x-xbar))
Sxx

Sxy=sum((x-xbar)*(y-ybar))
Sxy
beta1_hat=Sxy/Sxx
beta1_hat           # Slope Parameter

beta0_hat=ybar-beta1_hat*xbar
beta0_hat         # Intercept Parameter

summary(LM)

# Fitted Regression Model is: Yhat=  14.99752+3.155459*X

#Interpretation of slope parameter:
     # If  value of triceps increased by one unit, then insulin value is increasing by 3.155459 units. 

# Overall Conclusion about the regression model:

#1) The Fitted Regression model is obtained as below:

#Yhat=  14.99752+3.155459*X

#2) From summary of regression model, we can conclude that; 
     

	#2.1)  To test, H0: beta0= 0 vs H1: beta0 != 0

		#As P-value(0.0142) < alpha(0.05), 
		# Decision: Reject Ho at 5% level of significance
		# Conclusion: Beta0 != 0 and it is contributing in the regression model.

	#2.2)  To test, H0: beta1= 0 vs H1: beta1 != 0

		#As P-value(<2e-16 ) < alpha(0.05), 
		# Decision: Reject Ho at 5% level of significance
		# Conclusion: Beta1 != 0. It means, corresponding regressor variable is contributing significantly in the regression model. 

	#2.3) Multiple R-squared:  0.1908
		# Conclusion: Only 19.08 % variation in the response variable insulin is explained by the regressor variable triceps. It means, we need to add few more variables in the model. 






####Cheddar Dataset
CD=cheddar
CD
View(CD)
attach(CD)     # to dettach data frame

names(CD)     #Gives variable names in the data 


Y=taste      # Response variable
X1=Acetic    # Regressor Variable
X2=H2S       # Regressor Variable
X3=Lactic    # Regressor Variable

length(Y)
length(X1)

# SLRM by considering Taste as response variable and Acetic as regressor variable.

LM1=lm(Y~X1)      # By using R command lm().
summary(LM1)
anova(LM1)


# SLRM without using R command lm():

x1bar=mean(X1)
ybar=mean(Y)
Sxx=sum((X1-x1bar)^2)
Sxy=sum((X1-x1bar)*(Y-ybar))

beta1_hat=Sxy/Sxx

beta0_hat=ybar-beta1_hat*x1bar

beta1_hat     # Slope Parameter
beta0_hat     # Intercept Parameter

# Fitted Regression Model is obtained as: 

Yhat=-61.49861+15.64777*X1 

# UBE of Error Variance: 

Res1=Y-Yhat  		      # Residual= Observed value - Fitted Value
Res1

SSRes1=sum((Y-Yhat)^2) 		# Sum of Square due to residuals
SSRes1=sum(Res1^2)             # same as above
SSres1
	
n=length(Y)
MSRes1=SSRes1/(n-2)             # SSRes/ df(error)
MSRes1   				# MSRes is an UBE of sigma^2


#b) 

# To test Ho: beta1 =0 Vs H1: beta1 != 0
alpha=0.05
beta1=0
var_beta1_hat=MSRes1/Sxx   
SE_beta1_hat=sqrt(var_beta1_hat)

T=(beta1_hat-beta1)/SE_beta1_hat     # Value of test statstic
T

#Criteria 1: P value
 
Pvalue=2*(1-pt(q=T,df=length(Y)-2))
Pvalue

#Decision: Pvalue< alpha, Reject Ho. 
#Conclusion: Regressor variable X1 is significantly contributing in the regression model.

#### OR we can try if()...else to print output automatically.


if(Pvalue< alpha)
{
cat("P Value:",Pvalue, "\n") 
cat("Alpha:", alpha, "\n")
cat("P-value < Alpha")
cat("Decision: Reject Ho at",alpha*100, "% level of significance\n")
cat("Conclusion: Regressor variable Acetic is contributing significantly in the regression model\n")
}else
{
cat("P Value:",Pvalue, "\n") 
cat("Alpha:", alpha, "\n")
cat("P-value > Alpha")
cat("Decision: Accept Ho at",alpha*100, "% level of significance\n")
cat("Conclusion: Regressor variable Acetic is not contributing significantly in the regression model\n")
}


# Criteria 2: Calculated value

T_cal= T
T_cal
T_tab= qnorm(0.975,0,1)   # tabulated value at 5 % l.o.s.
T_tab



if(T_cal > T_tab)
{
cat("T calculated:",T_cal, "\n") 
cat("T tabulated:", T_tab, "\n")
cat("T_cal > T_tab")
cat("Decision: Reject Ho at",alpha*100, "% level of significance\n")
cat("Conclusion: Regressor variable Acetic is contributing significantly in the regression model\n")
}else
{
cat("T calculated:",T_cal, "\n") 
cat("T tabulated:", T_tab, "\n")
cat("T_cal < T_tab")
cat("Decision: Accept Ho at",alpha*100, "% level of significance\n")
cat("Conclusion: Regressor variable Acetic is not contributing significantly in the regression model\n")
}


# SLRM by considering Taste as response variable and H2S as regressor variable.

 # By using R command lm().

LM2=lm(Y~X2)
summary(LM2)
anova(LM2)

# SLRM without using R command lm():

x2bar=mean(X2)
ybar=mean(Y)
Sxx=sum((X2-x2bar)^2)
Sxy=sum((X2-x2bar)*(Y-ybar))

beta1_hat=Sxy/Sxx

beta0_hat=ybar-beta1_hat*x2bar

beta1_hat     # Slope Parameter
beta0_hat     # Intercept Parameter

# Fitted Regression Model is obtained as: 

Yhat=-9.786837+ 5.776089*X2

# UBE of Error Variance: 

Res2=Y-Yhat  		      # Residual= Observed value - Fitted Value

SSRes2=sum((Y-Yhat)^2) 		# Sum of Square due to residuals

SSRes2=sum(Res2^2) 			# same as above
n=length(Y)
MSRes2=SSRes2/(n-2)             # SSRes/ df(error)
MSRes2  				# MSRes is an UBE of sigma^2

#b) 

# To test Ho: beta1 =0 Vs H1: beta1 != 0
alpha=0.05
beta1=0
var_beta1_hat=MSRes2/Sxx   
SE_beta1_hat=sqrt(var_beta1_hat)

T=(beta1_hat-beta1)/SE_beta1_hat     # Value of test statstic
T

#Criteria 1: P value
 
Pvalue=2*(1-pt(q=T,df=length(Y)-2))
Pvalue

#Decision: Pvalue< alpha, Reject Ho. 
#Conclusion: Regressor variable X2 is significantly contributing in the regression model.

#### OR we can try if()...else to print output automatically.


if(Pvalue< alpha)
{
cat("P Value:",Pvalue, "\n") 
cat("Alpha:", alpha, "\n")
cat("P-value < Alpha")
cat("Decision: Reject Ho at",alpha*100, "% level of significance\n")
cat("Conclusion: Regressor variable Acetic is contributing significantly in the regression model\n")
}else
{
cat("P Value:",Pvalue, "\n") 
cat("Alpha:", alpha, "\n")
cat("P-value > Alpha")
cat("Decision: Accept Ho at",alpha*100, "% level of significance\n")
cat("Conclusion: Regressor variable Acetic is not contributing significantly in the regression model\n")
}


# Criteria 2: Calculated value

T_cal= T
T_cal
T_tab= qnorm(0.975,0,1)   # tabulated value at 5 % l.o.s.
T_tab


if(T_cal > T_tab)
{
cat("T calculated:",T_cal, "\n") 
cat("T tabulated:", T_tab, "\n")
cat("T_cal > T_tab")
cat("Decision: Reject Ho at",alpha*100, "% level of significance\n")
cat("Conclusion: Regressor variable Acetic is contributing significantly in the regression model\n")
}else
{
cat("T calculated:",T_cal, "\n") 
cat("T tabulated:", T_tab, "\n")
cat("T_cal < T_tab")
cat("Decision: Accept Ho at",alpha*100, "% level of significance\n")
cat("Conclusion: Regressor variable Acetic is not contributing significantly in the regression model\n")
}

# SLRM by considering Taste as response variable and Lactic as regressor variable.

 # By using R command lm().

LM3=lm(Y~X3)
summary(LM3)
anova(LM3)

# SLRM without using R command lm():

x3bar=mean(X3)
ybar=mean(Y)
Sxx=sum((X3-x3bar)^2)
Sxy=sum((X3-x3bar)*(Y-ybar))

beta1_hat=Sxy/Sxx

beta0_hat=ybar-beta1_hat*x3bar

beta1_hat     # Slope Parameter
beta0_hat     # Intercept Parameter

# Fitted Regression Model is obtained as: 

Yhat= -29.85883+37.71995*X3

# UBE of Error Variance: 

Res3=Y-Yhat  		      # Residual= Observed value - Fitted Value

SSRes3=sum((Y-Yhat)^2) 		# Sum of Square due to residuals

SSRes3=sum(Res3^2) 			# same as above
n=length(Y)
MSRes3=SSRes3/(n-2)             # SSRes/ df(error)
MSRes3  				# MSRes is an UBE of sigma^2

#b) 

# To test Ho: beta1 =0 Vs H1: beta1 != 0
alpha=0.05
beta1=0
var_beta1_hat=MSRes3/Sxx   
SE_beta1_hat=sqrt(var_beta1_hat)

T=(beta1_hat-beta1)/SE_beta1_hat     # Value of test statstic
T

#Criteria 1: P value
 
Pvalue=2*(1-pt(q=T,df=length(Y)-2))
Pvalue

#Decision: Pvalue< alpha, Reject Ho. 
#Conclusion: Regressor variable X3 is significantly contributing in the regression model.

#### OR we can try if()...else to print output automatically.


if(Pvalue< alpha)
{
cat("P Value:",Pvalue, "\n") 
cat("Alpha:", alpha, "\n")
cat("P-value < Alpha")
cat("Decision: Reject Ho at",alpha*100, "% level of significance\n")
cat("Conclusion: Regressor variable Acetic is contributing significantly in the regression model\n")
}else
{
cat("P Value:",Pvalue, "\n") 
cat("Alpha:", alpha, "\n")
cat("P-value > Alpha")
cat("Decision: Accept Ho at",alpha*100, "% level of significance\n")
cat("Conclusion: Regressor variable Acetic is not contributing significantly in the regression model\n")
}


# Criteria 2: Calculated value

T_cal= T
T_cal
T_tab= qnorm(0.975,0,1)   # tabulated value at 5 % l.o.s.
T_tab


if(T_cal > T_tab)
{
cat("T calculated:",T_cal, "\n") 
cat("T tabulated:", T_tab, "\n")
cat("T_cal > T_tab")
cat("Decision: Reject Ho at",alpha*100, "% level of significance\n")
cat("Conclusion: Regressor variable Acetic is contributing significantly in the regression model\n")
}else
{
cat("T calculated:",T_cal, "\n") 
cat("T tabulated:", T_tab, "\n")
cat("T_cal < T_tab")
cat("Decision: Accept Ho at",alpha*100, "% level of significance\n")
cat("Conclusion: Regressor variable Acetic is not contributing significantly in the regression model\n")
}




# C) 

#Compute R^2 for Y~X1

SST=sum((Y-ybar)^2)
SST


R2=1-(SSRes1/SST)
R2

cat("The value of R^2 is",R2,"\n")
cat("Conclusion:", R2*100, "% variation in the response variable is explained by the regressor variable Acetic\n" )


#Compute R^2 for Y~X2



SST=sum((Y-ybar)^2)
SST


R2=1-(SSRes2/SST)
R2

cat("The value of R^2 is",R2,"\n")
cat("Conclusion:", R2*100, "% variation in the response variable is explained by the regressor variable Acetic\n" )



#Compute R^2 for Y~X3

SST=sum((Y-ybar)^2)
SST


R2=1-(SSRes3/SST)
R2

cat("The value of R^2 is",R2,"\n")
cat("Conclusion:", R2*100, "% variation in the response variable is explained by the regressor variable Acetic\n" )


anova(LM1)







