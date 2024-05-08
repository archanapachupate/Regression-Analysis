library(faraway)

CD=cheddar

names(CD)
View(CD)

##Model fitting
MLRM2=lm(taste~Acetic+H2S+Lactic, data=CD)
MLRM2
#Fitted multiple regression model is Y= -28.8768+0.3277*Acetic+3.9118*H2S+19.6705*Lactic


##Summary / Contribution of independent variables
summary(MLRM2)
cat("At 5% level of significance both H2S and Lactic acid contribute significantly in the regression model\n")


# Interpretation of R2 and Adj. R2 is same.
Rsq=0.6518
Adjusted_Rsq=0.6116 
cat("The value of R-square for this model is:", Rsq,"\n")
cat("Interpretation:", Rsq*100, "% variation in response variable taste is explained by the regressor variable Acetic acid,H2S and lactic acid\n")


##Confidence Intervals
confint(MLRM2,level=.95)

##Plot
plot.new()
par(mfrow=c(2,2))
plot(MLRM2)
cat("In plot 1 indicates residuals form a horizontal band hence it indicates that model is adequate or appropriate\n")
cat("In plot 2 indicates errors come from normal distribution\n")
  

library(MASS)


####hat matrix
CD1=cbind(Int=1,CD[,-1])
View(CD1)
class(CD1)
CD1=X
H=X%*%solve(t(X)%*%X)%*%t(X)

X=as.matrix(X)
View(H)

H[1:2,}

h=diag(H)
h1=1-h

A=cheddar$Acetic
B=cheddar$H2S
C=cheddar$Lactic

Y=cheddar$taste
Y_hat= -28.8768+0.3277*A+3.9118*B+19.6705*C

ei=Y-Y_hat
anova(MLRM2)
MSres=102.63 

#Studentised residual
r=ei/sqrt(MSres*h1)
r
plot(r)
#8th and 15th oservation -1.71919258 , 2.51291568 are outliers


#standardised residual
di=ei/sqrt(MSres)
di
plot(di)
#8th and 15th oservation -1.71919258 , 2.51291568 are outliers



#PRESS Residual
p=ei/(h1)
pr=p^2
pr
press=sum(pr)
press

# standard, studentized and press residual

#studentized residual
library(MASS)
studentized_res=studres(MLRM2)
studentized_res
plot(studentized_res)
 
#Only 15th residual seems a have a very large value compared to others, hence it is an outlier

#standardized residual

standardized_res=rstandard(MLRM2)
standardized_res
plot(standardized_res)

 
#Here again the 15th residual is an outlier 

#PRESS residual

PRESS=function(MLRM2){
i=residuals(MLRM2)/(1-lm.influence(MLRM2)$hat)
sum(i^2)
}

PRESS(MLRM2)




