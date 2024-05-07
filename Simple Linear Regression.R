###Simple Linear Regression

GW=read.table(file="C:\\Users\\Lenovo\\Desktop\\Archana Pachupate PC\\Praticals\\BSc 5 sem\\Paper 3\\1 st Pratical\\GroundWater (1).txt",header=TRUE)
GW
names(GW)   #Gives variable names in the data 
dim(GW)     # 1st value: No. of rows, 2nd value: No of Columns
attach(GW)  # To dettach data frame.

Y=Bicarbonate   # Response Variable
X=phWellWater   # Regressor Variable


