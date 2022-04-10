library (Hmisc) 
library(corrplot)
setwd("C:/Users/torrr/OneDrive/Desktop/EDA HW/Data")

SKUMaster<-read.csv("SKU Master.csv") #Write in .csv files
#Filter according to UOM Cube 
SKUMaster$Uom <- as.factor(SKUMaster$Uom)
SKUMaster <- SKUMaster[SKUMaster$UomCube>0,]
SKUMaster <- SKUMaster[SKUMaster$UomCube<2,]
#Filter according to UOM Weight 
SKUMaster<-SKUMaster[SKUMaster$UomWeight>0,]
SKUMaster<-SKUMaster[SKUMaster$UomWeight<50,]
#Filter by UOM & drop all NAs 
SKUMaster<-SKUMaster[SKUMaster$Uom %in% c("CA","EA","PL","LB"),]
SKUMaster<-na.omit(SKUMaster)
SKUMaster<-droplevels(SKUMaster)


#Question 1:Filter the dataframe to keep only the variables UnitsPerCase,
#LeadTime, UoMCube, UoMWeight, and ShelfLifeDays
SKUMaster2<-SKUMaster[,c(6,7,8,9,11)]
SKUMaster2


#Question 2:Create a correlation plot for all the variables.  Show the numerical
#correlation on this plot and keep only the upper portion
cor(SKUMaster2) 
corrplot::corrplot(cor(SKUMaster2), type = "upper", method = "number")


#Question 3:Test for the significance of all correlations 
Hmisc::rcorr(as.matrix(SKUMaster2))


#Question 4:Examine variables UoMWeight and ShelfLifeDays for a linear 
#relationship (Pearsons) and test for the significance of this correlation.  
plot(SKUMaster2$UomWeight,SKUMaster2$ShelfLifeDays)
cor(SKUMaster2$UomWeight, SKUMaster2$ShelfLifeDays)
cor.test(SKUMaster2$UomWeight, SKUMaster2$ShelfLifeDays)


#Question 5:Examine variables UoMWeight and ShelfLifeDays for a monotonic
#relationship (Spearmans) and test for the significance of this correlation.  
plot(SKUMaster2$UomWeight,SKUMaster2$ShelfLifeDays)
cor(SKUMaster2$UomWeight, SKUMaster2$ShelfLifeDays, method="spearman")
cor.test(SKUMaster2$UomWeight, SKUMaster2$ShelfLifeDays,method ="spearman")


#Question 7:Examine variables UoMWeight and UoMCube for a linear relationship 
#(Pearsons) and test for the significance of this correlation.  
plot(SKUMaster2$UomWeight, SKUMaster2$UomCube)
cor(SKUMaster2$UomWeight,SKUMaster2$UomCube)
cor.test(SKUMaster2$UomWeight, SKUMaster2$UomCube)


#Question 8:Examine variables UoMWeight and UoMCube for a monotonic relationship 
#(Spearmans) and test for the significance of this correlation.  
plot(SKUMaster2$UomWeight,SKUMaster2$UomCube)
cor(SKUMaster2$UomWeight, SKUMaster2$UomCube, method="spearman")
cor.test(SKUMaster2$UomWeight, SKUMaster2$UomCube,method ="spearman")
