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
SKUMaster$Flow<- as.character(SKUMaster$Flow)
SKUMaster$Whs<-as.character(Whs)
SKUMaster$Uom<-as.character(Uom)
Main<-SKUMaster[,c(3,4,5,6,7,8,9,11)]
library(greybox)

#Question 1 
tableplot(SKUMaster$Flow,SKUMaster$Uom)
greybox::cramer(SKUMaster$Flow,SKUMaster$Uom)

tableplot(SKUMaster$Flow,SKUMaster$Whs)
greybox::cramer(SKUMaster$Flow,SKUMaster$Whs)

#Question 2
NewSku <- data.frame(SKUMaster$Whs,SKUMaster$Flow,SKUMaster$Uom)
assoc(NewSku)
spread(NewSku)

#Question 3: 1st- Whs & ShelfLifeDays(.5188)  2nd- Uom & ShelfLifeDays(.3874)
assoc(Main)
spread(Main)
