#CART

#Installing packages
install.packages("rpart")
install.packages("rpart.plot")
install.packages("datasets")
install.packages("C50")
install.packages("xlsx")
install.packages("tree")
library(tree)
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)


#Import all the libraries
library(rpart)
library(rpart.plot)
library(datasets)
library(xlsx)
require(rJava)
require(xlsxjars)
require(C50)



#Load Patient Data Set
patientData<-read.xlsx("C:/Users/TestUpdatedversion2.xlsx",sheetName="Sheet1")

View(patientData)


subdata<-patientData[(3:39)]
View(subdata)


partdata<-data.frame(subdata$State,subdata$Weight..Lb.,subdata$Age..Yrs.,subdata$Obesity)
View(partdata)


#C50 Algorithm for decision of Obesity factor in States 
treec5<-C5.0(partdata[1:10,2:4],subdata[1:10,1])
plot(treec5)













##bar plots
counts <- table(patientData$Type3, patientData$State)
barplot(counts, main="Ratio of Type 3 PCOS patients in states",
        xlab="STATE wise TYPE 3 PCOS PATIENTS", col=c("yellow","dark blue"),
        legend = rownames(counts))



#Initialize Random seed
set.seed(123)

prcomp(patientData)








##First We use decision tree to determine behaviour between the 'Tests' performed for the Symptoms and their corresponding FACTORS 

#CART for Mild Insulin Resistance factor

mir<-rpart(Mild.Insulin.Rsistance~Insulin.Glucose, data=patientData, method="class",control=rpart.control(minsplit = 5,cp=0.01))
printcp(mir)
plot(mir, uniform = TRUE, main="Classification for Presence/Absence of Mild Insulin Resistance")

text(mir, cex = 0.8, use.n = TRUE, xpd = TRUE,all=TRUE)

#CART for Hirsutism factor

hir<-rpart(Hirsutism.1~DHEA.S, data=patientData, method="class",control=rpart.control(minsplit = 5,cp=0.01))
printcp(hir)
plot(hir, uniform = TRUE, main="Classification for Presence/Absence of HIRSUTISM ")
text(hir, cex = 0.8, use.n = TRUE, xpd = TRUE,all=TRUE)












##CART is now implemented to create decision trees for the Type categorization based on the determinant factors


#CART for Type1 decision tree
a<-rpart(Type1~Anovulatory+Increased.Androgen+No.Insulin.Resistance+Ovulatory+Normal.Androgen+Insulin.Rsistance+Mild.Insulin.Rsistance+Hirsutism.1, data=patientData, method="class",control=rpart.control(minsplit = 1,cp=0.01))
printcp(a)
plot(a, uniform = TRUE, main="Classification for Type 1")
text(a, cex = 0.8, use.n = TRUE, xpd = TRUE,all=TRUE)
table(predict(a, type="class"))
summary(tree(a))



##CART for Type2 decision tree

b<-rpart(Type2~Anovulatory+Increased.Androgen+Insulin.Rsistance, data=patientData, method="class",control=rpart.control(minsplit = 1,cp=0.01))
printcp(b)
plot(b, uniform = TRUE, main="Classification for Type 2")
text(b, cex = 0.8, use.n = TRUE, xpd = TRUE,all=TRUE)

summary(tree(b))



##CART for Type3 decision tree

c<-rpart(Type3~Anovulatory+Normal.Androgen+Obesity+Insulin.Rsistance, data=patientData, method="class",control=rpart.control(minsplit = 1,cp=0.01))
printcp(c)
plot(c, uniform = TRUE, main="Classification for Type 3")
text(c, cex = 0.8, use.n = TRUE, xpd = TRUE,all=TRUE)



##CART for Type4 decision tree

d<-rpart(Type4~Ovulatory+Increased.Androgen+Mild.Insulin.Rsistance, data=patientData, method="class",control=rpart.control(minsplit = 1,cp=0.01))
printcp(d)
plot(d,uniform = TRUE, main="Classification for Type 4")
text(d, cex = 0.8, use.n = TRUE, xpd = TRUE,all=TRUE)






#CART for Type5 decision tree

y<-rpart(Type5~Ovulatory+Increased.Androgen+No.Insulin.Resistance+Hirsutism.1, data=patientData, method="class",control=rpart.control(minsplit = 1,cp=0.01))
printcp(y)
plot(y,uniform = TRUE, main="Classification for Type 5")
text(y, cex = 0.8, use.n = TRUE, xpd = TRUE, all=TRUE)












#Principal Component Analysis

install.packages("Matrix")


#Log Transform
log.pat<-log(patientData[,4:25]);
pat.factor<-(patientData[15]);
pat.pca<-prcomp(patientData[15:24])

summary(patientData)

#Apply PCA-

pat.pca<-prcomp(patientData,center = TRUE,scale= TRUE);

#Print function
print(pat.pca)



#plot function returns a plot ofthe variances (y-axis) associated with the PCs(x-axis)
#The result figure helps determine how many PCs to retain for furthur analysis
#We can see the first two PCs explain most of the variability in the data

#Plot Function
plot(ir.pca, type = "lines")

#Summary Function
summary(ir.pca)


#Predict PCs
predict(ir.pca,
        newdata=tail(log.ir,2))


install.packages("devtools")
library(devtools)
install_github("ggbiplot","vqv")
force=TRUE
library(ggbiplot)
g<-ggbiplot(ir.pca,obs.scale = 1,var.scale = 1,groups=ir.species,ellipse=TRUE,
            circle=TRUE)
g<-g+scale_color_discrete(name='')
g<-g+theme(legend.direction = 'horizontal',
           legend.position ='top')
print(g)





