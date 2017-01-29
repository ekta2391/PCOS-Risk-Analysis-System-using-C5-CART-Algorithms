#Apriori Algorithm

library("arules");

library("arulesViz");

library(datasets)


install.packages("RWeka")
install.packages("xlsx")
# load the package
library(RWeka)
library(xlsx)
require(rJava)
require(xlsxjars)
require(C50)

patientData<-read.xlsx("C:/Users/TestUpdated.xlsx",sheetName="Sheet1")
View(patientData)
summary(patientData)


itemFrequencyPlot(adata,topN=10,type="absolute")
str(patientData)

adata<-patientData[(1:4)]
View(adata)
adata


rules<-apriori(adata)

