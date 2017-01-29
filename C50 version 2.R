#Testing Dataset of 5 records

install.packages("rpart")
install.packages("rpart.plot")
install.packages("datasets")
install.packages("C50")

library(rpart)
library(rpart.plot)
library(datasets)

install.packages("RWeka")
install.packages("xlsx")
# load the package
library(RWeka)
library(xlsx)
require(rJava)
require(xlsxjars)
require(C50)
y<-c(1,NA,3)

View(y)
# load data
patientData<-read.xlsx("C:/Users/TestUpdated.xlsx",sheetName="Sheet1")
View(patientData)
summary(patientData)

subdata<-patientData[(3:39)]
View(subdata)

##bar plots
counts <- table(patientData$Anovulatory, patientData$State)
barplot(counts, main="Ratio of Weight Gain PCOS patients in states",
        xlab="States", col=c("darkblue","red"),
        legend = rownames(counts))


na.omit(subdata)

x<-C5.0(subdata[1:15,3:8],subdata[1:15,1])
plot(x)

subac<-data.frame(subdata$Excess.Hair.Growth,subdata$Male.Pattern.Baldness,subdata$Total.Testosterone,subdata$Irregular.Absent.Menses,subdata$Infertility,subdata$Hirsutism,subdata$Increased.Androgen)
View(subac)



data(subdata)
treeModel <- C5.0(x = subdata[, -19], y = subdata$Increased.Androgen)
treeModel
summary(treeModel)
ruleModel <- C5.0(Increased.Androgen ~ ., data = subdata, rules = TRUE)
ruleModel
summary(ruleModel)
plot(ruleModel)
