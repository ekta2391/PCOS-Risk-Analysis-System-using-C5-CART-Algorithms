install.packages("xlsx")
library(xlsx)
require(xlsxjars)
require(rJava)

logReg <- read.xlsx("C:/Users/SandData.xlsx",sheetName="Sheet1")
head(logReg)
summary(logReg)

sapply(logReg, sd)
logReg$X<-factor(logReg$X)
model <- glm(Y ~X,data=logReg,family="binomial")
model
summary(model)


