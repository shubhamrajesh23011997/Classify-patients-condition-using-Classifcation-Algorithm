data = read.csv("Dataset_spine.csv")
getwd()
View(data)
summary(data)

bx=boxplot(data$pelvic_incidence)
bx$stats
quantile(data$pelvic_incidence,seq(0,1,0.02))
data$pelvic_incidence<-ifelse(data$pelvic_incidence>=94,94,data$pelvic_incidence)
bx=boxplot(data$pelvic_incidence)

bx=boxplot(data$pelvic_tilt)
bx$stats
quantile(data$pelvic_tilt,seq(0,1,0.02))
data$pelvic_tilt<-ifelse(data$pelvic_tilt>=39,39,data$pelvic_tilt)
bx=boxplot(data$pelvic_tilt)

bx=boxplot(data$lumbar_lordosis_angle)
bx$stats
quantile(data$lumbar_lordosis_angle,seq(0,1,0.02))
data$lumbar_lordosis_angle<-ifelse(data$lumbar_lordosis_angle>=94,94,data$lumbar_lordosis_angle)
bx=boxplot(data$lumbar_lordosis_angle)

bx=boxplot(data$sacral_slope)
bx$stats
quantile(data$sacral_slope,seq(0,1,0.02))
data$sacral_slope<-ifelse(data$sacral_slope>=70,70,data$sacral_slope)
bx=boxplot(data$lumbar_lordosis_angle)

bx=boxplot(data$pelvic_radius)
bx$stats
quantile(data$pelvic_radius,seq(0,1,0.02))
data$pelvic_radius<-ifelse(data$pelvic_radius>=146,146,data$pelvic_radius)
data$pelvic_radius<-ifelse(data$pelvic_radius<=89,89,data$pelvic_radius)
bx=boxplot(data$pelvic_radius)

bx=boxplot(data$degree_spondylolisthesis)
bx$stats
quantile(data$degree_spondylolisthesis,seq(0,1,0.02))
data$degree_spondylolisthesis<-ifelse(data$degree_spondylolisthesis>=91,91,data$degree_spondylolisthesis)
bx=boxplot(data$degree_spondylolisthesis)

#create dummy variable for target
# making dummy variable for status
data$Status<- ifelse(data$Status=="Abnormal",1,0)
View(data)

library(car)
library(corrplot)
cor = cor(data)
corrplot(cor, method="circle", type="lower")

set.seed(1234)
library(caret)
splitIndex<- createDataPartition(data$Status, p = .70,list = FALSE, times = 1)
trainSplit<- data[ splitIndex,]
testSplit<- data[-splitIndex,]

print(table(trainSplit$Status))
print(table(testSplit$Status))

prop.table(table(trainSplit$Status))
prop.table(table(testSplit$Status))


library(randomForest)
modelrf<- randomForest(as.factor(Status) ~ . , data = trainSplit, do.trace=T)
modelrf

importance(modelrf)


varImpPlot(modelrf)

predrf_tr<- predict(modelrf, trainSplit)
predrf_test<- predict(modelrf, testSplit)
 
confusionMatrix(as.factor(predrf_tr),as.factor(trainSplit$Status)) 
confusionMatrix(as.factor(predrf_test),as.factor(testSplit$Status)) 

