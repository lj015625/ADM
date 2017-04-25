insuranceData<-read.csv(file="C:/Users/lj015625/Desktop/DataMining Class/Applied_Data_Mining/assignment_module_1/data/insurance.csv")
attach(insuranceData)
insuranceData$obesity <- (ifelse(bmi >= 30, 1, 0))
insuranceData$smokerFactor <- (ifelse(smoker=='yes', 1, 0))
insuranceData$obesity_smoker <- (insuranceData$obesity*insuranceData$smokerFactor)
summary(insuranceData)

Numeric_Vars <-insuranceData[,c('age','bmi','children','smokerFactor','charges')]
Correlation_Matrix <- cor(Numeric_Vars)
Correlation_Matrix

insuranceModel3 <- lm(charges~age+bmi+children+smokerFactor+obesity+obesity_smoker, data=insuranceData)
summary(insuranceModel3)

library(ggfortify)
autoplot(insuranceModel3, label.size = 3) 


insuranceModel4 <- lm(charges~age+children+smokerFactor+obesity_smoker, data=insuranceData)
summary(insuranceModel4)

autoplot(insuranceModel4, label.size = 3)


testingData<-read.csv(file="C:/Users/lj015625/Desktop/DataMining Class/Applied_Data_Mining/assignment_module_1/data/CompanyABC.csv")
testingData$obesity <- ifelse(testingData$bmi >= 30, 1, 0)
testingData$obesity_category <- ifelse(testingData$bmi >= 30, 'yes', 'no')
testingData$smokerFactor <- ifelse(testingData$smoker=='yes', 1, 0)
testingData$obesity_smoker <- testingData$obesity*testingData$smokerFactor
testingData$obesity_smoker_category <- ifelse(testingData$obesity_smoker == 1, 'yes', 'no')
summary(testingData)


#summaryData =data.frame(age=37.14,bmi=29.88,children=0.98,obesity_smoker=0.06, obesity=0.44, smokerFactor=0.24) 
#predict(insuranceModel3, summaryData)

testingData$estimated_charges <- predict(insuranceModel3, testingData)
summary(testingData$estimated_charges)

aggregate(estimated_charges~smoker, data=testingData, FUN="mean")
aggregate(estimated_charges~obesity, data=testingData, FUN="mean")
# interaction term
aggregate(estimated_charges~obesity_smoker, data=testingData, FUN="mean")

library(ggplot2)
ggplot(testingData, aes(x = age, y = estimated_charges)) +
  geom_point(color = "forestgreen", shape=4, size = 2) + theme_bw() + 
  stat_smooth(method = "lm", col = "red", se=FALSE) +
  xlab("Customer Age") + ylab("Estimated Charges in USD") +
  ggtitle("Customer Age vs. Estimate Charges") 

ggplot(testingData, aes(x = children, y = estimated_charges)) +
  geom_point(color = "blue", shape=1, size = 2) + theme_bw() + 
  stat_smooth(method = "lm", col = "red", se=FALSE) +
  xlab("Children") + ylab("Estimated Charges in USD") +
  ggtitle("Children vs. Estimate Charges") 

ggplot(testingData, aes(fill = obesity_category, factor(obesity_category), estimated_charges)) + 
  geom_boxplot() + theme_bw() + xlab("Obesity") + ylab("Estimated Charges in USD") +
  ggtitle("Obesity vs non-obesity Estimate Charges") 

ggplot(testingData, aes(fill = smoker, factor(smoker), estimated_charges)) +
  geom_boxplot() + theme_bw() + xlab("Smoker") + ylab("Estimated Charges in USD") +
  ggtitle("Non-smoker vs. Smoker Estimate Charges")

ggplot(testingData, aes(fill = obesity_smoker_category, factor(obesity_smoker_category), estimated_charges)) +
  geom_boxplot() + theme_bw() + xlab("Obesity and Smoker") + ylab("Estimated Charges in USD") +
  ggtitle("Normal vs. Obesity and Smoker Estimate Charges")

