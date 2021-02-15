library(sqldf)

insurance <- read.csv("~/Downloads/insurance.csv")
insurance

bmi <- ggplot(insurance, aes(bmi,charges)) + 
  geom_point(color='blue') + labs(title="BMI and Medical Charges")

cor(insurance$bmi,insurance$charges)

bmi_cor <- cor(insurance$bmi,insurance$charges)
age_cor <- cor(insurance$age,insurance$charges)
bmi_cor
age_cor

mod <- lm(charges ~ age + sex + bmi + children + smoker + region, data=insurance)
summary(mod)
coefficients(mod)
fitted(mod)

smoker <- ggplot(insurance, aes(smoker,charges)) + 
  geom_point(color='blue') + labs(title="Smoking and Medical Charges")

smoker <- sqldf("SELECT AVG(bmi) AS avg_bmi,smoker,AVG(charges) AS avg_charges FROM insurance GROUP BY smoker")


avg_nonsmoker_charges <- sum(nonsmoker_table$charges) / 1064


pred <- predict(mod, newdata=data.frame(age=50,sex='male',bmi=35,children=2,smoker='no',region='southwest'))

region <- sqldf("SELECT AVG(bmi) AS avg_bmi,region,AVG(charges) AS avg_charges FROM insurance GROUP BY region")
r <- ggplot(region, aes(region,avg_charges)) + geom_bar(stat="identity",fill='maroon') + theme_minimal()

s <- ggplot(smoker, aes(smoker,avg_charges)) + geom_bar(stat="identity",fill='black') + ylab("Average Charge")+ theme_minimal()
  
pred_1 <- predict(mod, newdata=data.frame(age=21,sex='male',bmi=23.1,children=0,smoker='no',region='northeast'))
pred_1

pred_2 <- predict(mod, newdata=data.frame(age=25,sex='female',bmi=25,children=0,smoker='yes',region='southwest'))
pred_2

pred_3 <- predict(mod, newdata=data.frame(age=60,sex='female',bmi=45,children=3,smoker='no',region='southeast'))
pred_3
