fraud_check <- read.csv(file ="G:\\ExcelR\\Assignments\\Decision Tree\\fraud data\\Files\\Fraud_check.csv" )

View(fraud_check)

fraud_check$Taxable.Income<- ifelse(fraud_check$Taxable.Income <= 30000,"Risky", "Good")

View(fraud_check)

str(fraud_check)

fraud_check$Taxable.Income <- as.factor(fraud_check$Taxable.Income)

fc <- fraud_check

# partitioning data into test and train

pd <- sample(2, nrow(fc), replace = TRUE, prob = c(0.8, 0.2))
 
test <- fc[pd==2,]

train <- fc[pd==1,] 

#desion tree

library("party")


tree<-C5.0(Taxable.Income~Work.Experience+Marital.Status+Undergrad, data =train)

pred <- predict.C5.0(tree,test[,-3])

a <- table(test$Taxable.Income,pred)

sum(diag(a))/sum(a)

summary(tree)

plot(tree)


