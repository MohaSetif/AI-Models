library(e1071)
library(C50)


data(iris)
model.iris = svm(Species~., iris)
plot(model.iris, iris, Petal.Width ~ Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))



data(churn)

str(churnTrain)

churnTrain = churnTrain[,! names(churnTrain) %in% c("state","area_code", "account_length") ]

set.seed(2)
ind = sample(2, nrow(churnTrain), replace = TRUE, prob=c(0.7,0.3))
trainset = churnTrain[ind == 1,]
testset = churnTrain[ind == 2,]



model = svm(churn~., data = trainset, kernel="radial", cost=1,gamma = 1/ncol(trainset))




plot(model, trainset, total_day_minutes ~ total_intl_charge)


