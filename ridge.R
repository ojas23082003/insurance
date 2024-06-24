library(ISLR)
library(glmnet)
library(dplyr)
library(tidyr)
library(MASS)
Hitters = na.omit(Hitters)
View(Boston)
x = model.matrix(medv~., Boston)[,-1]
y = Boston$medv
y
x
# range of values of lambda
grid = 10^seq(10, -2, length=100)

# ridge model
ridge_mod = glmnet(x,y,alpha=0, lambda = grid)
dim(coef(ridge_mod))

# plotting the model with respect to values of lambda
plot(ridge_mod, xvar='lambda', lable=TRUE)
plot(ridge_mod)
cv.ridge = cv.glmnet(x,y,alpha=0)
plot(cv.ridge)

ridge_mod$lambda[50] # 50th lambda value
coef(ridge_mod)[,50]


# lasso regression
lasso_mod = glmnet(x,y,alpha=1, lambda=grid) # fit lasso model 
plot(lasso_mod)
set.seed(1)
cv.out = cv.glmnet(x,y,alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min # select the best value of lambda
bestlam0.
out = glmnet(x,y,alpha=1, lambda=grid)


# estimate test error
set.seed(1)

train = Boston %>% sample_frac(0.5)

test = Boston %>% setdiff(train)

class(train)

x_train = model.matrix(medv~., train)[,-1]
x_test = model.matrix(medv~., test)[,-1]

x_train

# Assuming 'df' is your dataframe
y_train <- train[, ncol(train)]
y_train
y_test <- test[, ncol(test)]
y_test
test

ridge_model = glmnet(x_train, y_train, alpha=0, lambda=grid)
ridge_pred = predict(ridge_model, s=4, newx=x_test)
plot(ridge_model, xvar="lambda", label=TRUE)

mean((mean(ridge_pred) - y_test)^2)

lasso_model = glmnet(x_train, y_train, alpha=1, lambda=grid)
lasso_pred = predict(lasso_model, s=4, newx=x_test)
plot(lasso_model, xvar="lambda", label=TRUE)

mean((mean(lasso_pred) - y_test)^2)
