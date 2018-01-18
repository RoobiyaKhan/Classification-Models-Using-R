# Logistic Regression

# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[,1:2] = scale(training_set[,1:2])
test_set[-3] = scale(test_set[-3]) #removes third column alone

#fitting logistic regression to the training set
classifier = glm(formula = Purchased ~ .,
                 family = binomial, #for logistic reg mention binomial
                 data = training_set)

#predicting the test set results
prob_pred = predict(classifier, type = 'response',newdata = test_set[-3])#use type = response for logistic reg
prob_pred                                                          #that will give the prob listed in the single vector
y_pred = ifelse(prob_pred > 0.5, 1, 0)
y_pred

#making the confusion matrix
cm = table(test_set[,3], y_pred)
cm

#visualizing the training set results
#install.packages('ElemStatLearn')
library(ElemStatLearn)
set = training_set
x1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
x2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(x1, x2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'logistic regression(training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch =21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

#visualizing the test set results
#install.packages('ElemStatLearn')
library(ElemStatLearn)
set = test_set
x1 = seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
x2 = seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01)
grid_set = expand.grid(x1,x2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[,-3],
     main = 'logistic regression(test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))
contour(x1,x2,matrix(as.numeric(y_grid),length(x1),length(x2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))




