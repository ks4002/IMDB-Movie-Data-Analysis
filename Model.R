movie = read_csv('Final-IMDB-Movie-Data.csv')

#---------------Text mining modeling------------------
data <- select(movie, -1, -Title, -Genre, -Description, -Director, -Actors) #get rid of unused variables

set.seed(1031)
split = sample(1:nrow(data),size = 0.7*nrow(data))
train = data[split,]
test = data[-split,]

#CART model to predict review_rating using all other variables
library(rpart); library(rpart.plot)
tree = rpart(Rating~.,train)
rpart.plot(tree)
pred_tree = predict(tree,newdata=test)
rmse_tree = sqrt(mean((pred_tree - test$Rating)^2)); rmse_tree

#linear regression
lm1 <- lm(Rating ~.,train)
summary(lm1)
pred_linear <- predict(lm1, test) #predict
rmse_linear = sqrt(mean((pred_linear - test$Rating)^2)) #rmse score

#random forest
library(caret)
trControl=trainControl(method="cv",number=10)
tuneGrid = expand.grid(mtry=1:5)
startTime <- proc.time()
cvForest = train(Rating~.,movie,method="rf",ntree=1000,trControl=trControl,tuneGrid=tuneGrid)
proc.time() - startTime
cvForest #mytry = 5 best
#modeling part
startTime <- proc.time()
Forest = train(Rating~.,movie,method="rf",ntree=1000,mytry = 5)
proc.time() - startTime

#---------------How do the categories affect rating?------------------

