#this is the function for predicting  posts in MOOC
#the first input value "trainSet" is the data frame of all the train data,
#including features we used to predict the result and the given result set.
#the second input value "predictFormula" is formula to predict the result,
#if it is a classification, the formula should be like : as.factor(???)~.
#where ??? is the column name of the result to be predicted in "trainSet"
#if it is a regression model, the formula is: ???~. where ??? is 
#also the column name of the result to be predicted in "trainSet"
#the third input value is "testFeature" which is data frame of all the features we used to predict the result
#note: the column numbers of "testFeature" should be n-1 where n is the column numbers of "trainSet"
#the fourth input value is "methodName", it is the name of method you want to use, it should be "Character"
# so if you want to use Linear SVM, the methodName should be "svmLinear".
#the method name is the same as methods in package CARET
#the last input value is the range,if you want to do classification, you could just ignore it
#if you want to do regression, it should be like: c(min,max) 
#where min is the "smallest number" and "max" is the greast number
#the return value is a data frame contains all the predict values with column name "Result"
featurePredict<-function(trainSet,predictFormula,testFeature,methodName,range=NULL){
  require(caret)
  if (is.null(range)){
  control <- trainControl(method = "cv",savePred=T)}
  else {control <- trainControl(method = "cv",savePred=T,predictionBounds=range)}
  model<-train(predictFormula, data=trainSet, method = methodName, trControl = control)
  Result<-predict(model,newdata=testFeature)
  Result<-data.frame(Result)
  return(Result)}