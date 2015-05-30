#input:
#thread_id:the list of thread id
#title: the list of thread titles
#post_time: the list of post_time
#post_text: the list of post_text
#relevant_words: the list of relevant words in post text
#key_words: the list of key words in thread titles
#note: all these lists should have same length
#predicted_feature: the list of features to be predicted, 
#except the train data, the left should all be NA
#method_name:it is the name of method you want to use, it should be "Character"
# so if you want to use Linear SVM, the methodName should be "svmLinear".
#the method name is the same as methods in package CARET
#range:the last input value is the range,if you want to do classification, you could just ignore it
#if you want to do regression, it should be like: c(min,max) 
#where min is the "smallest number" and "max" is the greast number
#the return value is a data frame contains all the predict values with column name "Result"

autoAnn<-function(thread_id,title,post_time,post_text,relevant_words,key_words,predicted_feature,method_name,range=NULL){
        result<-featureExtract(thread_id,title,post_time,post_text,relevant_words,key_words)
        rowNum<-which(!is.na(predicted_feature))
        trainSet<-data.frame(result[rowNum,],predicted_feature[rowNum])
        colnames(trainSet)[ncol(trainSet)]<-"predict"
        predictResult<-featurePredict(trainSet,predict~.,result,method_name,range)
}