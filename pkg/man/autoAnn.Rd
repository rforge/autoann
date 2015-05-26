\name{autoAnn}
\alias{autoAnn}
\title{
autoAnn
}
\description{
given the train data, this function could be used to predict the posts' class, relevance and quality in MOOC
, it could also be implemented in similar dataset.
}
\usage{
autoAnn(trainSet,predictFormula,testFeature,methodName,range=NULL)
}
\arguments{
  \item{trainSet}{the data frame of all the train data,
including features we used to predict the result and the given result set.
}
  \item{predictFormula}{formula to predict the result,
if it is a classification, the formula should be like :
as.factor(???)~. where ??? is the column name of the result to be predicted in trainSet.
If it is a regression model, the formula is: ???~. where ??? is also the column name of the result to be predicted in trainSet.
}
  \item{testFeature}{data frame of all the features we used to predict the result. Note: the column numbers of testFeature should be n-1 where n is the column number of trainSet.
}
  \item{methodName}{It is the name of method you want to use, it should be Character. The method name is the same as methods in package CARET.
}
  \item{range}{if you want to do classification, you could just ignore it. If you want to do regression, it should be like: c(min,max). Where min is the smallest number and max is the greast number.
}
}
\details{
the package offers high accuracy in both classification and regression.
}
\value{
The return value is a data frame contains all the predict values with column name Result.
}
\author{
Weizhe Liu
\email{weizheliu1991@gmail.com}
Kidzinski Lukasz
\email{lukasz.kidzinski@epfl.ch}
}
\note{
pay attention to the format of input value predictFormula, the format changes between classification and regression.
}
\examples{
data(iris)
set.seed(100) 
trainRow<-sample(1:150,size=100)
testRow<-setdiff(1:150,trainRow)
trainSet<-iris[trainRow,]
testFeature<-iris[testRow,1:4]
result<-autoAnn(trainSet,as.factor(Species)~.,testFeature,"extraTrees")
}
\keyword{ MOOC}
\keyword{ automatic annotation }