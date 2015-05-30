\name{autoAnn}
\alias{autoAnn}
\title{
autoAnn
}
\description{
Combination of function featureExtract and featurePredict,
input: thread id, thread titles, post_time, post_text, relevant words in post text, key words in thread title,the feature to be predicted, the method and the range
output: the data frame contains all the predict values with column name "Result".
}
\usage{
autoAnn(thread_id,title,post_time,post_text,relevant_words,key_words,predicted_feature,method_name,range=NULL)
}
\arguments{
\item{thread_id}{
the list of thread id
}
  \item{title}{
the list of thread titles
}
  \item{post_time}{
the list of post_time
}
  \item{post_text}{
the list of post_text
}
  \item{relevant_words}{
the list of relevant words in post text
}
  \item{key_words}{
the list of key words in thread titles
}
  \item{predicted_feature}{
the list of features to be predicted
}
  \item{method_name}{
it is the name of method you want to use, it should be "Character"
}
  \item{range}{
the last input value is the range,if you want to do classification, you could just ignore it.
if you want to do regression, it should be like: c(min,max).Where min is the "smallest number" and "max" is the greast number
}
}
\value{
the return value is a data frame contains all the predict values with column name "Result"
}
\author{
Weizhe Liu
\email{weizheliu1991@gmail.com}
Kidzinski Lukasz
\email{lukasz.kidzinski@epfl.ch}
}
\note{
all the input list should contains the same length
}
\keyword{ MOOC}
\keyword{ automatic annotation}