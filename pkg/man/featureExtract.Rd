\name{featureExtract}
\alias{featureExtract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{featureExtract
%%  ~~function to do ... ~~
}
\description{
Extract the number of relevant words, number of key words,
#number of words post, ratio of relevant words, number of question marks,index, number of sentence and number of spell errors in each post text,along with the number of key words and ratio of key words in each thread title. Given thread id, thread titles, post_time, post_text, relevant words in post text, key words in thread title
}
\usage{
featureExtract(thread_id,title,post_time,post_text,relevant_words,key_words)
}
%- maybe also 'usage' for other objects documented here.
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
}
\value{
the return value is a data frame which contains the number of relevant words, number of key words,
#number of words post, ratio of relevant words, number of question marks,index, number of sentence and number of spell errors in each post text,along with the number of key words and ratio of key words in each thread title.
}
\author{
Weizhe Liu
\email{weizheliu1991@gmail.com}
Kidzinski Lukasz
\email{lukasz.kidzinski@epfl.ch}
}
\note{
all the input list should contain the same length
}
\keyword{ MOOC }
\keyword{ feature extract }