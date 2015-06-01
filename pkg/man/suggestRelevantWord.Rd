\name{suggestRelevantWord}
\alias{suggestRelevantWord}

\title{suggestRelevantWord

}
\description{
suggest relevant words given the post text, order by word count, remove the English stop words
}
\usage{
suggestRelevantWord(post_text)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{post_text}{
the list of post texts
}
}
\details{
given the words in post text, order by number and remove the English stop words
}
\value{
return a data frame with each word and its count
}
\author{
Weizhe Liu
\email{weizheliu1991@gmail.com}
Kidzinski Lukasz
\email{lukasz.kidzinski@epfl.ch}
}
\note{
the post text should be a list of all the posts
}
\keyword{ relevant }
\keyword{ post }% __ONLY ONE__ keyword per line
