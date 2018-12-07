\name{switchestotal}
\alias{switchestotal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Switches total - this function calculates contingentent switches between two AOIs.
}
\description{
Given two AOIs, how many contingent switches were made? It gives the total number of switches as well as the break down of when it was aoi1 to aoi2 AND aoi2 to aoi1.
}
\usage{
switchestotal(data_by_trial, "subjectID", "AOI1", "AOI2")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_by_trial}{
  the dataframe - broken up by unique trial type
  }
  \item{"subjectID"}{
  the column that contains the subjectIDs
  }
  \item{"AOI1"}{
  the column of 1 of the aois of interest
  }
   \item{"AOI2"}{
  the column of the other aoi of interest
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This function returns a dataframe. The rows are each subject and the columns are
the number of switches subjects make between the AOIs.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Nicole Burke
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
