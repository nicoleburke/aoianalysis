\name{firstlook}
\alias{firstlook}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FirstLook - which AOI was the first look?
}
\description{
This function specifies which AOI participants look to first for a given trial.
This function only deals with 2 AOIs - typically used in infant/child
studies where you want to know which AOI - the left or right AOI - did
infants/children first look? This data should deal with raw eyetracking data.
}
\usage{
firstlook(dataframe$subjects, dataframe$aoi1, dataframe$aoi2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe$subjects}{
    the column that contains the subjectIDs
}
  \item{dataframe$aoi1}{
  the column of 1 of the aois of interest
  }
   \item{dataframe$aoi2}{
  the column of the other aoi of interest
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This function returns a dataframe. The rows are each subject and the column is which AOI
subjects looked too first.
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
