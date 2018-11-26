\name{firstlook_gp}
\alias{firstlook_gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
First Look function for goal prediction paradigm
}
\description{
This function specifies which AOI participants look to first for a given trial AFTER
they have looked to the hand AOI. This function can be used for the goal prediction paradgim.
}
\usage{
firstlook_gp(dataframe, "subjectID", "handAOI", "AOI1", "AOI2")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
  The name of the dataframe.
  }
  \item{"subjectID"}{
  The column with the subjectIDs.
}
  \item{"handAOI"}{
  The column with the handAOI.
  }
  \item{"AOI1"}{
  One of the desired AOI columns.
  }
  \item{"AOI2"}{
  One of the desired AOI columns.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This function returns a dataframe. The rows are each subject and the column is which AOI
subjects looked to first after they have looked at the hand. If the value is none either 1) infants did not look to either AOI1 or AOI2 after looking at the hand AOI or 2) they never looked to the hand AOI.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Nicole Burke and Marlene Meyer
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
