\name{getridofNA_gp}
\alias{getridofNA_gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get rid of NAs for firstlook_gp function
}
\description{
This function is neccessary for 'firstlook_gp' function to run properly.
This gets rid of NA values in the desired AOI colunms so that the firstlook
function runs smoothly. This function requires the package 'tidyverse'.
}
\usage{
getridofNA_gp(dataframe, HandAOI, AOI1, AOI2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
  The name of the dataframe you are using
}
  \item{dataframe$HandAOI}{
  The column of the hand AOI.
  }
  \item{dataframe$AOI}{
  The column of one of the desired AOIs.
  }
  \item{dataframe$AOI2}{
  The column of one of the desired AOIs.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This returns the dataframe as it was before, but replacing NAs with 999 in the desired columns.
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
