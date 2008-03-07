\name{pava}
\alias{pava}
\alias{pava.sa}
\title{
Calculate the linear increasing order isotonic regression of a set of data.
}
\description{
The ``pool adjacent violators algorithm'' (PAVA) is applied to
calculate the isotonic regression of a set of data, with respect
to the usual increasing linear ordering on the indices.
}
\usage{
pava(y, w, long.out=FALSE)
pava.sa(y, w, long.out=FALSE)
}
\arguments{
\item{y}{
Vector of data whose isotonic regression is to be calculated.
}
\item{w}{
Optional vector of weights to be used for calculating a
weighted isotonic regression; if w is not given, all weights
are taken to equal 1.
}
\item{long.out}{
Logical argument controlling the nature of the value returned.
}}
\value{
If long.out = TRUE then the result returned consists of a list whose
components are

\item{y}{
the fitted values
}
\item{w}{
the final weights
}
\item{tr}{
a set of indices made up of the smallest index in each level set,
which thus "keeps track" of the level sets.
}

If long.out = FALSE then only the vector of fitted values is returned.
}
\details{
The function \code{pava()} uses dynamically loading of a fortran
subroutine "pava" to effect the computations.  The function \code{pava.sa()}
("sa" for "stand-alone") does all of the computations in raw R.  Thus
\code{pava.sa()} could be considerably slower for large data sets.
}
\author{Rolf Turner
\email{r.turner@auckland.ac.nz}
\url{http://www.math.unb.ca}
}
\references{
Robertson, T., Wright, F. T. and Dykstra, R.  L.  (1988).
Order Restricted Statistical Inference.  Wiley, New York.
}
\seealso{\code{\link{ufit}()}}
\examples{
y <- (1:20) + rnorm(20)
ystar <- pava(y)
plot(y)
lines(ystar,type='s')
}
\keyword{regression}
\keyword{nonlinear}
% Converted by Sd2Rd version 1.21.
