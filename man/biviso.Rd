\name{biviso}
\Rdversion{1.1}
\alias{biviso}
\title{
   Bivariate isotonic regression.
}
\description{
   Bivariate isotonic regression with respect to simple (increasing)
   linear ordering on both variables.
}
\usage{
biviso(y, w = NULL, eps = NULL, ncycle = 10000)
}
\arguments{
  \item{y}{
  The matrix of observations to be isotonized.  It must of course
  have at least two rows and at least two columns.
}
  \item{w}{
  A matrix of weights, greater than or equal to zero, of the same
  dimension as \code{y}.  If left \code{NULL} then \code{w} is
  created as a matrix all of whose entries are equal to \code{1}.
}
  \item{eps}{
  Convergence criterion.  The algorithm is deemed to have converged
  if each entry of the output matrix, after the completion of the
  current iteration, does not differ by more than \code{eps} from
  the corresponding entry of the matrix after the completion of the
  previous iteration.  If this argument is not supplied it defaults
  to \code{sqrt(.Machine$double.eps)}.
}
  \item{ncycle}{
  The maximum number of cycles of the iteration procedure.  If the
  procedure has not converged after \code{ncycle} iterations then
  an error is given.  (See below.)
}
}
\section{Error Messages}{
The subroutine comprising Algorithm AS 206 produces an error
code \code{ifault} with values from \code{1} to \code{6}
The meaning of these codes is as follows:
\itemize{
\item 0: No error.
\item 1: Convergence was not attained in \code{ncycle} cycles.
\item 2: At least one entry of \code{w} was negative.
\item 3: Either \code{nrow(y)} or \code{ncol(y)} was less than 2.
\item 4: A near-zero weight less than \code{delta=0.00001} was
          replaced by \code{delta}.
\item 5: Convergence was not attained \emph{and} a non-zero weight
         was replaced by \code{delta}.
\item 6: All entries of \code{w} were less than \code{delta}.
}

If \code{ifault==4} a warning is given.  All of the other non-zero
values of \code{ifault} result in an error being given.
}

\details{
  See the paper by Bril et al., (\emph{References}) and the references
  cited therein for details.
}
\value{
  A matrix of the same dimensions as \code{y} containing the
  corresponding isotonic values.  It has an attribute \code{icycle}
  equal to the number of cycles required to achieve convergence
  of the algorithm.
}
\references{
Bril, Gordon; Dykstra, Richard; Pillers Carolyn, and Robertson, Tim ;
Isotonic regression in two independent variables; Algorithm AS 206;
JRSSC (Applied Statistics), vol. 33, no. 3, pp. 352-357, 1984.
}
\author{Rolf Turner
\email{r.turner@auckland.ac.nz}
\url{http://www.math.unb.ca}
}
\section{WARNING}{
This  function appears not to achieve exact isotonicity, at least
not quite.  For instance one can do:
\preformatted{
    set.seed(42)
    u  <- matrix(runif(400),20,20)
    iu <- biviso(u)
    any(apply(iu,2,is.unsorted))
}
and get \code{TRUE}.  It turns out that columns 13, 14, and 16 of
\code{iu} have exceptions to isotonicity.  E.g. six of the values
of \code{diff(iu[,13])} are less than zero.  However only one of
these is less than \code{sqrt(.Machine$double.eps)}, and then only
\dQuote{marginally} smaller.

So some of these negative values are \dQuote{numerically different}
from zero, but not by much.  The largest in magnitude in this
example, from column 16, is \code{-2.217624e-08} --- which is
probably not of \dQuote{practical importance}.

Note also that this example occurs in a very artificial context in
which there is no actual isotonic structure underlying the data.
}

\seealso{ \code{\link{pava}()} \code{\link{pava.sa}()} \code{\link{ufit}()}
}
\examples{
x <- 1:20
y <- 1:10
xy <- outer(x,y,function(a,b){a+b+0.5*a*b})
ixy <- biviso(xy)

u <- matrix(runif(400),20,20)
v <- biviso(u)
}
\keyword{regression}
\keyword{nonlinear}
