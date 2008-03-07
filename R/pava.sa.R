pava.sa <- function(y,w=NULL,long.out=FALSE)
{
#
# Function 'pava.sa' (stand-alone pava). To perform isotonic
# regression for a simple (increasing) linear ordering using the ``pool
# adjacent violators algorithm''.  This version is programmed in raw
# R; i.e.  it does not invoke dynamically loaded fortran.  If
# long.out = TRUE then the result returned consists of a list containing
# the fitted values, the final weights, and a set of indices `tr',
# made up of the smallest index in each level set, which thus keeps
# track of the level sets.  Otherwise only the fitted values are
# returned.
# 

n <- length(y)
if(is.null(w)) w <- rep(1,n)
r <- rep(1,n)
repeat {
	stble <- TRUE
	i <- 1
	while(i < n) {
		if(y[i] > y[i+1]) {
			stble <- FALSE
			www <- w[i] + w[i+1]
			ttt <- (w[i]*y[i] + w[i+1]*y[i+1])/www
			y[i+1] <- ttt
			w[i+1] <- www
			y <- y[-i]
			w <- w[-i]
			r[i+1] <- r[i] + r[i+1]
			r <- r[-i]
			n <- n-1
		}
		i <- i+1
	}
if(stble) break
}
y  <- rep(y,r)
w  <- rep(w,r)
tr <- rep(tapply(1:length(y),rep(1:length(r),r),min),r)
if(long.out) list(y=y,w=w,tr=tr) else y
}
