pava <- function(y, w=NULL, decreasing=FALSE, long.out = FALSE)
{
#
# Function 'pava'.  To perform isotonic regression for a simple
# (increasing) linear ordering using the ``pool adjacent violators
# algorithm''.  If long.out = TRUE then the result returned consists
# of a list containing the fitted values, the final weights, and a set
# of indices `tr', made up of the smallest index in each level set,
# which thus keeps track of the level sets.  Otherwise only the fitted
# values are returned.
# 
	if(decreasing) y <- rev(y)
	n <- length(y)
	if(is.null(w))
		w <- rep(1, n)
	else if(decreasing) w <- rev(w)
	if(n == 1) {
		if(long.out) return(list(y=y,w=w,tr=1))
		else return(y)
	}

	rslt <- .Fortran(
			"pava",
			y=as.double(y),
			w=as.double(w),
			kt=integer(n),
			n=as.integer(n),
			PACKAGE="Iso"
			)
	if(long.out) {
		tr <- rslt$kt
		if(decreasing) {
			tr <- unname(unlist(tapply(1:n,-rev(tr),
				function(x){rep(min(x),length(x))})))
			rslt <- list(y=rev(rslt$y),w=rev(rslt$w),tr=tr)
		}
		else rslt <- list(y = rslt$y, w = rslt$w, tr = rslt$kt)
	} else rslt <- if(decreasing) rev(rslt$y) else y
	rslt
}
