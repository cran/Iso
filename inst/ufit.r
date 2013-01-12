subroutine ufit(xk,wk,xmode,x,w,mse,x1,w1,x2,w2,ind,kt,n,goof)
implicit double precision(a-h,o-z)
logical goof
double precision mse
dimension xk(n), wk(n), x(n), w(n),x1(n), w1(n), x2(n), w2(n), ind(n), kt(n)

# Nude virgin of ufit --- 18/8/95.
# The changes are based upon Pete's observation that when we are
# seeking the OPTIMIUM mode (in terms of SSE) we need only search
# over the ``half-points'' --- 1.5, 2.5, ..., n-0.5.  The optimum
# half-point will give the same SSE as one of the two adjacent
# integer points.  I.e. SSE(k+0.5) will be the same as either SSE(k)
# or SSE(k+1).  It will be SSE(k) if y-hat_k >= y-hat_{k+1}, and
# vice-versa.  The y-hat's are the fitted values corresponding to
# assuming that the mode is k+0.5.

if(xmode < 0) {
	m      =  n-1
	x0     =  1.5d0
	xmax   = -1.d0
	ssemin =  1.d200

	do i = 1,m {
		do j = 1,n {
			x(j) = xk(j)
			w(j) = wk(j)
		}
		call unimode(x,w,x1,w1,x2,w2,ind,kt,x0,n,goof)
		if(goof) return
		sse = 0.d0
		do j = 1,n {
			sse = sse + (x(j)-xk(j))**2
		}
		if(sse < ssemin) {
			ssemin = sse
			xmax = x0
		}
		x0 = x0+1.d0
	}
	k1 = int(xmax-0.5d0)
	k2 = int(xmax+0.5d0)
}
else xmax = xmode
do j = 1,n {
	x(j) = xk(j)
	w(j) = wk(j)
}
call unimode(x,w,x1,w1,x2,w2,ind,kt,xmax,n,goof)
if(goof) return
if(xmode < 0) {
	mse = ssemin/dble(n)
	if(x(k1)>=x(k2)) xmode=dble(k1)
	else xmode=dble(k2)
}
else {
	sse = 0.d0
	do j = 1,n {
		sse = sse + (x(j)-xk(j))**2
	}
	mse = sse/dble(n)
}
return
end
