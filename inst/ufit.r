subroutine ufit(xk,wk,xmode,x,w,mse,x1,w1,x2,w2,ind,kt,n,goof)
implicit double precision(a-h,o-z)
integer goof
double precision mse
dimension xk(n), wk(n), x(n), w(n),x1(n), w1(n), x2(n), w2(n), ind(n), kt(n)

# Nude virgin of ufit --- 18/8/95.
# The changes are based upon Pete's observation that when we are
# seeking the OPTIMIUM mode (in terms of SSE) we need only search
# over the ``half-points'' --- 1.5, 2.5, ..., n-0.5.  If the optimum
# is at k, then the half-points k-0.5 and k+05 give SSEs that are
# at least as small as and hence are equal to the SSE at k.  This is
# because if a function is increasing on 1,...,k and decreasing on
# k,...n, then it is increasing on 1,...,(k-1)) and decresing on
# k,...,n !!!  (And likewise for 1,...,k and (k+1),...n.)   Thus if
# there is an optimum at k then there are optima at k-0.5 and k+0.5.
# Of course if k=1 then k-0.5 is not considered and likewise if k=n
# then k+0.5 is not considered.
#
# Note also that if there is an optimum at the half-point k-0.5, then
# there is also a whole-point optimum either at k-1 or k.
#
# Explanation revised (corrected) 31/05/2015.

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
		if(goof > 0) return
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

if(goof > 0) return
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
