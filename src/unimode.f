C Output from Public domain Ratfor, version 1.0
      subroutine unimode(y,w,y1,w1,y2,w2,ind,kt,tau,n,goof)
      implicit double precision(a-h,o-z)
      logical goof
      dimension y(n), w(n), y1(n), w1(n), y2(n), w2(n), ind(n), kt(n)
      goof = .false.
      k1 = 0
      k2 = 0
      do23000 i = 1,n 
      if(i .lt. tau)then
      y1(i) = y(i)
      w1(i) = w(i)
      k1 = k1+1
      endif
      if(i .gt. tau)then
      j = n-i+1
      y2(j) = y(i)
      w2(j) = w(i)
      k2 = k2+1
      endif
23000 continue
      if(k1+k2 .eq. n)then
      if(k1 .gt. 0)then
      call pava(y1,w1,kt,k1)
      do23010 i = 1,k1 
      y(i) = y1(i)
      w(i) = w1(i)
23010 continue
      endif
      if(k2 .gt. 0)then
      call pava(y2,w2,kt,k2)
      do23014 i = 1,k2 
      j = n-i+1
      y(j) = y2(i)
      w(j) = w2(i)
23014 continue
      endif
      return
      endif
      if(k1+k2 .eq. n-1)then
      yk = y(k1+1)
      if(k1 .gt. 0)then
      call pava(y1,w1,kt,k1)
      endif
      if(k2 .gt. 0)then
      call pava(y2,w2,kt,k2)
      endif
      i1 = 1
      i2 = 1
      i = 1
23022 continue
      if(i1 .le. k1)then
      t1 = y1(i1)
      else
      t1 = y2(k2)+1.d10
      endif
      if(i2 .le. k2)then
      t2 = y2(i2)
      else
      t2 = y1(k1)+1.d10
      endif
      if(t1 .lt. t2)then
      y(i) = y1(i1)
      ind(i) = i1
      i1 = i1+1
      else
      y(i) = y2(i2)
      ind(i) = n-i2+1
      i2 = i2+1
      endif
      i = i + 1
      if(i .eq. n)then
      goto 23024
      endif
      goto 23022
23024 continue
      y(n) = yk
      ind(n) = k1+1
      do23033 i = 1,n 
      w1(ind(i)) = w(i)
23033 continue
      do23035 i = 1,n 
      w(i) = w1(i)
23035 continue
      call pava(y,w,kt,n)
      do23037 i = 1,n 
      y1(ind(i)) = y(i)
      w1(ind(i)) = w(i)
23037 continue
      do23039 i = 1,n 
      y(i) = y1(i)
      w(i) = w1(i)
23039 continue
      else
      goof = .true.
      endif
      return
      end
