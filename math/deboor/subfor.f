      subroutine subfor ( w, ipivot, nrow, last, b, x )
c  carries out the forward pass of substitution for the current block,
c  i.e., the action on the right side corresponding to the elimination
c  carried out in  f a c t r b	for this block.
c     at the end, x(j) contains the right side of the transformed
c  ipivot(j)-th equation in this block, j=1,...,nrow. then, since
c  for i=1,...,nrow-last, b(nrow+i) is going to be used as the right
c  side of equation  i	in the next block (shifted over there from
c  this block during factorization), it is set equal to x(last+i) here.
c
c parameters
c    w, ipivot, nrow, last  are as on return from factrb.
c    b(j)   is expected to contain, on input, the right side of j-th
c	    equation for this block, j=1,...,nrow.
c    b(nrow+j)	 contains, on output, the appropriately modified right
c	    side for equation j in next block, j=1,...,nrow-last.
c    x(j)   contains, on output, the appropriately modified right
c	    side of equation ipivot(j) in this block, j=1,...,last (and
c	    even for j=last+1,...,nrow).
c
      integer nrow
      integer ipivot(nrow), ip,jmax,k, j
      integer last, nrowml, lastp1
c     dimension b(nrow + nrow-last)
      real w(nrow,last),b(1),x(nrow),sum
      ip = ipivot(1)
      x(1) = b(ip)
      if (nrow .eq. 1)			go to 99
      do 15 k=2,nrow
	 ip = ipivot(k)
	 jmax = amin0(k-1,last)
	 sum = 0.
	 do 14 j=1,jmax
   14	    sum = w(ip,j)*x(j) + sum
   15	 x(k) = b(ip) - sum
c
c     transfer modified right sides of equations ipivot(last+1),...,
c     ipivot(nrow) to next block.
      nrowml = nrow - last
      if (nrowml .eq. 0)		go to 99
      lastp1 = last+1
      do 25 k=lastp1,nrow
   25	 b(nrowml+k) = x(k)
   99					return
      end
