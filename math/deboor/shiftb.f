      subroutine shiftb ( ai, ipivot, nrowi, ncoli, last,
     *			  ai1, nrowi1, ncoli1 )
c  shifts the rows in current block, ai, not used as pivot rows, if
c  any, i.e., rows ipivot(last+1),...,ipivot(nrowi), onto the first
c  mmax = nrow-last rows of the next block, ai1, with column last+j of
c  ai  going to column j , j=1,...,jmax=ncoli-last. the remaining col-
c  umns of these rows of ai1 are zeroed out.
c
c			      picture
c
c	original situation after	 results in a new block i+1
c	last = 2 columns have been	 created and ready to be
c	done in factrb (assuming no	 factored by next factrb call.
c	interchanges of rows)
c		    1
c	       x  x 1x	x  x	       x  x  x	x  x
c		    1
c	       0  x 1x	x  x	       0  x  x	x  x
c  block i	    1			    ---------------
c  nrowi = 4   0  0 1x	x  x	       0  0 1x	x  x  0  01
c  ncoli = 5	    1			    1		  1
c  last = 2    0  0 1x	x  x	       0  0 1x	x  x  0  01
c  -------------------------------	    1		  1   new
c		    1x	x  x  x  x	    1x	x  x  x  x1  block
c		    1			    1		  1   i+1
c  block i+1	    1x	x  x  x  x	    1x	x  x  x  x1
c  nrowi1= 5	    1			    1		  1
c  ncoli1= 5	    1x	x  x  x  x	    1x	x  x  x  x1
c  -------------------------------	    1-------------1
c		    1
c
      integer nrowi, ncoli, nrowi1, ncoli1
      integer ipivot(nrowi),last, ip,j,jmax,jmaxp1,m,mmax
      real ai(nrowi,ncoli),ai1(nrowi1,ncoli1)
      mmax = nrowi - last
      jmax = ncoli - last
      if (mmax .lt. 1 .or. jmax .lt. 1) return
c	       put the remainder of block i into ai1
      do 10 m=1,mmax
	 ip = ipivot(last+m)
	 do 10 j=1,jmax
   10	    ai1(m,j) = ai(ip,last+j)
      if (jmax .eq. ncoli1)		return
c	       zero out the upper right corner of ai1
      jmaxp1 = jmax + 1
      do 20 j=jmaxp1,ncoli1
	 do 20 m=1,mmax
   20	    ai1(m,j) = 0.
					return
      end
