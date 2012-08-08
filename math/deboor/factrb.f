      subroutine factrb ( w, ipivot, d, nrow, ncol, last, iflag )
c  adapted from p.132 of 'element.numer.analysis' by conte-de boor
c
c  constructs a partial plu factorization, corresponding to steps 1,...,
c   l a s t   in gauss elimination, for the matrix  w  of order
c   ( n r o w ,  n c o l ), using pivoting of scaled rows.
c
c  parameters
c    w	     contains the (nrow,ncol) matrix to be partially factored
c	     on input, and the partial factorization on output.
c    ipivot  an integer array of length nrow containing a record of the
c	     pivoting strategy used; row ipivot(i) is used during the
c	     i-th elimination step, i=1,...,last.
c    d	     a work array of length nrow used to store row sizes
c	     temporarily.
c    nrow    number of rows of w.
c    ncol    number of columns of w.
c    last    number of elimination steps to be carried out.
c    iflag   on output, equals iflag on input times (-1)**(number of
c	     row interchanges during the factorization process), in
c	     case no zero pivot was encountered.
c	     otherwise, iflag = 0 on output.
c
      integer nrow
      integer ipivot(nrow),ncol,last,iflag, i,ipivi,ipivk,j,k,kp1
      real w(nrow,ncol),d(nrow), awikdi,colmax,ratio,rowmax
c  initialize ipivot, d
      do 10 i=1,nrow
	 ipivot(i) = i
	 rowmax = 0.
	 do 9 j=1,ncol
    9	    rowmax = amax1(rowmax, abs(w(i,j)))
	 if (rowmax .eq. 0.)		go to 999
   10	 d(i) = rowmax
c gauss elimination with pivoting of scaled rows, loop over k=1,.,last
      k = 1
c	 as pivot row for k-th step, pick among the rows not yet used,
c	 i.e., from rows ipivot(k),...,ipivot(nrow), the one whose k-th
c	 entry (compared to the row size) is largest. then, if this row
c	 does not turn out to be row ipivot(k), redefine ipivot(k) ap-
c	 propriately and record this interchange by changing the sign
c	 of  i f l a g .
   11	 ipivk = ipivot(k)
	 if (k .eq. nrow)		go to 21
	 j = k
	 kp1 = k+1
	 colmax = abs(w(ipivk,k))/d(ipivk)
c	       find the (relatively) largest pivot
	 do 15 i=kp1,nrow
	    ipivi = ipivot(i)
	    awikdi = abs(w(ipivi,k))/d(ipivi)
	    if (awikdi .le. colmax)	go to 15
	       colmax = awikdi
	       j = i
   15	    continue
	 if (j .eq. k)			go to 16
	 ipivk = ipivot(j)
	 ipivot(j) = ipivot(k)
	 ipivot(k) = ipivk
	 iflag = -iflag
   16	 continue
c	 if pivot element is too small in absolute value, declare
c	 matrix to be noninvertible and quit.
	 if (abs(w(ipivk,k))+d(ipivk) .le. d(ipivk))
     *					go to 999
c	 otherwise, subtract the appropriate multiple of the pivot
c	 row from remaining rows, i.e., the rows ipivot(k+1),...,
c	 ipivot(nrow), to make k-th entry zero. save the multiplier in
c	 its place.
	 do 20 i=kp1,nrow
	    ipivi = ipivot(i)
	    w(ipivi,k) = w(ipivi,k)/w(ipivk,k)
	    ratio = -w(ipivi,k)
	    do 20 j=kp1,ncol
   20	       w(ipivi,j) = ratio*w(ipivk,j) + w(ipivi,j)
	 k = kp1
c	 check for having reached the next block.
	 if (k .le. last)		go to 11
					return
c     if  last	.eq. nrow , check now that pivot element in last row
c     is nonzero.
   21 if( abs(w(ipivk,nrow))+d(ipivk) .gt. d(ipivk) )
     *					return
c		    singularity flag set
  999 iflag = 0
					return
      end
