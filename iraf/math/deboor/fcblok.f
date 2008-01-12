      subroutine fcblok ( bloks, integs, nbloks, ipivot, scrtch, iflag )
calls subroutines  f a c t r b	and  s h i f t b .
c
c   f c b l o k  supervises the plu factorization with pivoting of
c  scaled rows of the almost block diagonal matrix stored in the arrays
c   b l o k s  and  i n t e g s .
c
c   factrb = subprogram which carries out steps 1,...,last of gauss
c	     elimination (with pivoting) for an individual block.
c   shiftb = subprogram which shifts the remaining rows to the top of
c	     the next block
c
c parameters
c    bloks   an array that initially contains the almost block diagonal
c	     matrix  a	to be factored, and on return contains the com-
c	     puted factorization of  a .
c    integs  an integer array describing the block structure of  a .
c    nbloks  the number of blocks in  a .
c    ipivot  an integer array of dimension  sum (integs(1,n) ; n=1,
c	     ...,nbloks) which, on return, contains the pivoting stra-
c	     tegy used.
c    scrtch  work area required, of length  max (integs(1,n) ; n=1,
c	     ...,nbloks).
c    iflag   output parameter;
c	     = 0  in case matrix was found to be singular.
c	     otherwise,
c	     = (-1)**(number of row interchanges during factorization)
c
      integer nbloks
      integer integs(3,nbloks),ipivot(1),iflag, i,index,indexb,indexn,
     *	      last,ncol,nrow
      real bloks(1),scrtch(1)
      iflag = 1
      indexb = 1
      indexn = 1
      i = 1
c			 loop over the blocks.	i  is loop index
   10	 index = indexn
	 nrow = integs(1,i)
	 ncol = integs(2,i)
	 last = integs(3,i)
c	 carry out elimination on the i-th block until next block
c	 enters, i.e., for columns 1,...,last  of i-th block.
	 call factrb(bloks(index),ipivot(indexb),scrtch,nrow,ncol,last,
     *		  iflag)
c	  check for having reached a singular block or the last block
	 if (iflag .eq. 0 .or. i .eq. nbloks)
     *					return
	 i = i+1
	 indexn = nrow*ncol + index
c	       put the rest of the i-th block onto the next block
	 call shiftb(bloks(index),ipivot(indexb),nrow,ncol,last,
     *		  bloks(indexn),integs(1,i),integs(2,i))
	 indexb = indexb + nrow
					go to 10
      end
