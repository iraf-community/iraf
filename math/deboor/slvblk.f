      subroutine slvblk ( bloks, integs, nbloks, b, ipivot, x, iflag )
c    this program solves  the  linear system  a*x = b  where a is an
c  almost block diagonal matrix.  such almost block diagonal matrices
c  arise naturally in piecewise polynomial interpolation or approx-
c  imation and in finite element methods for two-point boundary value
c  problems.  the plu factorization method is implemented here to take
c  advantage of the special structure of such systems for savings in
c  computing time and storage requirements.
c
c		   parameters
c  bloks   a one-dimenional array, of length
c		    sum( integs(1,i)*integs(2,i) ; i = 1,nbloks )
c	   on input, contains the blocks of the almost block diagonal
c	   matrix  a  .  the array integs (see below and the example)
c	   describes the block structure.
c	   on output, contains correspondingly the plu factorization
c	   of  a  (if iflag .ne. 0).  certain of the entries into bloks
c	   are arbitrary (where the blocks overlap).
c  integs  integer array description of the block structure of	a .
c	     integs(1,i) = no. of rows of block i	 =  nrow
c	     integs(2,i) = no. of colums of block i	 =  ncol
c	     integs(3,i) = no. of elim. steps in block i =  last
c			   i  = 1,2,...,nbloks
c	   the linear system is of order
c		 n  =  sum ( integs(3,i) , i=1,...,nbloks ),
c	   but the total number of rows in the blocks is
c	       nbrows = sum( integs(1,i) ; i = 1,...,nbloks)
c  nbloks  number of blocks
c  b	   right side of the linear system, array of length nbrows.
c	   certain of the entries are arbitrary, corresponding to
c	   rows of the blocks which overlap (see block structure and
c	   the example below).
c  ipivot  on output, integer array containing the pivoting sequence
c	   used. length is nbrows
c  x	   on output, contains the computed solution (if iflag .ne. 0)
c	   length is n.
c  iflag   on output, integer
c	     = (-1)**(no. of interchanges during factorization)
c		    if	a  is invertible
c	     = 0    if	a  is singular
c
c		    auxiliary programs
c  fcblok (bloks,integs,nbloks,ipivot,scrtch,iflag)  factors the matrix
c	    a , and is used for this purpose in slvblk. its arguments
c	   are as in slvblk, except for
c	       scrtch = a work array of length max(integs(1,i)).
c
c  sbblok (bloks,integs,nbloks,ipivot,b,x)  solves the system a*x = b
c	   once  a  is factored. this is done automatically by slvblk
c	   for one right side b, but subsequent solutions may be
c	   obtained for additional b-vectors. the arguments are all
c	   as in slvblk.
c
c  dtblok (bloks,integs,nbloks,ipivot,iflag,detsgn,detlog) computes the
c	   determinant of  a  once slvblk or fcblok has done the fact-
c	   orization.the first five arguments are as in slvblk.
c	       detsgn  = sign of the determinant
c	       detlog  = natural log of the determinant
c
c	      ------ block structure of  a  ------
c  the nbloks blocks are stored consecutively in the array  bloks .
c  the first block has its (1,1)-entry at bloks(1), and, if the i-th
c  block has its (1,1)-entry at bloks(index(i)), then
c	  index(i+1) = index(i)  +  nrow(i)*ncol(i) .
c    the blocks are pieced together to give the interesting part of  a
c  as follows.	for i = 1,2,...,nbloks-1, the (1,1)-entry of the next
c  block (the (i+1)st block ) corresponds to the (last+1,last+1)-entry
c  of the current i-th block.  recall last = integs(3,i) and note that
c  this means that
c      a. every block starts on the diagonal of  a .
c      b. the blocks overlap (usually). the rows of the (i+1)st block
c	  which are overlapped by the i-th block may be arbitrarily de-
c	  fined initially. they are overwritten during elimination.
c    the right side for the equations in the i-th block are stored cor-
c  respondingly as the last entries of a piece of  b  of length  nrow
c  (= integs(1,i)) and following immediately in  b  the corresponding
c  piece for the right side of the preceding block, with the right side
c  for the first block starting at  b(1) . in this, the right side for
c  an equation need only be specified once on input, in the first block
c  in which the equation appears.
c
c	      ------ example and test driver ------
c    the test driver for this package contains an example, a linear
c  system of order 11, whose nonzero entries are indicated in the fol-
c  lowing schema by their row and column index modulo 10. next to it
c  are the contents of the  integs  arrray when the matrix is taken to
c  be almost block diagonal with  nbloks = 5, and below it are the five
c  blocks.
c
c		       nrow1 = 3, ncol1 = 4
c	    11 12 13 14
c	    21 22 23 24   nrow2 = 3, ncol2 = 3
c	    31 32 33 34
c  last1 = 2	  43 44 45
c		  53 54 55	      nrow3 = 3, ncol3 = 4
c	 last2 = 3	   66 67 68 69	 nrow4 = 3, ncol4 = 4
c			   76 77 78 79	    nrow5 = 4, ncol5 = 4
c			   86 87 88 89
c		  last3 = 1   97 98 99 90
c		     last4 = 1	 08 09 00 01
c				 18 19 10 11
c			last5 = 4
c
c	  actual input to bloks shown by rows of blocks of  a .
c      (the ** items are arbitrary, this storage is used by slvblk)
c
c  11 12 13 14	/ ** ** **  / 66 67 68 69  / ** ** ** **  / ** ** ** **
c  21 22 23 24 /  43 44 45 /  76 77 78 79 /  ** ** ** ** /  ** ** ** **
c  31 32 33 34/   53 54 55/   86 87 88 89/   97 98 99 90/   08 09 00 01
c							    18 19 10 11
c
c  index = 1	  index = 13  index = 22     index = 34     index = 46
c
c	  actual right side values with ** for arbitrary values
c  b1 b2 b3 ** b4 b5 b6 b7 b8 ** ** b9 ** ** b10 b11
c
c  (it would have been more efficient to combine block 3 with block 4)
c
      integer nbloks
      integer integs(3,nbloks),ipivot(1),iflag
      real bloks(1),b(1),x(1)
c     in the call to fcblok,  x  is used for temporary storage.
      call fcblok(bloks,integs,nbloks,ipivot,x,iflag)
      if (iflag .eq. 0) 		return
      call sbblok(bloks,integs,nbloks,ipivot,b,x)
					return
      end
