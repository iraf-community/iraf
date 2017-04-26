       subroutine eqblok ( t, n, kpm,  work1, work2,
     *		       bloks, lenblk, integs, nbloks,  b )
c  from  * a practical guide to splines *  by c. de boor
calls putit(difequ,bsplvd(bsplvb))
c  to be called in  c o l l o c
c
c******  i n p u t  ******
c  t   the knot sequence, of length n+kpm
c  n   the dimension of the approximating spline space, i.e., the order
c      of the linear system to be constructed.
c  kpm = k+m, the order of the approximating spline
c  lenblk   the maximum length of the array  bloks  as allowed by the
c	    dimension statement in  colloc .
c
c******  w o r k   a r e a s  ******
c  work1    used in  putit, of size (kpm,kpm)
c  work2    used in  putit, of size (kpm,m+1)
c
c******  o u t p u t  ******
c  bloks    the coefficient matrix of the linear system, stored in al-
c	    most block diagonal form, of size
c	       kpm*sum(integs(1,i) , i=1,...,nbloks)
c  integs   an integer array, of size (3,nbloks), describing the block
c	    structure.
c	    integs(1,i)  =  number of rows in block  i
c	    integs(2,i)  =  number of columns in block	i
c	    integs(3,i)  =  number of elimination steps which can be
c			carried out in block  i  before pivoting might
c			bring in an equation from the next block.
c  nbloks   number of blocks, equals number of polynomial pieces
c  b   the right side of the linear system, stored corresponding to the
c      almost block diagonal form, of size sum(integs(1,i) , i=1,...,
c      nbloks).
c
c******  m e t h o d  ******
c  each breakpoint interval gives rise to a block in the linear system.
c  this block is determined by the  k  colloc.equations in the interval
c  with the side conditions (if any) in the interval interspersed ap-
c  propriately, and involves the  kpm  b-splines having the interval in
c  their support. correspondingly, such a block has  nrow = k + isidel
c  rows, with  isidel = number of side conditions in this and the prev-
c  ious intervals, and	ncol = kpm  columns.
c     further, because the interior knots have multiplicity  k, we can
c  carry out (in slvblk)  k  elimination steps in a block before pivot-
c  ing might involve an equation from the next block. in the last block,
c  of course, all kpm elimination steps will be carried out (in slvblk).
c
c  see the detailed comments in the solveblok package for further in-
c  formation about the almost block diagonal form used here.
      integer integs(3,1),kpm,lenblk,n,nbloks,	 i,index,indexb,iside
     *					    ,isidel,itermx,k,left,m,nrow
      real b(1),bloks(1),t(1),work1(1),work2(1),   rho,xside
      common /side/ m, iside, xside(10)
      common /other/ itermx,k,rho(19)
      index = 1
      indexb = 1
      i = 0
      iside = 1
      do 20 left=kpm,n,k
	 i = i+1
c	 determine integs(.,i)
	 integs(2,i) = kpm
	 if (left .lt. n)		go to 14
	 integs(3,i) = kpm
	 isidel = m
					go to 16
   14	 integs(3,i) = k
c	 at this point,  iside-1  gives the number of side conditions
c	 incorporated so far. adding to this the side conditions in the
c	 current interval gives the number  isidel .
	 isidel = iside-1
   15	 if (isidel .eq. m)		go to 16
	 if (xside(isidel+1) .ge. t(left+1))
     *					go to 16
	 isidel = isidel+1
					go to 15
   16	 nrow = k + isidel
	 integs(1,i) = nrow
c	 the detailed equations for this block are generated and put
c	 together in  p u t i t .
	 if (lenblk .lt. index+nrow*kpm-1)go to 999
	 call putit(t,kpm,left,work1,work2,bloks(index),nrow,b(indexb))
	 index = index + nrow*kpm
   20	 indexb = indexb + nrow
      nbloks = i
					return
  999 print 699,lenblk
  699 format(11h **********/23h the assigned dimension,i5
     *	      ,38h for	bloks  in  colloc  is too small.)
					stop
      end
