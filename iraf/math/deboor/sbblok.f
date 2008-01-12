      subroutine sbblok ( bloks, integs, nbloks, ipivot, b, x )
calls subroutines  s u b f o r	and  s u b b a k .
c
c  supervises the solution (by forward and backward substitution) of
c  the linear system  a*x = b  for x, with the plu factorization of  a
c  already generated in  f c b l o k .	individual blocks of equations
c  are solved via  s u b f o r	and  s u b b a k .
c
c parameters
c    bloks, integs, nbloks, ipivot    are as on return from fcblok.
c    b	     the right side, stored corresponding to the storage of
c	     the equations. see comments in  s l v b l k  for details.
c    x	     solution vector
c
      integer nbloks
      integer integs(3,nbloks),ipivot(1), i,index,indexb,indexx,j,last,
     *	      nbp1,ncol,nrow
      real bloks(1),b(1),x(1)
c
c      forward substitution pass
c
      index = 1
      indexb = 1
      indexx = 1
      do 20 i=1,nbloks
	 nrow = integs(1,i)
	 last = integs(3,i)
	 call subfor(bloks(index),ipivot(indexb),nrow,last,b(indexb),
     *		     x(indexx))
	 index = nrow*integs(2,i) + index
	 indexb = indexb + nrow
   20	 indexx = indexx + last
c
c     back substitution pass
c
      nbp1 = nbloks + 1
      do 30 j=1,nbloks
	 i = nbp1 - j
	 nrow = integs(1,i)
	 ncol = integs(2,i)
	 last = integs(3,i)
	 index = index - nrow*ncol
	 indexb = indexb - nrow
	 indexx = indexx - last
   30	 call subbak(bloks(index),ipivot(indexb),nrow,ncol,last,
     *		     x(indexx))
					return
      end
