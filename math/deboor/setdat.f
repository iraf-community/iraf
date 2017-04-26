      subroutine setdat(icount)
c  from  * a practical guide to splines *  by c. de boor
c  to be called in main program  l 2 m a i n .
c     this routine is set up to provide the specific data for example 2
c     in chapter xiv. for a general purpose l2-approximation program, it
c     would have to be replaced by a subroutine reading in
c	    ntau, tau(i), gtau(i), i=1,...,ntau
c     and reading in or setting
c	     k, l, break(i),i=1,...,l+1,  and weight(i),i=1,...,ntau,
c	    as well as	totalw = sum( weight(i) , i=1,...,ntau).
c    i c o u n t  is equal to zero when setdat is called in  l 2 m a i n
c     for the first time. after that, it is up to setdat to use icount
c     for keeping track of the passes through setdat . this is important
c     since l2main relies on setdat for  t e r m i n a t i o n .
      integer icount,  i,k,l,lp1,ntau,ntaum1
      real break,coef,gtau,step,tau,totalw,weight
c     parameter lpkmax=100,ntmax=200,ltkmax=2000
c     common / data / ntau, tau(ntmax),gtau(ntmax),weight(ntmax),totalw
c     common /approx/ break(lpkmax),coef(ltkmax),l,k
      common / data / ntau, tau(200),gtau(200),weight(200),totalw
      common /approx/ break(100),coef(2000),l,k
      if (icount .gt. 0)		stop
      icount = icount + 1
      ntau = 10
      ntaum1 = ntau-1
      do 8 i=1,ntaum1
    8	 tau(i) = 1. - .5**(i-1)
      tau(ntau) = 1.
      do 9 i=1,ntau
    9	 gtau(i) = tau(i)**2  + 1.
      do 10 i=1,ntau
   10	 weight(i) = 1.
      totalw = ntau
      l = 6
      lp1 = l+1
      step = 1./float(l)
      k = 2
      do 11 i=1,lp1
   11	 break(i) = (i-1)*step
					return
      end
