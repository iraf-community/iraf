      subroutine setdt3(icount)
c  from  * a practical guide to splines *  by c. de boor
c  to be called in main program  l 2 m a i n .
c	calls titand
c     this routine is set up to provide the specific data for example 4
c     in chapter xiv.
      integer icount,  i,k,l,n,ntau
      real break,brkpic(9),coef,gtau,tau,totalw,weight
c     parameter lpkmax=100,ntmax=200,ltkmax=2000
c     common / data / ntau, tau(ntmax),gtau(ntmax),weight(ntmax),totalw
c     common /approx/ break(lpkmax),coef(ltkmax),l,k
      common / data / ntau, tau(200),gtau(200),weight(200),totalw
      common /approx/ break(100),coef(2000),l,k
      data brkpic,n/595.,730.985,794.414,844.476,880.06,907.814,
     *		    938.001,976.752,1075.,9/
      if (icount .gt. 0)		stop
      icount = icount + 1
      call titand ( tau, gtau, ntau )
      do 10 i=1,ntau
   10	 weight(i) = 1.
      totalw = ntau
      l = n-1
      k = 5
      do 11 i=1,n
   11	 break(i) = brkpic(i)
					return
      end
