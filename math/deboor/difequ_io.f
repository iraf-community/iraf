      subroutine difequ ( mode, xx, v )
c  from  * a practical guide to splines *  by c. de boor
calls  ppvalu(interv)
c  to be called by   c o l l o c ,  p u t i t
c  information about the differential equation is dispensed from here
c
c******  i n p u t  ******
c  mode  an integer indicating the task to be performed.
c	 = 1   initialization
c	 = 2   evaluate  de  at  xx
c	 = 3   specify the next side condition
c	 = 4   analyze the approximation
c  xx a point at which information is wanted
c
c******  o u t p u t  ******
c  v  depends on the  mode  . see comments below
c
c     parameter npiece=100,ncoef=2000
      integer mode,   i,iside,itermx,k,kpm,l,m
      real v(20),xx,   break,coef,eps,ep1,ep2,error,factor,rho,solutn
     *		      ,s2ovep,un,x,xside
      real ppvalu
c     common /approx/ break(npiece),coef(ncoef),l,kpm
      common /approx/ break(100),coef(2000),l,kpm
      common /side/ m,iside,xside(10)
      common /other/ itermx,k,rho(19)
c
c  this sample of  difequ  is for the example in chapter xv. it is a
c  nonlinear second order two point boundary value problem.
c
					go to (10,20,30,40),mode
c  initialize everything
c  i.e. set the order  m  of the dif.equ., the nondecreasing sequence
c  xside(i),i=1,...,m, of points at which side cond.s are given and
c  anything else necessary.
   10 m = 2
      xside(1) = 0.
      xside(2) = 1.
c  *** print out heading
      print 499
  499 format(37h carrier,s nonlinear perturb. problem)
      eps = .5e-2
      print 610, eps
  610 format(5h eps ,e20.10)
c  *** set constants used in formula for solution below.
      factor = (sqrt(2.) + sqrt(3.))**2
      s2ovep = sqrt(2./eps)
c  *** initial guess for newton iteration. un(x) = x*x - 1.
      l = 1
      break(1) = 0.
      do 16 i=1,kpm
   16	 coef(i) = 0.
      coef(1) = -1.
      coef(3) = 2.
      itermx = 10
					return
c
c  provide value of left side coeff.s and right side at  xx .
c  specifically, at  xx  the dif.equ. reads
c	 v(m+1)d**m + v(m)d**(m-1) + ... + v(1)d**0  =	v(m+2)
c  in terms of the quantities v(i),i=1,...,m+2, to be computed here.
   20 continue
      v(3) = eps
      v(2) = 0.
      un = ppvalu(break,coef,l,kpm,xx,0)
      v(1) = 2.*un
      v(4) = un**2 + 1.
					return
c
c  provide the	m  side conditions. these conditions are of the form
c	 v(m+1)d**m + v(m)d**(m-1) + ... + v(1)d**0  =	v(m+2)
c  in terms of the quantities v(i),i=1,...,m+2, to be specified here.
c  note that v(m+1) = 0  for customary side conditions.
   30 v(m+1) = 0.
					go to (31,32,39),iside
   31 v(2) = 1.
      v(1) = 0.
      v(4) = 0.
					go to 38
   32 v(2) = 0.
      v(1) = 1.
      v(4) = 0.
   38 iside = iside + 1
   39					return
c
c  calculate the error near the boundary layer at  1.
   40 continue
      print 640
  640 format(44h x, g(x)  and  g(x)-f(x)  at selected points)
      x = .75
      do 41 i=1,9
	 ep1 = exp(s2ovep*(1.-x))*factor
	 ep2 = exp(s2ovep*(1.+x))*factor
	 solutn = 12./(1.+ep1)**2*ep1 + 12./(1.+ep2)**2*ep2 - 1.
	 error = solutn - ppvalu(break,coef,l,kpm,x,0)
	 print 641,x,solutn,error
  641	 format(3e20.10)
   41	 x = x + .03125
					return
      end
