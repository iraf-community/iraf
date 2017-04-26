c  main program for least-squares approximation by splines
c  from  * a practical guide to splines *  by c. de boor
calls setdat,l2knts,l2appr(bsplvb,bchfac,bchslv),bsplpp(bsplvb*)
c     ,l2err(ppvalu(interv)),ppvalu*,newnot
c
c  the program, though ostensibly written for l2-approximation, is typ-
c  ical for programs constructing a pp approximation to a function gi-
c  ven in some sense. the subprogram  l 2 a p p r , for instance, could
c  easily be replaced by one carrying out interpolation or some other
c  form of approximation.
c
c******  i n p u t  ******
c  is expected in  s e t d a t	(quo vide), specifying both the data to
c  be approximated and the order and breakpoint sequence of the pp ap-
c  proximating function to be used. further,  s e t d a t  is expected
c  to  t e r m i n a t e  the run (for lack of further input or because
c   i c o u n t  has reached a critical value).
c     the number  n t i m e s  is read in in the main program. it speci
c  fies the number of passes through the knot improvement algorithm in
c  n e w n o t	to be made. also,  a d d b r k	is read in to specify
c  that, on the average, addbrk knots are to be added per pass through
c  newnot. for example,  addbrk = .34  would cause a knot to be added
c  every third pass (as long as  ntimes .lt. 50).
c
c******  p r i n t e d	o u t p u t  ******
c  is governed by the three print control hollerith strings
c  p r b c o  = 2hon  gives printout of b-spline coeffs. of approxim.
c  p r p c o  = 2hon  gives printout of pp repr. of approximation.
c  p r f u n  = 2hon  gives printout of approximation and error at
c		      every data point.
c  the order  k , the number of pieces	l, and the interior breakpoints
c  are always printed out as are (in l2err) the mean, mean square, and
c  maximum errors in the approximation.
c
c     parameter lpkmax=100,ntmax=200,ltkmax=2000
      integer i,icount,ii,j,k,l,lbegin,lnew,ll,n,nt,ntimes,ntau
     *	     ,on,prbco,prfun,prpco
c     real addbrk,bcoef(lpkmax),break,coef,gtau,q(ltkmax),scrtch(ntmax)
c    *	  ,t(ntmax),tau,totalw,weight
c     common / data / ntau, tau(ntmax),gtau(ntmax),weight(ntmax),totalw
      real addbrk,bcoef(100),break,coef,gtau,q(2000),scrtch(200)
     *	  ,t(200),tau,totalw,weight
      common / data / ntau, tau(200),gtau(200),weight(200),totalw
c     common /data/ also occurs in setdat, l2appr and l2err. it is ment-
c     ioned here only because it might otherwise become undefined be-
c     tween calls to those subroutines.
c     common /approx/ break(lpkmax),coef(ltkmax),l,k
      common /approx/ break(100),coef(2000),l,k
c     common /approx/ also occurs in setdat and l2err.
      data on /2hon /
c
      icount = 0
c	 i c o u n t  provides communication with the data-input-and-
c     termination routine  s e t d a t . it is initialized to  0  to
c     signal to setdat when it is being called for the first time. after
c     that, it is up to setdat to use icount for keeping track of the
c     passes through setdat .
c
c     information about the function to be approximated and order and
c     breakpoint sequence of the approximating pp functions is gathered
c     by a
    1 call setdat(icount)
c
c     breakpoints are translated into knots, and the number  n	of
c     b-splines to be used is obtained by a
      call l2knts ( break, l, k, t, n )
c
c     the integer  n t i m e s	and the real  a d d b r k  are requested
c     as well as the print controls  p r b c o ,  p r p c o  and
c     p r f u n .  ntimes  passes  are made through the subroutine new-
c     not, with an increase of	addbrk	knots for every pass .
      print 600
  600 format(52h ntimes,addbrk , prbco,prpco,prfun =? (i3,f10.5/3a2))
      read 500,ntimes,addbrk,prbco,prpco,prfun
  500 format(i3,f10.5/3a2)
c
      lbegin = l
      nt = 1
c	 the b-spline coeffs.  b c o e f  of the l2-approx. are obtain-
c	 ed by a
   10	 call l2appr ( t, n, k, q, scrtch, bcoef )
	 if (prbco .eq. on)  print 609, (bcoef(i),i=1,n)
  609	 format(//22h b-spline coefficients/(5e16.9))
c
c	 convert the b-repr. of the approximation to pp repr.
	 call bsplpp ( t, bcoef, n, k, q, break, coef, l )
	 print 610, k, l, (break(ll),ll=2,l)
  610	 format(//34h approximation by splines of order,i3,4h on ,
     *	       i3,25h intervals. breakpoints -/(5e16.9))
	 if (prpco .ne. on)		go to 15
	 print 611
  611	 format(/36h pp-representation for approximation)
	 do 12 i=1,l
	    ii = (i-1)*k
   12	    print 613,break(i),(coef(ii+j),j=1,k)
  613	 format(f9.3,5e16.9/(11x,5e16.9))
c
c	 compute and print out various error norms by a
   15	 call l2err ( prfun, scrtch, q )
c
c	 if newnot has been applied less than  n t i m e s  times, try
c	 it again to obtain, from the current approx. a possibly improv-
c	 ed sequence of breakpoints with  addbrk  more breakpoints (on
c	 the average) than the current approximation has.
c	    if only an increase in breakpoints is wanted, without the
c	 adjustment that newnot provides, a fake newnot routine could be
c	 used here which merely returns the breakpoints for  l n e w
c	 equal intervals .
	 if (nt .ge. ntimes)		go to 1
	 lnew = lbegin + float(nt)*addbrk
	 call newnot (break, coef, l, k, scrtch, lnew, t )
	 call l2knts ( scrtch, lnew, k, t, n )
	 nt = nt + 1
					go to 10
      end
