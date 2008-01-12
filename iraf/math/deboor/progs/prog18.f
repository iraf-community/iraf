chapter xvi, example 2, taut spline interpolation to the
c  titanium heat data of example xiii.3.
c  from  * a practical guide to splines *  by c. de boor
calls titand,tautsp,ppvalu(interv)
      integer i,iflag,ipick(12),ipicki,k,l,lx,n,npoint
      real break(122),coef(4,22),gamma,gtau(49),gtitan(49),plotf(201)
     *	  ,plott(201),plotts(201),scrtch(119),step,tau(12),x(49)
      real ppvalu
      data n,ipick /12,1,5,11,21,27,29,31,33,35,40,45,49/
      data k,npoint /4,201/
      call titand ( x, gtitan, lx )
      do 10 i=1,n
	 ipicki = ipick(i)
	 tau(i) = x(ipicki)
   10	 gtau(i) = gtitan(ipicki)
      call tautsp(tau,gtau,n,0.,scrtch,break,coef,l,k,iflag)
      if (iflag .gt. 1) 		stop
      step = (tau(n) - tau(1))/float(npoint-1)
      do 20 i=1,npoint
	 plott(i) = tau(1) + step*float(i-1)
   20	 plotf(i) = ppvalu(break,coef,l,k,plott(i),0)
    1 print 601
  601 format(18h gamma = ? (f10.3))
      read 500,gamma
  500 format(f10.3)
      if (gamma .lt. 0.)		stop
      call tautsp(tau,gtau,n,gamma,scrtch,break,coef,l,k,iflag)
      if (iflag .gt. 1) 		stop
      do 30 i=1,npoint
   30	 plotts(i) = ppvalu(break,coef,l,k,plott(i),0)
      print 602,gamma,(plott(i),plotf(i),plotts(i),i=1,npoint)
  602 format(/42h cubic spline vs. taut spline with gamma =,f6.3//
     *	    (f15.7,2e20.8))
					go to 1
      end
