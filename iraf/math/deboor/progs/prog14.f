chapter xiii, example 3 . test of optimal spline interpolation routine
c			  on titanium heat data .
c  from  * a practical guide to splines *  by c. de boor
calls titand,splopt(bsplvb,banfac/slv),splint(*),bvalue(interv)
c
c     lenscr = (n-k)(2k+3)+5k+3  is the length of scrtch required in
c				 splopt .
c     parameter n=12,ntitan=49,k=5,npk=17,lenscr=119
c     integer i,iflag,ipick(n),ipicki,lx,nmk
c     real a(n),gtitan(ntitan),gtau(ntitan),scrtch(lenscr),t(npk),tau(n)
c    *	  ,x(ntitan)
      real bvalue
      integer i,iflag,ipick(12),ipicki,lx,nmk
      real a(12),gtitan(49),gtau(49),scrtch(119),t(17),tau(12)
     *	  ,x(49)
      data n,k /12,5/
      data ipick /1,5,11,21,27,29,31,33,35,40,45,49/
      call titand ( x, gtitan, lx )
      do 10 i=1,n
	 ipicki = ipick(i)
	 tau(i) = x(ipicki)
   10	 gtau(i) = gtitan(ipicki)
      call splopt ( tau, n, k, scrtch, t, iflag )
      if (iflag .gt. 1) 		stop
      call splint ( tau, gtau, t, n, k, scrtch, a, iflag )
      if (iflag .gt. 1) 		stop
      do 20 i=1,lx
	 gtau(i) = bvalue ( t, a, n, k, x(i), 0 )
   20	 scrtch(i) = gtitan(i) - gtau(i)
      print 620,(i,x(i),gtitan(i),gtau(i),scrtch(i),i=1,lx)
  620 format(41h  i, data point, data, interpolant, error//
     2	       (i3,f8.0,f10.4,f9.4,e11.3))
      nmk = n-k
      print 621,(i,t(k+i),i=1,nmk)
  621 format(///16h optimal knots =/(i5,f15.9))
					stop
      end
