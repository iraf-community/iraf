      subroutine splopt ( tau, n, k, scrtch, t, iflag )
c  from  * a practical guide to splines *  by c. de boor
calls bsplvb, banfac/slv
computes the knots  t  for the optimal recovery scheme of order  k
c  for data at	tau(i), i=1,...,n .
c
c******  i n p u t  ******
c  tau.....array of length  n , containing the interpolation points.
c     a s s u m e d  to be nondecreasing, with tau(i).lt.tau(i+k),all i.
c  n.....number of data points .
c  k.....order of the optimal recovery scheme to be used .
c
c******  w o r k  a r r a y  *****
c  scrtch.....array of length  (n-k)(2k+3) + 5k + 3 . the various
c	 contents are specified in the text below .
c
c******  o u t p u t  ******
c  iflag.....integer indicating success (=1) or failure (=2) .
c     if iflag = 1, then
c  t.....array of length  n+k  containing the optimal knots ready for
c	 use in optimal recovery. specifically,  t(1) = ... = t(k) =
c	 tau(1)  and  t(n+1) = ... = t(n+k) = tau(n) , while the  n-k
c	 interior knots  t(k+1), ..., t(n)  are calculated as described
c	 below under  *method* .
c     if iflag = 2, then
c	 k .lt. 3, or n .lt. k, or a certain linear system was found to
c	 be singular.
c
c******  p r i n t e d	o u t p u t  ******
c  a comment will be printed in case  ilfag = 2  or newton iterations
c  failed to converge in  n e w t m x  iterations .
c
c******  m e t h o d  ******
c     the (interior) knots  t(k+1), ..., t(n)  are determined by newtons
c  method in such a way that the signum function which changes sign at
c   t(k+1), ..., t(n)  and nowhere else in  (tau(1),tau(n)) is orthogon-
c  al to the spline space  spline( k , tau )  on that interval .
c     let  xi(j)  be the current guess for  t(k+j), j=1,...,n-k. then
c  the next newton iterate is of the form
c	       xi(j)  +  (-)**(n-k-j)*x(j)  ,  j=1,...,n-k,
c  with  x  the solution of the linear system
c			 c*x  =  d  .
c  here,  c(i,j) = b(i)(xi(j)), all j, with  b(i)  the i-th b-spline of
c  order  k  for the knot sequence  tau , all i, and  d  is the vector
c  given by  d(i) = sum( -a(j) , j=i,...,n )*(tau(i+k)-tau(i))/k, all i,
c  with  a(i) = sum ( (-)**(n-k-j)*b(i,k+1,tau)(xi(j)) , j=1,...,n-k )
c  for i=1,...,n-1, and  a(n) = -.5 .
c     (see chapter  xiii  of text and references there for a derivation)
c     the first guess for  t(k+j)  is  (tau(j+1)+...+tau(j+k-1))/(k-1) .
c     iteration terminates if  max(abs(x(j))) .lt. t o l  , with
c		  t o l  =  t o l r t e *(tau(n)-tau(1))/(n-k) ,
c  or else after  n e w t m x  iterations , currently,
c		  newtmx, tolrte / 10, .000001
c
      integer iflag,k,n,   i,id,index,j,km1,kpk,kpkm1,kpn,kp1,l,left
     *,leftmk,lenw,ll,llmax,llmin,na,nb,nc,nd,newtmx,newton,nmk,nmkm1,nx
      real scrtch(1),t(1),tau(n),   del,delmax,floatk,sign,signst,sum
     *				   ,tol,tolrte,xij
c     dimension scrtch((n-k)*(2*k+3)+5*k+3), t(n+k)
current fortran standard makes it impossible to specify the precise dim-
c  ensions of  scrtch  and  t  without the introduction of otherwise
c  superfluous additional arguments .
      data newtmx,tolrte / 10,.000001/
      nmk = n-k
      if (nmk)				1,56,2
    1 print 601,n,k
  601 format(13h argument n =,i4,29h in  splopt  is less than k =,i3)
					go to 999
    2 if (k .gt. 2)			go to 3
      print 602,k
  602 format(13h argument k =,i3,27h in  splopt  is less than 3)
					go to 999
   3  nmkm1 = nmk - 1
      floatk = k
      kpk = k+k
      kp1 = k+1
      km1 = k-1
      kpkm1 = kpk-1
      kpn = k+n
      signst = -1.
      if (nmk .gt. (nmk/2)*2)  signst = 1.
c  scrtch(i) = tau-extended(i), i=1,...,n+k+k
      nx = n+kpk+1
c  scrtch(i+nx) = xi(i),i=0,...,n-k+1
      na = nx + nmk + 1
c  scrtch(i+na) = -a(i), i=1,...,n
      nd = na + n
c  scrtch(i+nd) = x(i) or d(i), i=1,...,n-k
      nb = nd + nmk
c  scrtch(i+nb) = biatx(i),i=1,...,k+1
      nc = nb + kp1
c  scrtch(i+(j-1)*(2k-1) + nc) = w(i,j) = c(i-k+j,j), i=j-k,...,j+k,
c						      j=1,...,n-k.
      lenw = kpkm1*nmk
c  extend  tau	to a knot sequence and store in scrtch.
      do 5 j=1,k
	 scrtch(j) = tau(1)
    5	 scrtch(kpn+j) = tau(n)
      do 6 j=1,n
    6	 scrtch(k+j) = tau(j)
c  first guess for  scrtch (.+nx)  =  xi .
      scrtch(nx) = tau(1)
      scrtch(nmk+1+nx) = tau(n)
      do 10 j=1,nmk
	 sum = 0.
	 do 9 l=1,km1
    9	    sum = sum + tau(j+l)
   10	 scrtch(j+nx) = sum/float(km1)
c  last entry of  scrtch (.+na)  =  - a  is always ...
      scrtch(n+na) = .5
c  start newton iteration.
      newton = 1
      tol = tolrte*(tau(n) - tau(1))/float(nmk)
c  start newton step
compute the 2k-1 bands of the matrix c and store in scrtch(.+nc),
c  and compute the vector  scrtch(.+na) = -a.
   20 do 21 i=1,lenw
   21	 scrtch(i+nc) = 0.
      do 22 i=2,n
   22	 scrtch(i-1+na) = 0.
      sign = signst
      left = kp1
      do 28 j=1,nmk
	 xij = scrtch(j+nx)
   23	    if (xij .lt. scrtch(left+1))go to 25
	    left = left + 1
	    if (left .lt. kpn)		go to 23
	    left = left - 1
   25	 call bsplvb(scrtch,k,1,xij,left,scrtch(1+nb))
c	 the tau sequence in scrtch is preceded by  k  additional knots
c	 therefore,  scrtch(ll+nb)  now contains  b(left-2k+ll)(xij)
c	 which is destined for	c(left-2k+ll,j), and therefore for
c	     w(left-k-j+ll,j)= scrtch(left-k-j+ll + (j-1)*kpkm1 + nc)
c	 since we store the 2k-1 bands of  c  in the 2k-1  r o w s  of
c	 the work array w, and	w  in turn is stored in  s c r t c h ,
c	 with  w(1,1) = scrtch(1 + nc) .
c	     also, c  being of order  n-k, we would want  1 .le.
c	 left-2k+ll .le. n-k  or
c	    llmin = 2k-left  .le.  ll  .le.  n-left+k = llmax .
	 leftmk = left-k
	 index = leftmk-j + (j-1)*kpkm1 + nc
	 llmin = max0(1,k-leftmk)
	 llmax = min0(k,n-leftmk)
	 do 26 ll=llmin,llmax
   26	    scrtch(ll+index) = scrtch(ll+nb)
	 call bsplvb(scrtch,kp1,2,xij,left,scrtch(1+nb))
	 id = max0(0,leftmk-kp1)
	 llmin = 1 - min0(0,leftmk-kp1)
	 do 27 ll=llmin,kp1
	    id = id + 1
   27	     scrtch(id+na) = scrtch(id+na) - sign*scrtch(ll+nb)
   28	 sign = -sign
      call banfac(scrtch(1+nc),kpkm1,nmk,km1,km1,iflag)
					go to (45,44),iflag
   44 print 644
  644 format(32h c in  splopt  is not invertible)
					return
compute  scrtch (.+nd) =  d  from  scrtch (.+na) = - a .
   45 i=n
   46	 scrtch(i-1+na) = scrtch(i-1+na) + scrtch(i+na)
	 i = i-1
	 if (i .gt. 1)			go to 46
      do 49 i=1,nmk
   49	 scrtch(i+nd) = scrtch(i+na)*(tau(i+k)-tau(i))/floatk
compute  scrtch (.+nd) =  x .
      call banslv(scrtch(1+nc),kpkm1,nmk,km1,km1,scrtch(1+nd))
compute  scrtch (.+nd) = change in  xi . modify, if necessary, to
c  prevent new	xi  from moving more than 1/3 of the way to its
c  neighbors. then add to  xi  to obtain new  xi  in scrtch(.+nx).
      delmax = 0.
      sign = signst
      do 53 i=1,nmk
	 del = sign*scrtch(i+nd)
	 delmax = amax1(delmax,abs(del))
	 if (del .gt. 0.)		go to 51
	 del = amax1(del,(scrtch(i-1+nx)-scrtch(i+nx))/3.)
					go to 52
   51	 del = amin1(del,(scrtch(i+1+nx)-scrtch(i+nx))/3.)
   52	 sign = -sign
   53	 scrtch(i+nx) = scrtch(i+nx) + del
call it a day in case change in  xi  was small enough or too many
c  steps were taken.
      if (delmax .lt. tol)		go to 54
      newton = newton + 1
      if (newton .le. newtmx)		go to 20
      print 653,newtmx
  653 format(33h no convergence in  splopt  after,i3,14h newton steps.)
   54 do 55 i=1,nmk
   55	 t(k+i) = scrtch(i+nx)
   56 do 57 i=1,k
	 t(i) = tau(1)
   57	 t(n+i) = tau(n)
					return
  999 iflag = 2
					return
      end
