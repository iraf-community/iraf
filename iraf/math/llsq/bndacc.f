      subroutine bndacc (g,mdg,nb,ip,ir,mt,jt)
c     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 jun 12
c     to appear in 'solving least squares problems', prentice-hall, 1974
c	   sequential algorithm for banded least squares problem..
c	   accumulation phase.	    for solution phase use bndsol.
c
c     the calling program must set ir=1 and ip=1 before the first call
c     to bndacc for a new case.
c
c     the second subscript of g( ) must be dimensioned at least
c     nb+1 in the calling program.
      dimension g(mdg,1)
      zero=0.
c
c	       alg. steps 1-4 are performed external to this subroutine.
c
      nbp1=nb+1
      if (mt.le.0) return
c					      alg. step 5
      if (jt.eq.ip) go to 70
c					      alg. steps 6-7
      if (jt.le.ir) go to 30
c					      alg. steps 8-9
      do 10 i=1,mt
	ig1=jt+mt-i
	ig2=ir+mt-i
	do 10 j=1,nbp1
   10	g(ig1,j)=g(ig2,j)
c					      alg. step 10
      ie=jt-ir
      do 20 i=1,ie
	ig=ir+i-1
	do 20 j=1,nbp1
   20	g(ig,j)=zero
c					      alg. step 11
      ir=jt
c					      alg. step 12
   30 mu=min0(nb-1,ir-ip-1)
      if (mu.eq.0) go to 60
c					      alg. step 13
      do 50 l=1,mu
c					      alg. step 14
	k=min0(l,jt-ip)
c					      alg. step 15
	lp1=l+1
	ig=ip+l
	do 40 i=lp1,nb
	  jg=i-k
   40	  g(ig,jg)=g(ig,i)
c					      alg. step 16
	do 50 i=1,k
	jg=nbp1-i
   50	g(ig,jg)=zero
c					      alg. step 17
   60 ip=jt
c					      alg. steps 18-19
   70 mh=ir+mt-ip
      kh=min0(nbp1,mh)
c					      alg. step 20
      do 80 i=1,kh
   80	call h12 (1,i,max0(i+1,ir-ip+1),mh,g(ip,i),1,rho,
     1		  g(ip,i+1),1,mdg,nbp1-i)
c					      alg. step 21
      ir=ip+kh
c					      alg. step 22
      if (kh.lt.nbp1) go to 100
c					      alg. step 23
      do 90 i=1,nb
   90	g(ir-1,i)=zero
c					      alg. step 24
  100 continue
c					      alg. step 25
      return
      end
