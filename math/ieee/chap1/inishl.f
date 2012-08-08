c
c-----------------------------------------------------------------------
c subroutine: inishl
c   this subroutine initializes the wfta routine for a given
c   value of the transform length n.  the factors of n are
c   determined, the multiplication coefficients are calculated
c   and stored in the array coef(.), the input and output
c   permutation vectors are computed and stored in the arrays
c   indx1(.) and indx2(.)
c
c-----------------------------------------------------------------------
c
      subroutine inishl(n,coef,xr,xi,indx1,indx2,ierr)
      dimension coef(1),xr(1),xi(1)
      integer s1,s2,s3,s4,indx1(1),indx2(1),p1
      dimension co3(3),co4(4),co8(8),co9(11),co16(18),cda(18),cdb(11),
     1cdc(9),cdd(6)
      common na,nb,nc,nd,nd1,nd2,nd3,nd4
c
c   data statements assign short dft coefficients.
c
      data co4(1),co4(2),co4(3),co4(4)/4*1.0/
c
      data cda(1),cda(2),cda(3),cda(4),cda(5),cda(6),cda(7),
     1 cda(8),cda(9),cda(10),cda(11),cda(12),cda(13),cda(14),
     2 cda(15),cda(16),cda(17),cda(18)/18*1.0/
c
      data cdb(1),cdb(2),cdb(3),cdb(4),cdb(5),cdb(6),cdb(7),cdb(8),
     1 cdb(9),cdb(10),cdb(11)/11*1.0/
c
      data ionce/1/
c
c   get multiplier constants
c
      if(ionce.ne.1) go to 20
      call const(co3,co8,co16,co9,cdc,cdd)
20    ionce=-1
c
c   following segment determines factors of n and chooses
c   the appropriate short dft coefficients.
c
      iout=i1mach(2)
      ierr=0
      nd1=1
      na=1
      nb=1
      nd2=1
      nc=1
      nd3=1
      nd=1
      nd4=1
      if(n.le.0 .or. n.gt.5040) go to 190
      if(16*(n/16).eq.n) go to 30
      if(8*(n/8).eq.n) go to 40
      if(4*(n/4).eq.n) go to 50
      if(2*(n/2).ne.n) go to 70
      nd1=2
      na=2
      cda(2)=1.0
      go to 70
30    nd1=18
      na=16
      do 31 j=1,18
31    cda(j)=co16(j)
      go to 70
40    nd1=8
      na=8
      do 41 j=1,8
41    cda(j)=co8(j)
      go to 70
50    nd1=4
      na=4
      do 51 j=1,4
51    cda(j)=co4(j)
70    if(3*(n/3).ne.n) go to 120
      if(9*(n/9).eq.n) go to 100
      nd2=3
      nb=3
      do 71 j=1,3
71    cdb(j)=co3(j)
      go to 120
100   nd2=11
      nb=9
      do 110 j=1,11
110   cdb(j)=co9(j)
120   if(7*(n/7).ne.n) go to 160
      nd3=9
      nc=7
160   if(5*(n/5).ne.n) go to 190
      nd4=6
      nd=5
190   m=na*nb*nc*nd
      if(m.eq.n) go to 250
      write(iout,210)
210   format(21h this n does not work)
      ierr=-1
      return
c
c   next segment generates the dft coefficients by
c   multiplying together the short dft coefficients
c
250   j=1
      do 300 n4=1,nd4
      do 300 n3=1,nd3
      do 300 n2=1,nd2
      do 300 n1=1,nd1
      coef(j)=cda(n1)*cdb(n2)*cdc(n3)*cdd(n4)
      j=j+1
300   continue
c
c   following segment forms the input indexing vector
c
      j=1
      nu=nb*nc*nd
      nv=na*nc*nd
      nw=na*nb*nd
      ny=na*nb*nc
      k=1
      do 440 n4=1,nd
      do 430 n3=1,nc
      do 420 n2=1,nb
      do 410 n1=1,na
405   if(k.le.n) go to 408
      k=k-n
      go to 405
408   indx1(j)=k
      j=j+1
410   k=k+nu
420   k=k+nv
430   k=k+nw
440   k=k+ny
c
c   following segment forms the output indexing vector
c
      m=1
      s1=0
      s2=0
      s3=0
      s4=0
      if(na.eq.1) go to 530
520   p1=m*nu-1
      if((p1/na)*na.eq.p1) go to 510
      m=m+1
      go to 520
510   s1=p1+1
530   if(nb.eq.1) go to 540
      m=1
550   p1=m*nv-1
      if((p1/nb)*nb.eq.p1) go to 560
      m=m+1
      go to 550
560   s2=p1+1
540   if(nc.eq.1) go to 630
      m=1
620   p1=m*nw-1
      if((p1/nc)*nc.eq.p1) go to 610
      m=m+1
      go to 620
610   s3=p1+1
630   if(nd.eq.1) go to 660
      m=1
640   p1=m*ny-1
      if((p1/nd)*nd.eq.p1) go to 650
      m=m+1
      go to 640
650   s4=p1+1
660   j=1
      do 810 n4=1,nd
      do 810 n3=1,nc
      do 810 n2=1,nb
      do 810 n1=1,na
      indx2(j)=s1*(n1-1)+s2*(n2-1)+s3*(n3-1)+s4*(n4-1)+1
900   if(indx2(j).le.n) go to 910
      indx2(j)=indx2(j)-n
      go to 900
910   j=j+1
810   continue
      return
      end
