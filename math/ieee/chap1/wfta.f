c
c-----------------------------------------------------------------------
c subroutine: wfta
c winograd fourier transform algorithm
c-----------------------------------------------------------------------
c
      subroutine wfta(xr,xi,n,invrs,init,ierr)
      dimension xr(1),xi(1)
c
c inputs:
c   n-- transform length.  must be formed as the product of
c       relatively prime integers from the set:
c           2,3,4,5,7,8,9,16
c       thus the largest possible value of n is 5040.
c   xr(.)-- array that holds the real part of the data
c           to be transformed.
c   xi(.)-- array that holds the imaginary part of the
c           data to be transformed.
c   invrs-- parameter that flags whether or not the inverse
c           transform is to be calculated.  a division by n
c           is included in the inverse.
c           invrs = 1 yields inverse transform
c           invrs .ne. 1 gives forward transform
c   init-- parameter that flags whether or not the program
c          is to be initialized for this value of n.  the
c          initialization is performed only once in order to
c          to speed up the computation on succeeding calls
c          to the wfta routine, when n is held fixed.
c          init = 0 results in initialization.
c   ierr-- error code that is negative when the wfta
c          terminates incorrectly.
c           0 = successful completion
c          -1 = this value of n does not factor properly
c          -2 = an initialization has not been done for
c               this value of n.
c
c
c   the following two cards may be changed if the maximum
c   desired transform length is less than 5040
c
c  *********************************************************************
      dimension sr(1782),si(1782),coef(1782)
      integer indx1(1008),indx2(1008)
c  *********************************************************************
c
      common na,nb,nc,nd,nd1,nd2,nd3,nd4
c
c   test for initial run
c
      if(init.eq.0) call inishl(n,coef,xr,xi,indx1,indx2,ierr)
c
      if(ierr.lt.0) return
      m=na*nb*nc*nd
      if(m.eq.n) go to 100
      ierr=-2
      return
c
c  error(-2)-- program not initialized for this value of n
c
100   nmult=nd1*nd2*nd3*nd4
c
c   the following code maps the data arrays xr and xi to
c   the working arrays sr and si via the mapping vector
c   indx1(.).  the permutation of the data follows the
c   sino correspondence of the chinese remainder theorem.
c
      j=1
      k=1
      inc1=nd1-na
      inc2=nd1*(nd2-nb)
      inc3=nd1*nd2*(nd3-nc)
      do 140 n4=1,nd
      do 130 n3=1,nc
      do 120 n2=1,nb
      do 110 n1=1,na
      ind=indx1(k)
      sr(j)=xr(ind)
      si(j)=xi(ind)
      k=k+1
110   j=j+1
120   j=j+inc1
130   j=j+inc2
140   j=j+inc3
c
c   do the pre-weave modules
c
      call weave1(sr,si)
c
c   the following loop performs all the multiplications of the
c   winograd fourier transform algorithm.  the multiplication
c   coefficients are stored on the initialization pass in the
c   array coef(.).
c
      do 200 j=1,nmult
      sr(j)=sr(j)*coef(j)
      si(j)=si(j)*coef(j)
  200  continue
c
c   do the post-weave modules
c
      call weave2(sr,si)
c
c
c   the following code maps the working arrays sr and si
c   to the data arrays xr and xi via the mapping vector
c   indx2(.).  the permutation of the data follows the
c   chinese remainder theorem.
c
      j=1
      k=1
      inc1=nd1-na
      inc2=nd1*(nd2-nb)
      inc3=nd1*nd2*(nd3-nc)
c
c   check for inverse
c
      if(invrs.eq.1) go to 400
      do 340 n4=1,nd
      do 330 n3=1,nc
      do 320 n2=1,nb
      do 310 n1=1,na
      kndx=indx2(k)
      xr(kndx)=sr(j)
      xi(kndx)=si(j)
      k=k+1
310   j=j+1
320   j=j+inc1
330   j=j+inc2
340   j=j+inc3
      return
c
c   different permutation for the inverse
c
400   fn=float(n)
      np2=n+2
      indx2(1)=n+1
      do 440 n4=1,nd
      do 430 n3=1,nc
      do 420 n2=1,nb
      do 410 n1=1,na
      kndx=np2-indx2(k)
      xr(kndx)=sr(j)/fn
      xi(kndx)=si(j)/fn
      k=k+1
410   j=j+1
420   j=j+inc1
430   j=j+inc2
440   j=j+inc3
      return
      end
