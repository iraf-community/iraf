c
c-----------------------------------------------------------------------
c main program: test program to exercise the wfta subroutine
c   the test waveform is a complex exponential a**i whose
c   transform is known analytically to be (1 - a**n)/(1 - a*w**k).
c
c authors:
c   james h. mcclellan     and     hamid nawab
c   department of electrical engineering and computer science
c   massachusetts of technology
c   cambridge, mass.  02139
c
c inputs:
c   n-- transform length. it must be formed as the product of
c       relatively prime integers from the set:
c           2,3,4,5,7,8,9,16
c   invrs is the flag for forward or inverse transform.
c           invrs = 1 yields inverse transform
c           invrs .ne. 1 gives forward transform
c   rad and phi are the magnitude and angle (as a fraction of
c       2*pi/n) of the complex exponential test signal.
c          suggestion: rad = 0.98, phi = 0.5.
c-----------------------------------------------------------------------
c
      double precision pi2,pin,xn,xj,xt
      dimension xr(1260),xi(1260)
      complex cone,ca,can,cnum,cden
c
c   output will be punched
c
      iout=i1mach(3)
      input=i1mach(1)
      cone=cmplx(1.0,0.0)
      pi2=8.0d0*datan(1.0d0)
50    continue
      read(input,130)n
130   format(i5)
      write(iout,150) n
150   format(10h length = ,i5)
      if(n.le.0 .or. n.gt.1260) stop
c
c   enter a 1 to perform the inverse
c
c     read(input,130) invrs
      invrs = 0
c
c   enter magnitude and angle (in fraction of 2*pi/n)
c   avoid multiples of n for the angle if the radius is
c   close to one.  suggestion: rad = 0.98, phi = 0.5.
c
c     read(input,160) rad,phi
      rad = 0.98
      phi = 0.5
160   format(2f15.10)
      xn=float(n)
      pin=phi
      pin=pin*pi2/xn
c
c   generate z**j
c
      init=0
      do 200 j=1,n
      an=rad**(j-1)
      xj=j-1
      xj=xj*pin
      xt=dcos(xj)
      xr(j)=xt
      xr(j)=xr(j)*an
      xt=dsin(xj)
      xi(j)=xt
      xi(j)=xi(j)*an
200   continue
      can=cmplx(xr(n),xi(n))
      ca=cmplx(xr(2),xi(2))
      can=can*ca
c
c   print first 50 values of input sequence
c
      max=50
      if(n.lt.50)max=n
      write(iout,300)(j,xr(j),xi(j),j=1,max)
c
c   call the winograd fourier transform algorithm
c
      call wfta(xr,xi,n,invrs,init,ierr)
c
c   check for error return
c
      if(ierr.lt.0) write(iout,250) ierr
250   format(1x,5herror,i5)
      if(ierr.lt.0) go to 50
c
c   print first 50 values of the transformed sequence
c
      write(iout,300)(j,xr(j),xi(j),j=1,max)
300   format(1x,3hj =,i3,6hreal =,e20.12,6himag =,e20.12)
c
c   calculate absolute and relative deviations
c
      devabs=0.0
      devrel=0.0
      cnum=cone-can
      pin=pi2/xn
      do 350 j=1,n
      xj=j-1
      xj=-xj*pin
      if(invrs.eq.1) xj=-xj
      tr=dcos(xj)
      ti=dsin(xj)
      can=cmplx(tr,ti)
      cden=cone-ca*can
      cden=cnum/cden
c
c   true value of the transform (1. - a**n)/(1. - a*w**k),
c   where a = rad*exp(j*phi*(2*pi/n)), w = exp(-j*2*pi/n).
c   for the inverse transform the complex exponential w
c   is conjugated.
c
      tr=real(cden)
      ti=aimag(cden)
      if(invrs.ne.1) go to 330
c
c   scale inverse transform by 1/n
c
      tr=tr/float(n)
      ti=ti/float(n)
330   tr=xr(j)-tr
      ti=xi(j)-ti
      devabs=sqrt(tr*tr+ti*ti)
      xmag=sqrt(xr(j)*xr(j)+xi(j)*xi(j))
      devrel=100.0*devabs/xmag
      if(devabs.le.devmx1)go to 340
      devmx1=devabs
      labs=j-1
340   if(devrel.le.devmx2)go to 350
      devmx2=devrel
      lrel=j-1
350   continue
c
c   print the absolute and relative deviations together
c   with their locations.
c
      write(iout,380) devabs,labs,devrel,lrel
380   format(1x,21habsolute deviation = ,e20.12,9h at index,i5/
     1 1x,21hrelative deviation = ,f11.7,8h percent,1x,9h at index,i5)
      go to 50
      end
