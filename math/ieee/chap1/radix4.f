c
c-----------------------------------------------------------------------
c subroutine:  radix4
c computes forward or inverse complex dft via radix-4 fft.
c uses autogen technique to yield time efficient program.
c-----------------------------------------------------------------------
c
        subroutine radix4(mm,iflag,jflag)
c
c       input:
c             mm = power of 4 (i.e., n = 4**mm complex point transform)
c             (mm.ge.2 and mm.le.5)
c
c          iflag = 1 on first pass for given n
c                = 0 on subsequent passes for given n
c
c          jflag = -1 for forward transform
c                = +1 for inverse transform
c
c       input/output:
c              a = array of dimensions 2*n with real and imaginary parts
c                  of dft input/output in odd, even array components.
c
c       for optimal time efficiency, common is used to pass arrays.
c       this means that dimensions of arrays a, ix, and t can be
c       modified to reflect maximum value of n = 4**mm to be used. note
c       that array "ix" is also dimensioned in subroutine "rad4sb".
c
c    i.e.,    a(    ) ix(  ) t(   )
c
c       m =2      32     38     27
c       m<=3     128    144    135
c       m<=4     512    658    567
c       m<=5    2048   2996   2295
c
       dimension a(2048),ix(2996),t(2295)
       dimension nfac(11),np(209)
       common ntypl,kkp,index,ixc
       common /aa/a
       common /xx/ix
c
c      check for mm<2 or mm>5
c
       if(mm.lt.2.or.mm.gt.5)stop
c
c      initialize on first pass """"""""""""""""""""""""""""""""""""""""
c
       if(iflag.eq.1) go to 9999
c
c      fast fourier transform start ####################################
c
8885   kspan=2*4**mm
       if(jflag.eq.1) go to 8887
c
c      conjugate data for forward transform
c
       do 8886 j=2,n2,2
8886   a(j)=-a(j)
       go to 8889
c
c      multiply data by n**(-1) if inverse transform
c
8887   do 8888 j=1,n2,2
       a(j)=a(j)*xp
8888   a(j+1)=a(j+1)*xp
8889   i=3
       it=ix(i-1)
       go to (1,2,3,4,5,6,7,8),it
c***********************************************************************
c
c                                    8 multiply butterfly
c
1      kk=ix(i)
c
11     k1=kk+kspan
       k2=k1+kspan
       k3=k2+kspan
c
       akp=a(kk)+a(k2)
       akm=a(kk)-a(k2)
       ajp=a(k1)+a(k3)
       ajm=a(k1)-a(k3)
       a(kk)=akp+ajp
c
       bkp=a(kk+1)+a(k2+1)
       bkm=a(kk+1)-a(k2+1)
       bjp=a(k1+1)+a(k3+1)
       bjm=a(k1+1)-a(k3+1)
       a(kk+1)=bkp+bjp
c
       bjp=bkp-bjp
c
       a(k2+1)=(akp+bjp-ajp)*c707
       a(k2)=a(k2+1)+bjp*cm141
c
       bkp=bkm+ajm
       akp=akm-bjm
c
       ac0=(akp+bkp)*c924
       a(k1+1)=ac0+akp*cm541
       a(k1)  =ac0+bkp*cm131
c
       bkm=bkm-ajm
       akm=akm+bjm
c
       ac0=(akm+bkm)*c383
       a(k3+1)=ac0+akm*c541
       a(k3)  =ac0+bkm*cm131
c
       i=i+1
       kk=ix(i)
       if (kk) 111,111,11
111    i=i+2
       it=ix(i-1)
       go to (1,2,3,4,5,6,7,8), it
c***********************************************************************
c
c                                    4 multiply butterfly
c
2      kk=ix(i)
c
22     k1=kk+kspan
       k2=k1+kspan
       k3=k2+kspan
c
       akp=a(kk)+a(k2)
       akm=a(kk)-a(k2)
       ajp=a(k1)+a(k3)
       ajm=a(k1)-a(k3)
       a(kk)=akp+ajp
c
       bkp=a(kk+1)+a(k2+1)
       bkm=a(kk+1)-a(k2+1)
       bjp=a(k1+1)+a(k3+1)
       bjm=a(k1+1)-a(k3+1)
       a(kk+1)=bkp+bjp
       a(k2)=-bkp+bjp
       a(k2+1)=akp-ajp
c
       bkp=bkm+ajm
c
       a(k1+1)=(bkp+akm-bjm)*c707
       a(k1)=a(k1+1)+bkp*cm141
c
       akm=akm+bjm
c
       a(k3+1)=(akm+ajm-bkm)*c707
       a(k3)=a(k3+1)+akm*cm141
c
       i=i+1
       kk=ix(i)
       if (kk) 222,222,22
222    i=i+2
       it=ix(i-1)
       go to (1,2,3,4,5,6,7,8), it
c***********************************************************************
c
c                                    8 multiply butterfly
c
3      kk=ix(i)
c
33     k1=kk+kspan
       k2=k1+kspan
       k3=k2+kspan
c
       akp=a(kk)+a(k2)
       akm=a(kk)-a(k2)
       ajp=a(k1)+a(k3)
       ajm=a(k1)-a(k3)
       a(kk)=akp+ajp
c
       bkp=a(kk+1)+a(k2+1)
       bkm=a(kk+1)-a(k2+1)
       bjp=a(k1+1)+a(k3+1)
       bjm=a(k1+1)-a(k3+1)
       a(kk+1)=bkp+bjp
c
       ajp=akp-ajp
c
       a(k2+1)=(ajp+bjp-bkp)*c707
       a(k2)=a(k2+1)+ajp*cm141
c
       bkp=bkm+ajm
       akp=akm-bjm
c
       ac0=(akp+bkp)*c383
       a(k1+1)=ac0+akp*c541
       a(k1)  =ac0+bkp*cm131
c
       bkm=bkm-ajm
       akm=akm+bjm
c
       ac0=(akm+bkm)*cm924
       a(k3+1)=ac0+akm*c541
       a(k3)  =ac0+bkm*c131
c
       i=i+1
       kk=ix(i)
       if (kk) 333,333,33
333    i=i+2
       it=ix(i-1)
       go to (1,2,3,4,5,6,7,8), it
c***********************************************************************
c
c                                    general 9 multiply butterfly
c
4      kk=ix(i)
c
44     k1=kk+kspan
       k2=k1+kspan
       k3=k2+kspan
c
       akp=a(kk)+a(k2)
       akm=a(kk)-a(k2)
       ajp=a(k1)+a(k3)
       ajm=a(k1)-a(k3)
       a(kk)=akp+ajp
c
       bkp=a(kk+1)+a(k2+1)
       bkm=a(kk+1)-a(k2+1)
       bjp=a(k1+1)+a(k3+1)
       bjm=a(k1+1)-a(k3+1)
       a(kk+1)=bkp+bjp
c
       ajp=akp-ajp
       bjp=bkp-bjp
c
       j=ix(i+1)
c
       ac0=(ajp+bjp)*t(j+8)
       a(k2+1)=ac0+ajp*t(j+6)
       a(k2)  =ac0+bjp*t(j+7)
c
       bkp=bkm+ajm
       akp=akm-bjm
c
       ac0=(akp+bkp)*t(j+5)
       a(k1+1)=ac0+akp*t(j+3)
       a(k1)  =ac0+bkp*t(j+4)
c
       bkm=bkm-ajm
       akm=akm+bjm
c
       ac0=(akm+bkm)*t(j+2)
       a(k3+1)=ac0+akm*t(j)
       a(k3)  =ac0+bkm*t(j+1)
c
       i=i+2
       kk=ix(i)
       if (kk) 444,444,44
444    i=i+2
       it=ix(i-1)
       go to (1,2,3,4,5,6,7,8), it
c***********************************************************************
c
c                                    0 multiply butterfly
c
5      kk=ix(i)
c
55     k1=kk+kspan
       k2=k1+kspan
       k3=k2+kspan
c
       akp=a(kk)+a(k2)
       akm=a(kk)-a(k2)
       ajp=a(k1)+a(k3)
       ajm=a(k1)-a(k3)
       a(kk)=akp+ajp
       a(k2)=akp-ajp
c
       bkp=a(kk+1)+a(k2+1)
       bkm=a(kk+1)-a(k2+1)
       bjp=a(k1+1)+a(k3+1)
       bjm=a(k1+1)-a(k3+1)
       a(kk+1)=bkp+bjp
       a(k2+1)=bkp-bjp
c
       a(k3+1)=bkm-ajm
       a(k1+1)=bkm+ajm
       a(k3)=akm+bjm
       a(k1)=akm-bjm
c
       i=i+1
       kk=ix(i)
       if (kk) 555,555,55
555    i=i+2
       it=ix(i-1)
       go to (1,2,3,4,5,6,7,8), it
c***********************************************************************
c
c                                       offset reduced
c
6      kspan=kspan/4
       i=i+2
       it=ix(i-1)
       go to (1,2,3,4,5,6,7,8), it
c***********************************************************************
c
c                                       bit reversal (shuffling)
c
7      ip1=ix(i)
77     ip2=ix(i+1)
       t1=a(ip2)
       a(ip2)=a(ip1)
       a(ip1)=t1
       t1=a(ip2+1)
       a(ip2+1)=a(ip1+1)
       a(ip1+1)=t1
       i=i+2
       ip1=ix(i)
       if (ip1) 777,777,77
777    i=i+2
       it=ix(i-1)
       go to (1,2,3,4,5,6,7,8), it
c***********************************************************************
8      if(jflag.eq.1) go to 888
c
c      conjugate output if forward transform
c
       do 88 j=2,n2,2
88     a(j)=-a(j)
888    return
c
c      fast fourier transform ends #####################################
c
c      initialization phase starts. done only once
c
9999   ixc=1
       n=4**mm
       xp=n
       xp=1./xp
       ntot=n
       n2=n*2
       nspan=n
       n1test=n/16
       n2test=n/8
       n3test=(3*n)/16
       nspan4=nspan/4
       ibase=0
       isn=1
       inc=isn
       rad=8.0*atan(1.0)
       pi=4.*atan(1.0)
       c707=sin(pi/4.)
       cm141=-2.*c707
       c383=sin(pi/8.)
       c924=cos(pi/8.)
       cm924=-c924
       c541=c924-c383
       cm541=-c541
       c131=c924+c383
       cm131=-c131
10     nt=inc*ntot
       ks=inc*nspan
       kspan=ks
       jc=ks/n
       radf=rad*float(jc)*.5
       i=0
c
c      determine the factors of n
c      all factors must be 4 for this version
c
       m=0
       k=n
15     m=m+1
       nfac(m)=4
       k=k/4
20     if(k-(k/4)*4.eq.0) go to 15
       kt=1
       if(n.ge.256) kt=2
       kspan0=kspan
       ntypl=0
c
100    ndelta=kspan0/kspan
       index=0
       sd=radf/float(kspan)
       cd=2.0*sin(sd)**2
       sd=sin(sd+sd)
       kk=1
       i=i+1
c
c      transform for a factor of 4
c
       kspan=kspan/4
       ix(ixc)=0
       ix(ixc+1)=6
       ixc=ixc+2
c
410    c1=1.0
       s1=0.0
420    k1=kk+kspan
       k2=k1+kspan
       k3=k2+kspan
       if(s1.eq.0.0) go to 460
430    if(kspan.ne.nspan4) go to 431
       t(ibase+5)=-(s1+c1)
       t(ibase+6)=c1
       t(ibase+4)=s1-c1
       t(ibase+8)=-(s2+c2)
       t(ibase+9)=c2
       t(ibase+7)=s2-c2
       t(ibase+2)=-(s3+c3)
       t(ibase+3)=c3
       t(ibase+1)=s3-c3
       ibase=ibase+9
c
431    kkp=(kk-1)*2
       if(index.ne.n1test) go to 150
       call rad4sb(1)
       go to 5035
150    if(index.ne.n2test) go to 160
       call rad4sb(2)
       go to 5035
160    if(index.ne.n3test) go to 170
       call rad4sb(3)
       go to 5035
170    call rad4sb(4)
5035   kk=k3+kspan
       if(kk.le.nt) go to 420
440    index=index+ndelta
       c2=c1-(cd*c1+sd*s1)
       s1=(sd*c1-cd*s1)+s1
       c1=c2
       c2=c1*c1-s1*s1
       s2=c1*s1+c1*s1
       c3=c2*c1-s2*s1
       s3=c2*s1+s2*c1
       kk=kk-nt+jc
       if(kk.le.kspan) go to 420
       kk=kk-kspan+inc
       if(kk.le.jc) go to 410
       if(kspan.eq.jc) go to 800
       go to 100
460    kkp=(kk-1)*2
       call rad4sb(5)
5050   kk=k3+kspan
       if(kk.le.nt) go to 420
       go to 440
c
800    ix(ixc)=0
       ix(ixc+1)=7
       ixc=ixc+2
c
c      compute parameters to permute the results to normal order
c      done in two steps
c      permutation for square factors of n
c
       np(1)=ks
       k=kt+kt+1
       if(m.lt.k) k=k-1
       j=1
       np(k+1)=jc
810    np(j+1)=np(j)/nfac(j)
       np(k)=np(k+1)*nfac(j)
       j=j+1
       k=k-1
       if(j.lt.k) go to 810
       k3=np(k+1)
       kspan=np(2)
       kk=jc+1
       k2=kspan+1
       j=1
c
c      permutation for single variate transform
c
820    kkp=(kk-1)*2
       k2p=(k2-1)*2
       ix(ixc)=kkp+1
       ix(ixc+1)=k2p+1
       ixc=ixc+2
       kk=kk+inc
       k2=kspan+k2
       if(k2.lt.ks) go to 820
830    k2=k2-np(j)
       j=j+1
       k2=np(j+1)+k2
       if(k2.gt.np(j)) go to 830
       j=1
840    if(kk.lt.k2) go to 820
       kk=kk+inc
       k2=kspan+k2
       if(k2.lt.ks) go to 840
       if(kk.lt.ks) go to 830
       jc=k3
       ix(ixc)=0
       ix(ixc+1)=8
       go to 8885
       end
