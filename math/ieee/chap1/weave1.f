c
c-----------------------------------------------------------------------
c subroutine: weave1
c   this subroutine implements the different pre-weave
c   modules of the wfta.  the working arrays are sr and si.
c   the routine checks to see which factors are present
c   in the transform length n = na*nb*nc*nd and executes
c   the pre-weave code for these factors.
c
c-----------------------------------------------------------------------
c
      subroutine weave1(sr,si)
      common na,nb,nc,nd,nd1,nd2,nd3,nd4
      dimension q(8),t(16)
      dimension sr(1),si(1)
      if(na.eq.1) go to 300
      if(na.ne.2) go to 800
c
c **********************************************************************
c
c     the following code implements the 2 point pre-weave module
c
c **********************************************************************
c
      nlup2=2*(nd2-nb)
      nlup23=2*nd2*(nd3-nc)
      nbase=1
      do 240 n4=1,nd
      do 230 n3=1,nc
      do 220 n2=1,nb
      nr1=nbase+1
      t0=sr(nbase)+sr(nr1)
      sr(nr1)=sr(nbase)-sr(nr1)
      sr(nbase)=t0
      t0=si(nbase)+si(nr1)
      si(nr1)=si(nbase)-si(nr1)
      si(nbase)=t0
220   nbase=nbase+2
230   nbase=nbase+nlup2
240   nbase=nbase+nlup23
800   if(na.ne.8) go to 1600
c
c **********************************************************************
c
c     the following code implements the 8 point pre-weave module
c
c **********************************************************************
c
      nlup2=8*(nd2-nb)
      nlup23=8*nd2*(nd3-nc)
      nbase=1
      do 840 n4=1,nd
      do 830 n3=1,nc
      do 820 n2=1,nb
      nr1=nbase+1
      nr2=nr1+1
      nr3=nr2+1
      nr4=nr3+1
      nr5=nr4+1
      nr6=nr5+1
      nr7=nr6+1
      t3=sr(nr3)+sr(nr7)
      t7=sr(nr3)-sr(nr7)
      t0=sr(nbase)+sr(nr4)
      sr(nr4)=sr(nbase)-sr(nr4)
      t1=sr(nr1)+sr(nr5)
      t5=sr(nr1)-sr(nr5)
      t2=sr(nr2)+sr(nr6)
      sr(nr6)=sr(nr2)-sr(nr6)
      sr(nbase)=t0+t2
      sr(nr2)=t0-t2
      sr(nr1)=t1+t3
      sr(nr3)=t1-t3
      sr(nr5)=t5+t7
      sr(nr7)=t5-t7
      t3=si(nr3)+si(nr7)
      t7=si(nr3)-si(nr7)
      t0=si(nbase)+si(nr4)
      si(nr4)=si(nbase)-si(nr4)
      t1=si(nr1)+si(nr5)
      t5=si(nr1)-si(nr5)
      t2=si(nr2)+si(nr6)
      si(nr6)=si(nr2)-si(nr6)
      si(nbase)=t0+t2
      si(nr2)=t0-t2
      si(nr1)=t1+t3
      si(nr3)=t1-t3
      si(nr5)=t5+t7
      si(nr7)=t5-t7
820   nbase=nbase+8
830   nbase=nbase+nlup2
840   nbase=nbase+nlup23
1600  if(na.ne.16) go to 300
c
c **********************************************************************
c
c     the following code implements the 16 point pre-weave module
c
c **********************************************************************
c
      nlup2=18*(nd2-nb)
      nlup23=18*nd2*(nd3-nc)
      nbase=1
      do 1640 n4=1,nd
      do 1630 n3=1,nc
      do 1620 n2=1,nb
      nr1=nbase+1
      nr2=nr1+1
      nr3=nr2+1
      nr4=nr3+1
      nr5=nr4+1
      nr6=nr5+1
      nr7=nr6+1
      nr8=nr7+1
      nr9=nr8+1
      nr10=nr9+1
      nr11=nr10+1
      nr12=nr11+1
      nr13=nr12+1
      nr14=nr13+1
      nr15=nr14+1
      nr16=nr15+1
      nr17=nr16+1
      jbase=nbase
      do 1645 j=1,8
      t(j)=sr(jbase)+sr(jbase+8)
      t(j+8)=sr(jbase)-sr(jbase+8)
      jbase=jbase+1
1645  continue
      do 1650 j=1,4
      q(j)=t(j)+t(j+4)
      q(j+4)=t(j)-t(j+4)
1650  continue
      sr(nbase)=q(1)+q(3)
      sr(nr2)=q(1)-q(3)
      sr(nr1)=q(2)+q(4)
      sr(nr3)=q(2)-q(4)
      sr(nr5)=q(6)+q(8)
      sr(nr7)=q(6)-q(8)
      sr(nr4)=q(5)
      sr(nr6)=q(7)
      sr(nr8)=t(9)
      sr(nr9)=t(10)+t(16)
      sr(nr15)=t(10)-t(16)
      sr(nr13)=t(14)+t(12)
      sr(nr11)=t(14)-t(12)
      sr(nr17)=sr(nr11)+sr(nr15)
      sr(nr16)=sr(nr9)+sr(nr13)
      sr(nr10)=t(11)+t(15)
      sr(nr14)=t(11)-t(15)
      sr(nr12)=t(13)
      jbase=nbase
      do 1745 j=1,8
      t(j)=si(jbase)+si(jbase+8)
      t(j+8)=si(jbase)-si(jbase+8)
      jbase=jbase+1
1745  continue
      do 1750 j=1,4
      q(j)=t(j)+t(j+4)
      q(j+4)=t(j)-t(j+4)
1750  continue
      si(nbase)=q(1)+q(3)
      si(nr2)=q(1)-q(3)
      si(nr1)=q(2)+q(4)
      si(nr3)=q(2)-q(4)
      si(nr5)=q(6)+q(8)
      si(nr7)=q(6)-q(8)
      si(nr4)=q(5)
      si(nr6)=q(7)
      si(nr8)=t(9)
      si(nr9)=t(10)+t(16)
      si(nr15)=t(10)-t(16)
      si(nr13)=t(14)+t(12)
      si(nr11)=t(14)-t(12)
      si(nr17)=si(nr11)+si(nr15)
      si(nr16)=si(nr9)+si(nr13)
      si(nr10)=t(11)+t(15)
      si(nr14)=t(11)-t(15)
      si(nr12)=t(13)
1620  nbase=nbase+18
1630  nbase=nbase+nlup2
1640  nbase=nbase+nlup23
300   if(nb.eq.1) go to 700
      if(nb.ne.3) go to 900
c
c **********************************************************************
c
c     the following code implements the 3 point pre-weave module
c
c **********************************************************************
c
      nlup2=2*nd1
      nlup23=3*nd1*(nd3-nc)
      nbase=1
      noff=nd1
      do 340 n4=1,nd
      do 330 n3=1,nc
      do 310 n2=1,nd1
      nr1=nbase+noff
      nr2=nr1+noff
      t1=sr(nr1)+sr(nr2)
      sr(nbase)=sr(nbase)+t1
      sr(nr2)=sr(nr1)-sr(nr2)
      sr(nr1)=t1
      t1=si(nr1)+si(nr2)
      si(nbase)=si(nbase)+t1
      si(nr2)=si(nr1)-si(nr2)
      si(nr1)=t1
310   nbase=nbase+1
330   nbase=nbase+nlup2
340   nbase=nbase+nlup23
900   if(nb.ne.9) go to 700
c
c **********************************************************************
c
c     the following code implements the 9 point pre-weave module
c
c **********************************************************************
c
      nlup2=10*nd1
      nlup23=11*nd1*(nd3-nc)
      nbase=1
      noff=nd1
      do 940 n4=1,nd
      do 930 n3=1,nc
      do 910 n2=1,nd1
      nr1=nbase+noff
      nr2=nr1+noff
      nr3=nr2+noff
      nr4=nr3+noff
      nr5=nr4+noff
      nr6=nr5+noff
      nr7=nr6+noff
      nr8=nr7+noff
      nr9=nr8+noff
      nr10=nr9+noff
      t3=sr(nr3)+sr(nr6)
      t6=sr(nr3)-sr(nr6)
      sr(nbase)=sr(nbase)+t3
      t7=sr(nr7)+sr(nr2)
      t2=sr(nr7)-sr(nr2)
      sr(nr2)=t6
      t1=sr(nr1)+sr(nr8)
      t8=sr(nr1)-sr(nr8)
      sr(nr1)=t3
      t4=sr(nr4)+sr(nr5)
      t5=sr(nr4)-sr(nr5)
      sr(nr3)=t1+t4+t7
      sr(nr4)=t1-t7
      sr(nr5)=t4-t1
      sr(nr6)=t7-t4
      sr(nr10)=t2+t5+t8
      sr(nr7)=t8-t2
      sr(nr8)=t5-t8
      sr(nr9)=t2-t5
      t3=si(nr3)+si(nr6)
      t6=si(nr3)-si(nr6)
      si(nbase)=si(nbase)+t3
      t7=si(nr7)+si(nr2)
      t2=si(nr7)-si(nr2)
      si(nr2)=t6
      t1=si(nr1)+si(nr8)
      t8=si(nr1)-si(nr8)
      si(nr1)=t3
      t4=si(nr4)+si(nr5)
      t5=si(nr4)-si(nr5)
      si(nr3)=t1+t4+t7
      si(nr4)=t1-t7
      si(nr5)=t4-t1
      si(nr6)=t7-t4
      si(nr10)=t2+t5+t8
      si(nr7)=t8-t2
      si(nr8)=t5-t8
      si(nr9)=t2-t5
910   nbase=nbase+1
930   nbase=nbase+nlup2
940   nbase=nbase+nlup23
700   if(nc.ne.7) go to 500
c
c **********************************************************************
c
c     the following code implements the 7 point pre-weave module
c
c **********************************************************************
c
      noff=nd1*nd2
      nbase=1
      nlup2=8*noff
      do 740 n4=1,nd
      do 710 n1=1,noff
      nr1=nbase+noff
      nr2=nr1+noff
      nr3=nr2+noff
      nr4=nr3+noff
      nr5=nr4+noff
      nr6=nr5+noff
      nr7=nr6+noff
      nr8=nr7+noff
      t1=sr(nr1)+sr(nr6)
      t6=sr(nr1)-sr(nr6)
      t4=sr(nr4)+sr(nr3)
      t3=sr(nr4)-sr(nr3)
      t2=sr(nr2)+sr(nr5)
      t5=sr(nr2)-sr(nr5)
      sr(nr5)=t6-t3
      sr(nr2)=t5+t3+t6
      sr(nr6)=t5-t6
      sr(nr8)=t3-t5
      sr(nr3)=t2-t1
      sr(nr4)=t1-t4
      sr(nr7)=t4-t2
      t1=t1+t4+t2
      sr(nbase)=sr(nbase)+t1
      sr(nr1)=t1
      t1=si(nr1)+si(nr6)
      t6=si(nr1)-si(nr6)
      t4=si(nr4)+si(nr3)
      t3=si(nr4)-si(nr3)
      t2=si(nr2)+si(nr5)
      t5=si(nr2)-si(nr5)
      si(nr5)=t6-t3
      si(nr2)=t5+t3+t6
      si(nr6)=t5-t6
      si(nr8)=t3-t5
      si(nr3)=t2-t1
      si(nr4)=t1-t4
      si(nr7)=t4-t2
      t1=t1+t4+t2
      si(nbase)=si(nbase)+t1
      si(nr1)=t1
710   nbase=nbase+1
740   nbase=nbase+nlup2
500   if(nd.ne.5) return
c
c **********************************************************************
c
c     the following code implements the 5 point pre-weave module
c
c **********************************************************************
c
      noff=nd1*nd2*nd3
      nbase=1
      do 510 n1=1,noff
      nr1=nbase+noff
      nr2=nr1+noff
      nr3=nr2+noff
      nr4=nr3+noff
      nr5=nr4+noff
      t4=sr(nr1)-sr(nr4)
      t1=sr(nr1)+sr(nr4)
      t3=sr(nr3)+sr(nr2)
      t2=sr(nr3)-sr(nr2)
      sr(nr3)=t1-t3
      sr(nr1)=t1+t3
      sr(nbase)=sr(nbase)+sr(nr1)
      sr(nr5)=t2+t4
      sr(nr2)=t4
      sr(nr4)=t2
      t4=si(nr1)-si(nr4)
      t1=si(nr1)+si(nr4)
      t3=si(nr3)+si(nr2)
      t2=si(nr3)-si(nr2)
      si(nr3)=t1-t3
      si(nr1)=t1+t3
      si(nbase)=si(nbase)+si(nr1)
      si(nr5)=t2+t4
      si(nr2)=t4
      si(nr4)=t2
510   nbase=nbase+1
      return
      end
