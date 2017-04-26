c
c-----------------------------------------------------------------------
c
c subroutine: weave2
c   this subroutine implements the post-weave modules
c   of the wfta.  the working arrays are sr and si.
c   the routine checks to see which factors are present
c   in the transform length n = na*nb*nc*nd and executes
c   the post-weave code for these factors.
c
c-----------------------------------------------------------------------
c
      subroutine weave2(sr,si)
      common na,nb,nc,nd,nd1,nd2,nd3,nd4
      dimension sr(1),si(1)
      dimension q(8),t(16)
      if(nd.ne.5) go to 700
c
c **********************************************************************
c
c     the following code implements the 5 point post-weave module
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
      t1=sr(nbase)+sr(nr1)
      t3=t1-sr(nr3)
      t1=t1+sr(nr3)
      t4=si(nr2)+si(nr5)
      t2=si(nr4)+si(nr5)
      sr(nr1)=t1-t4
      sr4=t1+t4
      sr2=t3+t2
      sr(nr3)=t3-t2
      t1=si(nbase)+si(nr1)
      t3=t1-si(nr3)
      t1=t1+si(nr3)
      t4=sr(nr2)+sr(nr5)
      t2=sr(nr4)+sr(nr5)
      si(nr1)=t1+t4
      si(nr4)=t1-t4
      si(nr2)=t3-t2
      si(nr3)=t3+t2
      sr(nr2)=sr2
      sr(nr4)=sr4
510   nbase=nbase+1
700   if(nc.ne.7) go to 300
c
c **********************************************************************
c
c     the following code implements the 7 point post-weave module
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
      t1=sr(nr1)+sr(nbase)
      t2=t1-sr(nr3)-sr(nr4)
      t4=t1+sr(nr3)-sr(nr7)
      t1=t1+sr(nr4)+sr(nr7)
      t6=si(nr2)+si(nr5)+si(nr8)
      t5=si(nr2)-si(nr5)-si(nr6)
      t3=si(nr2)+si(nr6)-si(nr8)
      sr(nr1)=t1-t6
      sr6=t1+t6
      sr2=t2-t5
      sr5=t2+t5
      sr(nr4)=t4-t3
      sr(nr3)=t4+t3
      t1=si(nr1)+si(nbase)
      t2=t1-si(nr3)-si(nr4)
      t4=t1+si(nr3)-si(nr7)
      t1=t1+si(nr4)+si(nr7)
      t6=sr(nr2)+sr(nr5)+sr(nr8)
      t5=sr(nr2)-sr(nr5)-sr(nr6)
      t3=sr(nr2)+sr(nr6)-sr(nr8)
      si(nr1)=t1+t6
      si(nr6)=t1-t6
      si(nr2)=t2+t5
      si(nr5)=t2-t5
      si(nr4)=t4+t3
      si(nr3)=t4-t3
      sr(nr2)=sr2
      sr(nr5)=sr5
      sr(nr6)=sr6
710   nbase=nbase+1
740   nbase=nbase+nlup2
300   if(nb.eq.1) go to 400
      if(nb.ne.3) go to 900
c
c **********************************************************************
c
c     the following code implements the 3 point post-weave module
c
c **********************************************************************
c
      nlup2=2*nd1
      nlup23=3*nd1*(nd3-nc)
      nbase=1
      noff=nd1
      do 340 n5=1,nd
      do 330 n4=1,nc
      do 310 n2=1,nd1
      nr1=nbase+noff
      nr2=nr1+noff
      t1=sr(nbase)+sr(nr1)
      sr(nr1)=t1-si(nr2)
      sr2=t1+si(nr2)
      t1=si(nbase)+si(nr1)
      si(nr1)=t1+sr(nr2)
      si(nr2)=t1-sr(nr2)
      sr(nr2)=sr2
310   nbase=nbase+1
330   nbase=nbase+nlup2
340   nbase=nbase+nlup23
900   if(nb.ne.9) go to 400
c
c **********************************************************************
c
c     the following code implements the 9 point post-weave module
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
      t3=sr(nbase)-sr(nr3)
      t7=sr(nbase)+sr(nr1)
      sr(nbase)=sr(nbase)+sr(nr3)+sr(nr3)
      t6=t3+si(nr10)
      sr(nr3)=t3-si(nr10)
      t4=t7+sr(nr5)-sr(nr6)
      t1=t7-sr(nr4)-sr(nr5)
      t7=t7+sr(nr4)+sr(nr6)
      sr(nr6)=t6
      t8=si(nr2)-si(nr7)-si(nr8)
      t5=si(nr2)+si(nr8)-si(nr9)
      t2=si(nr2)+si(nr7)+si(nr9)
      sr(nr1)=t7-t2
      sr8=t7+t2
      sr(nr4)=t1-t8
      sr(nr5)=t1+t8
      sr7=t4-t5
      sr2=t4+t5
      t3=si(nbase)-si(nr3)
      t7=si(nbase)+si(nr1)
      si(nbase)=si(nbase)+si(nr3)+si(nr3)
      t6=t3-sr(nr10)
      si(nr3)=t3+sr(nr10)
      t4=t7+si(nr5)-si(nr6)
      t1=t7-si(nr4)-si(nr5)
      t7=t7+si(nr4)+si(nr6)
      si(nr6)=t6
      t8=sr(nr2)-sr(nr7)-sr(nr8)
      t5=sr(nr2)+sr(nr8)-sr(nr9)
      t2=sr(nr2)+sr(nr7)+sr(nr9)
      si(nr1)=t7+t2
      si(nr8)=t7-t2
      si(nr4)=t1+t8
      si(nr5)=t1-t8
      si(nr7)=t4+t5
      si(nr2)=t4-t5
      sr(nr2)=sr2
      sr(nr7)=sr7
      sr(nr8)=sr8
910   nbase=nbase+1
930   nbase=nbase+nlup2
940   nbase=nbase+nlup23
400   if(na.eq.1) return
      if(na.ne.4) go to 800
c
c **********************************************************************
c
c     the following code implements the 4 point post-weave module
c
c **********************************************************************
c
      nlup2=4*(nd2-nb)
      nlup23=4*nd2*(nd3-nc)
      nbase=1
      do 440 n4=1,nd
      do 430 n3=1,nc
      do 420 n2=1,nb
      nr1=nbase+1
      nr2=nr1+1
      nr3=nr2+1
      tr0=sr(nbase)+sr(nr2)
      tr2=sr(nbase)-sr(nr2)
      tr1=sr(nr1)+sr(nr3)
      tr3=sr(nr1)-sr(nr3)
      ti1=si(nr1)+si(nr3)
      ti3=si(nr1)-si(nr3)
      sr(nbase)=tr0+tr1
      sr(nr2)=tr0-tr1
      sr(nr1)=tr2+ti3
      sr(nr3)=tr2-ti3
      ti0=si(nbase)+si(nr2)
      ti2=si(nbase)-si(nr2)
      si(nbase)=ti0+ti1
      si(nr2)=ti0-ti1
      si(nr1)=ti2-tr3
      si(nr3)=ti2+tr3
420   nbase=nbase+4
430   nbase=nbase+nlup2
440   nbase=nbase+nlup23
800   if(na.ne.8) go to 1600
c
c **********************************************************************
c
c     the following code implements the 8 point post-weave module
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
      t1=sr(nbase)-sr(nr1)
      sr(nbase)=sr(nbase)+sr(nr1)
      sr6=sr(nr2)+si(nr3)
      sr(nr2)=sr(nr2)-si(nr3)
      t4=sr(nr4)-si(nr5)
      t5=sr(nr4)+si(nr5)
      t6=sr(nr7)-si(nr6)
      t7=sr(nr7)+si(nr6)
      sr(nr4)=t1
      sr(nr1)=t4+t6
      sr3=t4-t6
      sr5=t5-t7
      sr(nr7)=t5+t7
      t1=si(nbase)-si(nr1)
      si(nbase)=si(nbase)+si(nr1)
      t3=si(nr2)-sr(nr3)
      si(nr2)=si(nr2)+sr(nr3)
      t4=si(nr4)+sr(nr5)
      t5=si(nr4)-sr(nr5)
      si(nr6)=t3
      t6=sr(nr6)+si(nr7)
      t7=sr(nr6)-si(nr7)
      si(nr4)=t1
      si(nr1)=t4+t6
      si(nr3)=t4-t6
      si(nr5)=t5+t7
      si(nr7)=t5-t7
      sr(nr3)=sr3
      sr(nr5)=sr5
      sr(nr6)=sr6
820   nbase=nbase+8
830   nbase=nbase+nlup2
840   nbase=nbase+nlup23
1600  if(na.ne.16) return
c
c **********************************************************************
c
c     the following code implements the 16 point post-weave module
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
      t(2)=sr(nbase)-sr(nr1)
      sr(nbase)=sr(nr1)+sr(nbase)
      t(4)=sr(nr2)+si(nr3)
      t(3)=sr(nr2)-si(nr3)
      t(6)=sr(nr4)+si(nr5)
      t(5)=sr(nr4)-si(nr5)
      t(8)=-si(nr6)-sr(nr7)
      t(7)=-si(nr6)+sr(nr7)
      t(9)=sr(nr8)+sr(nr14)
      t(15)=sr(nr8)-sr(nr14)
      t(13)=-si(nr10)-si(nr12)
      t(11)=si(nr10)-si(nr12)
      t(16)=sr(nr15)-sr(nr17)
      t(12)=sr(nr11)-sr(nr17)
      t(10)=-si(nr9)-si(nr16)
      t(14)=-si(nr16)+si(nr13)
      sr(nr2)=t(5)+t(7)
      sr6=t(5)-t(7)
      sr10=t(6)+t(8)
      sr(nr14)=t(6)-t(8)
      q(7)=t(9)+t(10)
      q(8)=t(9)-t(10)
      q(1)=t(11)+t(12)
      q(2)=t(11)-t(12)
      q(4)=t(14)+t(15)
      q(5)=t(15)-t(14)
      q(3)=t(13)+t(16)
      q(6)=t(13)-t(16)
      sr(nr1)=q(3)+q(7)
      sr(nr7)=q(7)-q(3)
      sr9=q(8)+q(6)
      sr(nr15)=q(8)-q(6)
      sr5=q(1)+q(4)
      sr3=q(4)-q(1)
      sr13=q(2)+q(5)
      sr11=q(5)-q(2)
      sr(nr8)=t(2)
      sr(nr4)=t(3)
      sr12=t(4)
      t(2)=si(nbase)-si(nr1)
      si(nbase)=si(nr1)+si(nbase)
      t(4)=si(nr2)-sr(nr3)
      t(3)=si(nr2)+sr(nr3)
      t(6)=si(nr4)-sr(nr5)
      t(5)=si(nr4)+sr(nr5)
      t(8)=sr(nr6)-si(nr7)
      t(7)=sr(nr6)+si(nr7)
      t(9)=si(nr8)+si(nr14)
      t(15)=si(nr8)-si(nr14)
      t(13)=sr(nr10)+sr(nr12)
      t(11)=sr(nr12)-sr(nr10)
      t(16)=si(nr15)-si(nr17)
      t(12)=si(nr11)-si(nr17)
      t(10)=sr(nr9)+sr(nr16)
      si(nr2)=t(5)+t(7)
      si(nr6)=t(5)-t(7)
      si(nr10)=t(6)+t(8)
      si(nr14)=t(6)-t(8)
      q(7)=t(9)+t(10)
      q(8)=t(9)-t(10)
      q(1)=t(11)+t(12)
      q(2)=t(11)-t(12)
      q(4)=t(14)+t(15)
      q(5)=t(15)-t(14)
      q(3)=t(13)+t(16)
      q(6)=t(13)-t(16)
      si(nr1)=q(3)+q(7)
      si(nr7)=q(7)-q(3)
      si(nr9)=q(8)+q(6)
      si(nr15)=q(8)-q(6)
      si(nr5)=q(1)+q(4)
      si(nr3)=q(4)-q(1)
      si(nr13)=q(2)+q(5)
      si(nr11)=q(5)-q(2)
      si(nr8)=t(2)
      si(nr4)=t(3)
      si(nr12)=t(4)
      sr(nr3)=sr3
      sr(nr5)=sr5
      sr(nr6)=sr6
      sr(nr9)=sr9
      sr(nr10)=sr10
      sr(nr11)=sr11
      sr(nr12)=sr12
      sr(nr13)=sr13
1620  nbase=nbase+18
1630  nbase=nbase+nlup2
1640  nbase=nbase+nlup23
      return
      end
