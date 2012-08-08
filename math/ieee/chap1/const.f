c
c-----------------------------------------------------------------------
c subroutine: const
c   computes the multipliers for the various modules
c-----------------------------------------------------------------------
c
      subroutine const(co3,co8,co16,co9,cdc,cdd)
      double precision dtheta,dtwopi,dsq32,dsq2
      double precision dcos1,dcos2,dcos3,dcos4
      double precision dsin1,dsin2,dsin3,dsin4
      dimension co3(3),co8(8),co16(18),co9(11),cdc(9),cdd(6)
      dtwopi=8.0d0*datan(1.0d0)
      dsq32=dsqrt(0.75d0)
      dsq2=dsqrt(0.5d0)
c
c   multipliers for the three point module
c
      co3(1)=1.0
      co3(2)=-1.5
      co3(3)=-dsq32
c
c   multipliers for the five point module
c
      dtheta=dtwopi/5.0d0
      dcos1=dcos(dtheta)
      dcos2=dcos(2.0d0*dtheta)
      dsin1=dsin(dtheta)
      dsin2=dsin(2.0d0*dtheta)
      cdd(1)=1.0
      cdd(2)=-1.25
      cdd(3)=-dsin1-dsin2
      cdd(4)=0.5*(dcos1-dcos2)
      cdd(5)=dsin1-dsin2
      cdd(6)=dsin2
c
c
c   multipliers for the seven point module
c
      dtheta=dtwopi/7.0d0
      dcos1=dcos(dtheta)
      dcos2=dcos(2.0d0*dtheta)
      dcos3=dcos(3.0d0*dtheta)
      dsin1=dsin(dtheta)
      dsin2=dsin(2.0d0*dtheta)
      dsin3=dsin(3.0d0*dtheta)
      cdc(1)=1.0
      cdc(2)=-7.0d0/6.0d0
      cdc(3)=-(dsin1+dsin2-dsin3)/3.0d0
      cdc(4)=(dcos1+dcos2-2.0d0*dcos3)/3.0d0
      cdc(5)=(2.0d0*dcos1-dcos2-dcos3)/3.0d0
      cdc(6)=-(2.0d0*dsin1-dsin2+dsin3)/3.0d0
      cdc(7)=-(dsin1+dsin2+2.0d0*dsin3)/3.0d0
      cdc(8)=(dcos1-2.0d0*dcos2+dcos3)/3.0d0
      cdc(9)=-(dsin1-2.0d0*dsin2-dsin3)/3.0d0
c
c   multipliers for the eight point module
c
      co8(1)=1.0
      co8(2)=1.0
      co8(3)=1.0
      co8(4)=-1.0
      co8(5)=1.0
      co8(6)=-dsq2
      co8(7)=-1.0
      co8(8)=dsq2
c
c   multipliers for the nine point module
c
      dtheta=dtwopi/9.0d0
      dcos1=dcos(dtheta)
      dcos2=dcos(2.0d0*dtheta)
      dcos4=dcos(4.0d0*dtheta)
      dsin1=dsin(dtheta)
      dsin2=dsin(2.0d0*dtheta)
      dsin4=dsin(4.0d0*dtheta)
      co9(1)=1.0
      co9(2)=-1.5
      co9(3)=-dsq32
      co9(4)=0.5
      co9(5)=(2.0d0*dcos1-dcos2-dcos4)/3.0d0
      co9(6)=(dcos1-2.0d0*dcos2+dcos4)/3.0d0
      co9(7)=(dcos1+dcos2-2.0d0*dcos4)/3.0d0
      co9(8)=-(2.0d0*dsin1+dsin2-dsin4)/3.0d0
      co9(9)=-(dsin1+2.0d0*dsin2+dsin4)/3.0d0
      co9(10)=-(dsin1-dsin2-2.0d0*dsin4)/3.0d0
      co9(11)=-dsq32
c
c   multipliers for the sixteen point module
c
      dtheta=dtwopi/16.0d0
      dcos1=dcos(dtheta)
      dcos3=dcos(3.0d0*dtheta)
      dsin1=dsin(dtheta)
      dsin3=dsin(3.0d0*dtheta)
      co16(1)=1.0
      co16(2)=1.0
      co16(3)=1.0
      co16(4)=-1.0
      co16(5)=1.0
      co16(6)=-dsq2
      co16(7)=-1.0
      co16(8)=dsq2
      co16(9)=1.0
      co16(10)=-(dsin1-dsin3)
      co16(11)=-dsq2
      co16(12)=-co16(10)
      co16(13)=-1.0
      co16(14)=-(dsin1+dsin3)
      co16(15)=dsq2
      co16(16)=-co16(14)
      co16(17)=-dsin3
      co16(18)=dcos3
      return
      end
