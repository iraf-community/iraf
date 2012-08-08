      subroutine titand ( tau, gtau, n )
c  from  * a practical guide to splines *  by c. de boor
c  these data represent a property of titanium as a function of
c  temperature. they have been used extensively as an example in spline
c  approximation with variable knots.
      integer n,   i
      real gtau(49),tau(49),gtitan(49)
      data gtitan /.644,.622,.638,.649,.652,.639,.646,.657,.652,.655,
     2		   .644,.663,.663,.668,.676,.676,.686,.679,.678,.683,
     3		   .694,.699,.710,.730,.763,.812,.907,1.044,1.336,1.881,
     4		   2.169,2.075,1.598,1.211,.916,.746,.672,.627,.615,.607
     5		  ,.606,.609,.603,.601,.603,.601,.611,.601,.608/
      n = 49
      do 10 i=1,n
	 tau(i) = 585. + 10.*float(i)
   10	 gtau(i) = gtitan(i)
					return
      end
