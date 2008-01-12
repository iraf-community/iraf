c subroutine linfit.f
c
c source
c   Bevington, pages 104-105.
c
c purpose
c   make a least-squares fit to a data set with a straight line
c
c usage
c   call linfit (x, y, sigmay, npts, mode, a, sigmaa, b, sigmab, r)
c
c description of parameters
c   x      - array of data points for independent variable
c   y      - array of data points for dependent variable
c   sigmay - array of standard deviations for y data points
c   npts   - number of pairs of data points
c   mode   - determines method of weighting least-squares fit
c            +1 (instrumental) weight(i) = 1./sigmay(i)**2
c             0 (no weighting) weight(i) = 1.
c            -1 (statistical)  weight(i) = 1./y(i)
c   a      - y intercept of fitted straight line
c   sigmaa - standard deviation of a
c   b      - slope of fitted straight line
c   sigmab - standard deviation of b
c   r      - linear correlation coefficient
c
c subroutines and function subprograms required
c   none
c
      subroutine linfit (x,y,sigmay,npts,mode,a,sigmaa,b,sigmab,r)
      double precision sum,sumx,sumy,sumx2,sumxy,sumy2
      double precision xi,yi,weight,delta,varnce
      dimension x(1),y(1),sigmay(1)
c
c accumulate weighted sums
c
11    sum=0.
      sumx=0.
      sumy=0.
      sumx2=0.
      sumxy=0.
      sumy2=0.
21    do 50 i=1,npts
      xi=x(i)
      yi=y(i)
      if (mode) 31,36,38
31    if (yi) 34,36,32
32    weight=1./yi
      goto 41
34    weight=1./(-yi)
      goto 41
36    weight=1.
      goto 41
38    weight=1./sigmay(i)**2
41    sum=sum+weight
      sumx=sumx+weight*xi
      sumy=sumy+weight*yi
      sumx2=sumx2+weight*xi*xi
      sumxy=sumxy+weight*xi*yi
      sumy2=sumy2+weight*yi*yi
50    continue
c
c calculate coefficients and standard deviations
c
51    delta=sum*sumx2-sumx*sumx
      a=(sumx2*sumy-sumx*sumxy)/delta
53    b=(sumxy*sum-sumx*sumy)/delta
61    if (mode) 62,64,62
62    varnce=1.
      goto 67
64    c=npts-2
      varnce=(sumy2+a*a*sum+b*b*sumx2
     *-2.*(a*sumy+b*sumxy-a*b*sumx))/c
67    sigmaa=dsqrt(varnce*sumx2/delta)
68    sigmab=dsqrt(varnce*sum/delta)
71    r=(sum*sumxy-sumx*sumy)/
     *dsqrt(delta*(sum*sumy2-sumy*sumy))
      return
      end
