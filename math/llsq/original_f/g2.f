      subroutine g2    (cos,sin,x,y)
c     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1972 dec 15
c     to appear in 'solving least squares problems', prentice-hall, 1974
c	   apply the rotation computed by g1 to (x,y).
      xr=cos*x+sin*y
      y=-sin*x+cos*y
      x=xr
      return
      end
