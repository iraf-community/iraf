      real function round ( x )
c  from  * a practical guide to splines *  by c. de boor
called in example 1  of chapter xiii
      real x,	flip,size
      common /rount/ size
      data flip /-1./
      flip = -flip
      round = x + flip*size
					return
      end
