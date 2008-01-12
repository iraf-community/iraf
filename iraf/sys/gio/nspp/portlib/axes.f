      subroutine axes (x,y)
      call getset (idummy,idummy,idummy,idummy,xc,xd,yc,yd,idummy)
      call line (x,yc,x,yd)
      call line (xc,y,xd,y)
      return
      end
