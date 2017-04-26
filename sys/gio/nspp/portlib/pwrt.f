      subroutine pwrt (x,y,chars,nchar,jsiz,jor)
      dimension       chars(1)
      dimension       jfix(4)
      data jfix(1),jfix(2),jfix(3),jfix(4)/128,192,256,384/
      isiz = max0(0,min0(3,jsiz))
      call fl2int (x,y,nx,ny)
      call getsi (ixsave,iysave)
      nx = max0(0,ishift(nx-(1-jor)*jfix(isiz+1),ixsave-15))
      ny = max0(0,ishift(ny-jor*jfix(isiz+1),iysave-15))
      call pwrit (nx,ny,chars,nchar,isiz,jor*90,-1)
      return
      end
