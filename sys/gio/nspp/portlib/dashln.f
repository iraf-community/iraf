      subroutine dashln (ipat)
      jpat = ior(ishift(ipat,6),ishift(ipat,-4))
      call optn (4hdpat,jpat)
      return
      end
