      subroutine thello ()
      integer*2 st0001(15)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) / 72,101,108,108,111, 44, 32, 87/
      data (st0001(iyy),iyy= 9,15) /111,114,108,100, 33, 10, 0/
         call xprinf(st0001)
100      call zzepro
         return
      end
c     thello  t_hello
