      integer function mktabl (nodsiz)
      integer nodsiz
      integer mem( 60000)
      common/cdsmem/mem
      integer st
      integer dsget
      integer i
      st = dsget (43 + 1)
      mem (st) = nodsiz
      mktabl = st
      do 23000 i = 1, 43
      st = st + 1
      mem (st) = 0
23000 continue
23001 continue
      return
      end
