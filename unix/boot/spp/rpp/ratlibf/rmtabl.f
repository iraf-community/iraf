      subroutine rmtabl (st)
      integer st
      integer mem( 60000)
      common/cdsmem/mem
      integer i
      integer walker, bucket, node
      bucket = st
      do 23000 i = 1, 43
      bucket = bucket + 1
      walker = mem (bucket)
23002 if (.not.(walker .ne. 0))goto 23003
      node = walker
      walker = mem (node + 0)
      call dsfree (node)
      goto 23002
23003 continue
23000 continue
23001 continue
      call dsfree (st)
      return
      end
