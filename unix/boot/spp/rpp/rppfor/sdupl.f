      integer function sdupl (str)
      integer str (100)
      integer mem( 60000)
      common/cdsmem/mem
      integer i
      integer length
      integer j
      integer dsget
      j = dsget (length (str) + 1)
      sdupl = j
      i = 1
23000 if (.not.(str (i) .ne. -2))goto 23002
      mem (j) = str (i)
      j = j + 1
23001 i = i + 1
      goto 23000
23002 continue
      mem (j) = -2
      return
      end
