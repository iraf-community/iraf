      integer function esc (array, i)
      integer array (100)
      integer i
      if (.not.(array (i) .ne. 64))goto 23000
      esc = array (i)
      goto 23001
23000 continue
      if (.not.(array (i+1) .eq. -2))goto 23002
      esc = 64
      goto 23003
23002 continue
      i = i + 1
      if (.not.(array (i) .eq. 110 .or. array (i) .eq. 78))goto 23004
      esc = 10
      goto 23005
23004 continue
      if (.not.(array (i) .eq. 116 .or. array (i) .eq. 84))goto 23006
      esc = 9
      goto 23007
23006 continue
      esc = array (i)
23007 continue
23005 continue
23003 continue
23001 continue
      return
      end
