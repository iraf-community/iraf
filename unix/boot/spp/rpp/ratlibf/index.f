      integer function index (str, c)
      integer str (100), c
      index = 1
23000 if (.not.(str (index) .ne. -2))goto 23002
      if (.not.(str (index) .eq. c))goto 23003
      return
23003 continue
23001 index = index + 1
      goto 23000
23002 continue
      index = 0
      return
      end
