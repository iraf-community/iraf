      integer function alldig (str)
      integer str (100)
      integer i
      alldig = 0
      if (.not.(str (1) .eq. -2))goto 23000
      return
23000 continue
      i = 1
23002 if (.not.(str (i) .ne. -2))goto 23004
      if (.not.(.not.(48.le.str (i).and.str (i).le.57)))goto 23005
      return
23005 continue
23003 i = i + 1
      goto 23002
23004 continue
      alldig = 1
      return
      end
