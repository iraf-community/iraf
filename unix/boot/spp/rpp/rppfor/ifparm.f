      integer function ifparm (strng)
      integer strng (100)
      integer c
      external index
      integer i, index, type
      c = strng (1)
      if (.not.(c .eq. -12 .or. c .eq. -13 .or. c .eq. -11 .or. c .eq. -
     *14 .or. c .eq. -10))goto 23000
      ifparm = 1
      goto 23001
23000 continue
      ifparm = 0
      i = 1
23002 if (.not.(index (strng (i), 36) .gt. 0))goto 23004
      i = i + index (strng (i), 36)
      if (.not.(type (strng (i)) .eq. 48))goto 23005
      if (.not.(type (strng (i + 1)) .ne. 48))goto 23007
      ifparm = 1
      goto 23004
23007 continue
23005 continue
23003 goto 23002
23004 continue
23001 continue
      return
      end
