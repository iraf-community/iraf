      integer function strcmp (str1, str2)
      integer str1 (100), str2 (100)
      integer i
      i = 1
23000 if (.not.(str1 (i) .eq. str2 (i)))goto 23002
      if (.not.(str1 (i) .eq. -2))goto 23003
      strcmp=(0)
      return
23003 continue
23001 i = i + 1
      goto 23000
23002 continue
      if (.not.(str1 (i) .eq. -2))goto 23005
      strcmp = -1
      goto 23006
23005 continue
      if (.not.(str2 (i) .eq. -2))goto 23007
      strcmp = + 1
      goto 23008
23007 continue
      if (.not.(str1 (i) .lt. str2 (i)))goto 23009
      strcmp = -1
      goto 23010
23009 continue
      strcmp = +1
23010 continue
23008 continue
23006 continue
      return
      end
