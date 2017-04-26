      integer function equal (str1, str2)
      integer str1(100), str2(100)
      integer i
      i = 1
23000 if (.not.(str1 (i) .eq. str2 (i)))goto 23002
      if (.not.(str1 (i) .eq. -2))goto 23003
      equal=(1)
      return
23003 continue
23001 i = i + 1
      goto 23000
23002 continue
      equal=(0)
      return
      end
