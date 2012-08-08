      integer function patsiz (pat, n)
      integer pat (128)
      integer n
      if (.not.(pat (n) .eq. 97 .or. pat (n) .eq. 123 .or. pat (n) .eq. 
     *125))goto 23000
      patsiz = 2
      goto 23001
23000 continue
      if (.not.(pat (n) .eq. 37 .or. pat (n) .eq. 36 .or. pat (n) .eq. 6
     *3))goto 23002
      patsiz = 1
      goto 23003
23002 continue
      if (.not.(pat (n) .eq. 91 .or. pat (n) .eq. 110))goto 23004
      patsiz = pat (n + 1) + 2
      goto 23005
23004 continue
      if (.not.(pat (n) .eq. 42))goto 23006
      patsiz = 4
      goto 23007
23006 continue
      call error (24Hin patsiz: can't happen.)
23007 continue
23005 continue
23003 continue
23001 continue
      return
      end
