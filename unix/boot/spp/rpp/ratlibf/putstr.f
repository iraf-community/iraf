      subroutine putstr (str, w, fd)
      integer str (100)
      integer w
      integer fd
      integer length
      integer i, len
      len = length (str)
      i = len + 1
23000 if (.not.(i .le. w))goto 23002
      call putch (32, fd)
23001 i = i + 1
      goto 23000
23002 continue
      i = 1
23003 if (.not.(i .le. len))goto 23005
      call putch (str (i), fd)
23004 i = i + 1
      goto 23003
23005 continue
      i = (-w) - len
23006 if (.not.(i .gt. 0))goto 23008
      call putch (32, fd)
23007 i = i - 1
      goto 23006
23008 continue
      return
      end
