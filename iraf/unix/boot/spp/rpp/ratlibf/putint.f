      subroutine putint (n, w, fd)
      integer n, w
      integer fd
      integer chars (20)
      integer junk
      integer itoc
      junk = itoc (n, chars, 20)
      call putstr (chars, w, fd)
      return
      end
