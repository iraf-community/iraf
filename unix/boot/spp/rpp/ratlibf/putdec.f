      subroutine putdec(n,w)
      integer n, w
      integer chars (20)
      integer i, nd
      integer itoc
      nd = itoc (n, chars, 20)
      i = nd + 1
23000 if (.not.(i .le. w))goto 23002
      call putc (32)
23001 i = i + 1
      goto 23000
23002 continue
      i = 1
23003 if (.not.(i .le. nd))goto 23005
      call putc (chars (i))
23004 i = i + 1
      goto 23003
23005 continue
      return
      end
