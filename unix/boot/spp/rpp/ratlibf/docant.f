      subroutine docant(name)
      integer name(100), prog(30)
      integer length
      integer getarg
      length = getarg(0, prog, 30)
      if (.not.(length .ne. -1))goto 23000
      call putlin(prog, 2)
      call putch(58, 2)
      call putch(32, 2)
23000 continue
      call cant(name)
      return
      end
