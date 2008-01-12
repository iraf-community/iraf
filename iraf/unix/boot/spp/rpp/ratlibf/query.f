      subroutine query (mesg)
      integer mesg (100)
      integer getarg
      integer arg1 (3), arg2 (1)
      if (.not.(getarg (1, arg1, 3) .ne. -1 .and. getarg (2, arg2, 1) .e
     *q. -1))goto 23000
      if (.not.(arg1 (1) .eq. 63 .and. arg1 (2) .eq. -2))goto 23002
      call error (mesg)
23002 continue
23000 continue
      return
      end
