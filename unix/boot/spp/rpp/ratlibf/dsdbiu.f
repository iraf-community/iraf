      subroutine dsdbiu (b, form)
      integer b
      integer form
      integer mem( 60000)
      common/cdsmem/mem
      integer l, s, lmax
      integer blanks(6)
      data blanks(1)/9/,blanks(2)/32/,blanks(3)/32/,blanks(4)/32/,blanks
     *(5)/32/,blanks(6)/-2/
      call putint (b, 5, 2)
      call putch (32, 2)
      call putint (mem (b + 0), 0, 2)
      call remark (14H words in use.)
      l = 0
      s = b + mem (b + 0)
      if (.not.(form .eq. 48))goto 23000
      lmax = 5
      goto 23001
23000 continue
      lmax = 50
23001 continue
      b = b + 2
23002 if (.not.(b .lt. s))goto 23004
      if (.not.(l .eq. 0))goto 23005
      call putlin (blanks, 2)
23005 continue
      if (.not.(form .eq. 48))goto 23007
      call putint (mem (b), 10, 2)
      goto 23008
23007 continue
      if (.not.(form .eq. 97))goto 23009
      call putch (mem (b), 2)
23009 continue
23008 continue
      l = l + 1
      if (.not.(l .ge. lmax))goto 23011
      l = 0
      call putch (10, 2)
23011 continue
23003 b = b + 1
      goto 23002
23004 continue
      if (.not.(l .ne. 0))goto 23013
      call putch (10, 2)
23013 continue
      return
      end
