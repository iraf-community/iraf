      subroutine dsdump (form)
      integer form
      integer mem( 60000)
      common/cdsmem/mem
      integer p, t, q
      t = 2
      call remark (27H** DYNAMIC STORAGE DUMP **.)
      call putint (1, 5, 2)
      call putch (32, 2)
      call putint (2 + 1, 0, 2)
      call remark (14H words in use.)
      p = mem (t + 1)
23000 if (.not.(p .ne. 0))goto 23001
      call putint (p, 5, 2)
      call putch (32, 2)
      call putint (mem (p + 0), 0, 2)
      call remark (17H words available.)
      q = p + mem (p + 0)
23002 if (.not.(q .ne. mem (p + 1) .and. q .lt. mem (1)))goto 23003
      call dsdbiu (q, form)
      goto 23002
23003 continue
      p = mem (p + 1)
      goto 23000
23001 continue
      call remark (15H** END DUMP **.)
      return
      end
