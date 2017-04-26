      subroutine fcopy (in, out)
      integer in, out
      integer line (128)
      integer getlin
23000 if (.not.(getlin (line, in) .ne. -1))goto 23001
      call putlin (line, out)
      goto 23000
23001 continue
      return
      end
