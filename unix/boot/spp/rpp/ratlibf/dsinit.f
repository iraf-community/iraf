      subroutine dsinit (w)
      integer w
      integer mem( 60000)
      common/cdsmem/mem
      integer t
      if (.not.(w .lt. 2 * 2 + 2))goto 23000
      call error (42Hin dsinit: unreasonably small memory size.)
23000 continue
      t = 2
      mem (t + 0) = 0
      mem (t + 1) = 2 + 2
      t = 2 + 2
      mem (t + 0) = w - 2 - 1
      mem (t + 1) = 0
      mem (1) = w
      return
      end
