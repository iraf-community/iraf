      integer function push (ep, argstk, ap)
      integer ap, argstk (100), ep
      if (.not.(ap .gt. 100))goto 23000
      call baderr (19Harg stack overflow.)
23000 continue
      argstk (ap) = ep
      push = ap + 1
      return
      end
