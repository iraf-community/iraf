      integer function clower(c)
      integer c
      integer k
      if (.not.(c .ge. 65 .and. c .le. 90))goto 23000
      k = 97 - 65
      clower = c + k
      goto 23001
23000 continue
      clower = c
23001 continue
      return
      end
