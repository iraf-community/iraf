      integer function cupper (c)
      integer c
      if (.not.(c .ge. 97 .and. c .le. 122))goto 23000
      cupper = c + (65 - 97)
      goto 23001
23000 continue
      cupper = c
23001 continue
      return
      end
