      integer function type (c)
      integer c
      if (.not.((97 .le. c .and. c .le. 122) .or. (65 .le. c .and. c .le
     *. 90)))goto 23000
      type = 97
      goto 23001
23000 continue
      if (.not.(48 .le. c .and. c .le. 57))goto 23002
      type = 48
      goto 23003
23002 continue
      type = c
23003 continue
23001 continue
      return
      end
