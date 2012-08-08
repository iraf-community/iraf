      integer function outsub (arg, file, access)
      integer arg (100), file (100)
      integer access
      if (.not.(arg (1) .eq. 62 .and. arg (2) .ne. 62 .and. arg (2) .ne.
     * -2))goto 23000
      outsub = 1
      access = 2
      call scopy (arg, 2, file, 1)
      goto 23001
23000 continue
      if (.not.(arg (1) .eq. 62 .and. arg (2) .eq. 62 .and. arg (3) .ne.
     * -2))goto 23002
      access = 4
      outsub = 1
      call scopy (arg, 3, file, 1)
      goto 23003
23002 continue
      outsub = 0
23003 continue
23001 continue
      return
      end
