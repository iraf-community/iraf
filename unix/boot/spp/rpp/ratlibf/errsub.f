      integer function errsub (arg, file, access)
      integer arg (100), file (100)
      integer access
      if (.not.(arg (1) .eq. 63 .and. arg (2) .ne. 63 .and. arg (2) .ne.
     * -2))goto 23000
      errsub = 1
      access = 2
      call scopy (arg, 2, file, 1)
      goto 23001
23000 continue
      if (.not.(arg (1) .eq. 63 .and. arg (2) .eq. 63 .and. arg (3) .ne.
     * -2))goto 23002
      errsub = 1
      access = 4
      call scopy (arg, 3, file, 1)
      goto 23003
23002 continue
      errsub = 0
23003 continue
23001 continue
      return
      end
