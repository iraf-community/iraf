      integer function insub (arg, file)
      integer arg (100), file (100)
      if (.not.(arg (1) .eq. 60 .and. arg (2) .ne. -2))goto 23000
      insub = 1
      call scopy (arg, 2, file, 1)
      goto 23001
23000 continue
      insub = 0
23001 continue
      return
      end
