      integer function length (str)
      integer str (100)
      length = 0
23000 if (.not.(str (length+1) .ne. -2))goto 23002
23001 length = length + 1
      goto 23000
23002 continue
      return
      end
