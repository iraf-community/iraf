      integer function ctoc (from, to, len)
      integer len
      integer from (100), to (len)
      integer i
      i = 1
23000 if (.not.(i .lt. len .and. from (i) .ne. -2))goto 23002
      to (i) = from (i)
23001 i = i + 1
      goto 23000
23002 continue
      to (i) = -2
      ctoc=(i - 1)
      return
      end
