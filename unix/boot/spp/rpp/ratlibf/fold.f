      subroutine fold (token)
      integer token (100)
      integer clower
      integer i
      i = 1
23000 if (.not.(token (i) .ne. -2))goto 23002
      token (i) = clower (token (i))
23001 i = i + 1
      goto 23000
23002 continue
      return
      end
