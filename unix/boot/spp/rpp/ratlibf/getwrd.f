      integer function getwrd (in, i, out)
      integer in (100), out (100)
      integer i
      integer j
23000 if (.not.(in (i) .eq. 32 .or. in (i) .eq. 9))goto 23001
      i = i + 1
      goto 23000
23001 continue
      j = 1
23002 if (.not.(in (i) .ne. -2 .and. in (i) .ne. 32 .and. in (i) .ne. 9 
     *.and. in (i) .ne. 10))goto 23003
      out (j) = in (i)
      i = i + 1
      j = j + 1
      goto 23002
23003 continue
      out (j) = -2
      getwrd = j - 1
      return
      end
