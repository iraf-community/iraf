      integer function strim (str)
      integer str (100)
      integer lnb, i
      lnb = 0
      i = 1
23000 if (.not.(str (i) .ne. -2))goto 23002
      if (.not.(str (i) .ne. 32 .and. str (i) .ne. 9))goto 23003
      lnb = i
23003 continue
23001 i = i + 1
      goto 23000
23002 continue
      str (lnb + 1) = -2
      strim=(lnb)
      return
      end
