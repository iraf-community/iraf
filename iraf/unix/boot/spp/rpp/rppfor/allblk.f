      integer function allblk (buf)
      integer buf (100)
      integer i
      allblk = 1
      i = 1
23000 if (.not.(buf (i) .ne. 10 .and. buf (i) .ne. -2))goto 23002
      if (.not.(buf (i) .ne. 32))goto 23003
      allblk = 0
      goto 23002
23003 continue
23001 i = i + 1
      goto 23000
23002 continue
      return
      end
