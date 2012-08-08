      subroutine pbstr (s)
      integer s(100)
      integer lenstr, i
      integer length
      lenstr = length (s)
      if (.not.(s(1) .eq. 46 .and. s(lenstr) .eq. 46))goto 23000
      if (.not.(lenstr .eq. 4))goto 23002
      if (.not.(s(2) .eq. 103))goto 23004
      if (.not.(s(3) .eq. 116))goto 23006
      call putbak (62)
      return
23006 continue
      if (.not.(s(3) .eq. 101))goto 23008
      call putbak (61)
      call putbak (62)
      return
23008 continue
23007 continue
      goto 23005
23004 continue
      if (.not.(s(2) .eq. 108))goto 23010
      if (.not.(s(3) .eq. 116))goto 23012
      call putbak (60)
      return
23012 continue
      if (.not.(s(3) .eq. 101))goto 23014
      call putbak (61)
      call putbak (60)
      return
23014 continue
23013 continue
      goto 23011
23010 continue
      if (.not.(s(2) .eq. 101 .and. s(3) .eq. 113))goto 23016
      call putbak (61)
      call putbak (61)
      return
23016 continue
      if (.not.(s(2) .eq. 110 .and. s(3) .eq. 101))goto 23018
      call putbak (61)
      call putbak (33)
      return
23018 continue
      if (.not.(s(2) .eq. 111 .and. s(3) .eq. 114))goto 23020
      call putbak (124)
      return
23020 continue
23019 continue
23017 continue
23011 continue
23005 continue
      goto 23003
23002 continue
      if (.not.(lenstr .eq. 5))goto 23022
      if (.not.(s(2) .eq. 110 .and. s(3) .eq. 111 .and. s(4) .eq. 116))g
     *oto 23024
      call putbak (33)
      return
23024 continue
      if (.not.(s(2) .eq. 97 .and. s(3) .eq. 110 .and. s(4) .eq. 100))go
     *to 23026
      call putbak (38)
      return
23026 continue
23025 continue
23022 continue
23003 continue
23000 continue
      i=lenstr
23028 if (.not.(i .gt. 0))goto 23030
      call putbak (s(i))
23029 i=i-1
      goto 23028
23030 continue
      end
