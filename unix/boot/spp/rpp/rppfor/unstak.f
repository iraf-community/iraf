      subroutine unstak (sp, lextyp, labval, token)
      integer labval(100), lextyp(100)
      integer sp, token, type
23000 if (.not.(sp .gt. 1))goto 23002
      type = lextyp(sp)
      if (.not.((type .eq. -98 .or. type .eq. -97) .and. token .eq. -86)
     *)goto 23003
      goto 23002
23003 continue
      if (.not.(type .eq. -99 .or. type .eq. -98 .or. type .eq. -97))got
     *o 23005
      type = 999
23005 continue
      if (.not.(type .eq. 123 .or. type .eq. -92))goto 23007
      goto 23002
23007 continue
      if (.not.(type .eq. 999 .and. token .eq. -87))goto 23009
      goto 23002
23009 continue
      if (.not.(type .eq. 999))goto 23011
      call indent (-1)
      call outcon (labval(sp))
      goto 23012
23011 continue
      if (.not.(type .eq. -87 .or. type .eq. -72))goto 23013
      if (.not.(sp .gt. 2))goto 23015
      sp = sp - 1
23015 continue
      if (.not.(type .ne. -72))goto 23017
      call indent (-1)
23017 continue
      call outcon (labval(sp) + 1)
      goto 23014
23013 continue
      if (.not.(type .eq. -96))goto 23019
      call dostat (labval(sp))
      goto 23020
23019 continue
      if (.not.(type .eq. -95))goto 23021
      call whiles (labval(sp))
      goto 23022
23021 continue
      if (.not.(type .eq. -94))goto 23023
      call fors (labval(sp))
      goto 23024
23023 continue
      if (.not.(type .eq. -93))goto 23025
      call untils (labval(sp), token)
23025 continue
23024 continue
23022 continue
23020 continue
23014 continue
23012 continue
23001 sp=sp-1
      goto 23000
23002 continue
      end
