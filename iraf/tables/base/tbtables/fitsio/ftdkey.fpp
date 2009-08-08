C--------------------------------------------------------------------------
        subroutine ftdkey(iunit,keynam,status)

C       delete a header keyword
C
C       iunit   i  fortran output unit number
C       keynam  c  keyword name    ( 8 characters, cols.  1- 8)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Feb 1992

        character*(*) keynam
        integer iunit,status,i,lenval,nkeys,keypos
        character keybuf*80,strval*70,comm*8,value*70,bslash*1,kname*8

        if (status .gt. 0)return

C       have to use 2 \\'s because the SUN compiler treats 1 \ as an escape
        bslash='\\'

C       find the keyword to be deleted
        call ftgcrd(iunit,keynam,keybuf,status)
        if (status .eq. 202)then
            kname=keynam
            call ftpmsg('FTDKEY could not find the '//kname//
     &      ' keyword to be deleted.')
            return
        end if

C       get the position of the keyword in the header
        call ftghps(iunit,nkeys,keypos,status)
        keypos=keypos-1

C       get position of last character in value string to see if it is a \ or &
        call ftpsvc(keybuf,strval,comm,status)
        call ftc2s(strval,value,status)
        do 10 i=70,1,-1
                if (value(i:i) .ne. ' ')then
                        lenval=i
                        go to 20
                end if
10      continue

C       now delete this keyword
20      call ftdrec(iunit,keypos,status)
        if (status .gt. 0)return

C       test if this keyword was also continued
        if (value(lenval:lenval) .eq. bslash .or.
     &          value(lenval:lenval) .eq. '&')then
                call ftgnst(iunit,value,lenval,comm,status)
                if (lenval .gt. 0)go to 20
        end if
        end
