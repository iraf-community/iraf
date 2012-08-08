C--------------------------------------------------------------------------
        subroutine fttkyn(iunit,nkey,keynam,keyval,status)

C       test that the keyword number NKEY has name = KEYNAM
C       and has value = KEYVAL
C
C       iunit   i  Fortran I/O unit number
C       nkey    i  sequence number of the keyword to test
C       keynam  c  name that the keyword is supposed to have
C       keyval  c  value that the keyword is supposed to have
C       OUTPUT PARAMETERS:
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991
C
        integer iunit,nkey,status
        character*(*) keynam,keyval
        character kname*8,value*30,comm*48,npos*8,keybuf*80
        character errmsg*80

        if (status .gt. 0)return

C       read the name and value of the keyword

C       get the whole record
        call ftgrec(iunit,nkey,keybuf,status)

        kname=keybuf(1:8)
C       parse the value and comment fields from the record
        call ftpsvc(keybuf,value,comm,status)
        if (status .gt. 0)go to 900

C       test if the keyword has the correct name
        if (kname .ne. keynam)then
                status=208
                go to 900
        end if

C       check that the keyword has the correct value
        if (value .ne. keyval)then
                status=209
        end if

900     continue
        if (status .gt. 0)then

            write(npos,1000)nkey
1000        format(i8)
            errmsg='FTTKYN found unexpected keyword or value '//
     &      'for header keyword number '//npos//'.'
            call ftpmsg(errmsg)
            errmsg='  Was expecting keyword '//keynam//
     &      ' with value = '//keyval
            call ftpmsg(errmsg)
            if (keybuf(9:10) .ne. '= ')then
              errmsg='      but found keyword '//kname//
     &      ' with no "= " in cols. 9-10.'
            else
              errmsg='      but found keyword '//kname//
     &      ' with value = '//value
            end if
            call ftpmsg(errmsg)
            call ftpmsg(keybuf)
        end if
        end
