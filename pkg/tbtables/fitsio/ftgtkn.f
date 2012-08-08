C--------------------------------------------------------------------------
        subroutine ftgtkn(iunit,nkey,keynam,ival,status)

C       test that keyword number NKEY has name = KEYNAM and get the
C       integer value of the keyword.  Return an error if the keyword
C       name does not match the input KEYNAM, or if the value of the
C       keyword is not a positive integer.
C
C       iunit   i  Fortran I/O unit number
C       nkey    i  sequence number of the keyword to test
C       keynam  c  name that the keyword is supposed to have
C       OUTPUT PARAMETERS:
C       ival    i  returned value of the integer keyword
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991
C
        integer iunit,nkey,status,ival
        character*(*) keynam
        character kname*8,value*30,comm*48,npos*8,keybuf*80

        if (status .gt. 0)return

C       read the name and value of the keyword
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

C       convert character string to integer
        call ftc2ii(value,ival,status)
        if (status .gt. 0 .or. ival .lt. 0 )then
C               keyword value must be zero or positive integer
                status=209
        end if

900     continue

        if (status .gt. 0)then
            write(npos,1000)nkey
1000        format(i8)
            call ftpmsg('FTGTKN found unexpected keyword or value '// 
     &      'for header keyword number '//npos//'.')
            call ftpmsg('  Was expecting positive integer keyword '//
     &      keynam(1:8))
            if (keybuf(9:10) .ne. '= ')then
                call ftpmsg('  but found the keyword '//kname//
     &          ' with no value field (no "= " in cols. 9-10).')
            else
              call ftpmsg('  but instead found keyword = '//kname//
     &        ' with value = '//value)
            end if
            call ftpmsg(keybuf)
        end if
        end
