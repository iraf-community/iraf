C--------------------------------------------------------------------------
        subroutine ftgkyn(iunit,nkey,keynam,value,comm,status)

C       Read value and comment of the NKEYth header record 
C       This routine is useful for reading the entire header, one
C       record at a time.

C       iunit   i  Fortran I/O unit number
C       nkey    i  sequence number (starting with 1) of the keyword to read
C       OUTPUT PARAMETERS:
C       keynam  c  output name of the keyword
C       value   c  output value of the keyword, if any
C       comm    c  output comment string, if any, of the keyword
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,nkey,status
        character*(*) keynam,value,comm
        character keybuf*80,arec*8

        if (status .gt. 0)return

        call ftgrec(iunit,nkey,keybuf,status)
        if (status .gt. 0)return

        keynam=keybuf(1:8)

C       parse the value and comment fields from the record
        call ftpsvc(keybuf,value,comm,status)
        if (status .gt. 0)return

C       Test that keyword name contains only valid characters.
C       This also serves as a check in case there was no END keyword and
C       program continues to read on into the data unit
        call fttkey(keybuf(1:8),status)
        if (status .gt. 0)then
            write(arec,1000)nkey
1000        format(i8)
            call ftpmsg('Name of header keyword number'//arec//
     &     ' contains illegal character(s):')
            call ftpmsg(keybuf)

C          see if we are at the beginning of FITS logical record
           if (nkey-1 .eq. (nkey-1)/36*36 .and. nkey .gt. 1)then
             call ftpmsg('(This may indicate a missing END keyword).')
           end if
        end if
        end
