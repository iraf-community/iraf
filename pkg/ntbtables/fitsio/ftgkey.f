C--------------------------------------------------------------------------
        subroutine ftgkey(iunit,keynam,value,comm,status)

C       Read value and comment of a header keyword from the keyword buffer

C       iunit   i  Fortran I/O unit number
C       keynam  c  name of keyword to be read
C       OUTPUT PARAMETERS:
C       value   c  output value of the keyword, if any
C       comm    c  output comment string, if any, of the keyword
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June, 1991

        integer iunit,status
        character*(*) keynam,value,comm
        character*80 keybuf

        call ftgcrd(iunit,keynam,keybuf,status)
        if (status .le. 0)then
C               parse the record to find value and comment strings
                call ftpsvc(keybuf,value,comm,status)
        end if
        end
