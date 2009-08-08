C--------------------------------------------------------------------------
        subroutine ftmkyf(ounit,keywrd,rval,decim,comm,status)

C       modify a real*4 value header record in F format
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       rval    r  keyword value 
C       decim   i  number of decimal places to display in value field
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        real rval
        integer ounit,status,decim
        character value*20,cmnt*48

C       find the old keyword
        call ftgkey(ounit,keywrd,value,cmnt,status)

C       check for special symbol indicating that comment should not be changed
        if (comm .ne. '&')then
              cmnt=comm
        end if

C       convert real to F format character string
        call ftr2f(rval,decim,value,status)

C       write the keyword record
        call ftmkey(ounit,keywrd,value,cmnt,status)
        end
