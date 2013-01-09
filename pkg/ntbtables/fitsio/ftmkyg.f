C--------------------------------------------------------------------------
        subroutine ftmkyg(ounit,keywrd,dval,decim,comm,status)

C       modify a double precision value header record in F format
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       dval    d  keyword value 
C       decim   i  number of decimal places to display in value field
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        double precision dval
        integer ounit,status,decim
        character value*20,cmnt*48

C       find the old keyword
        call ftgkey(ounit,keywrd,value,cmnt,status)

C       check for special symbol indicating that comment should not be changed
        if (comm .ne. '&')then
              cmnt=comm
        end if

C       convert double precision to F format character string
        call ftd2f(dval,decim,value,status)

C       modify the keyword record
        call ftmkey(ounit,keywrd,value,cmnt,status)
        end
