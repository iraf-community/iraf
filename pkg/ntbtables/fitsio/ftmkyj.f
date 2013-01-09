C--------------------------------------------------------------------------
        subroutine ftmkyj(ounit,keywrd,intval,comm,status)

C       modify an integer value header record 
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       intval  i  keyword value 
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        integer ounit,status,intval
        character value*20,cmnt*48

C       find the old keyword
        call ftgkey(ounit,keywrd,value,cmnt,status)

C       check for special symbol indicating that comment should not be changed
        if (comm .ne. '&')then
              cmnt=comm
        end if

C       convert integer to character string
        call fti2c(intval,value,status)

C       modify the keyword record
        call ftmkey(ounit,keywrd,value,cmnt,status)
        end
