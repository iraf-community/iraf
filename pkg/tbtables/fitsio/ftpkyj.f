C--------------------------------------------------------------------------
        subroutine ftpkyj(ounit,keywrd,intval,comm,status)

C       write an integer value to a header record 
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       intval  i  keyword value 
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        integer ounit,status,intval
        character value*20

C       convert integer to character string
        call fti2c(intval,value,status)

C       write the keyword record
        call ftpkey(ounit,keywrd,value,comm,status)
        end
