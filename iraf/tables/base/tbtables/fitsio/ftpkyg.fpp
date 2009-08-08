C--------------------------------------------------------------------------
        subroutine ftpkyg(ounit,keywrd,dval,decim,comm,status)

C       write a double precision value to a header record in F format
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
        character value*20

C       convert double precision to F format character string
        call ftd2f(dval,decim,value,status)

C       write the keyword record
        call ftpkey(ounit,keywrd,value,comm,status)
        end
