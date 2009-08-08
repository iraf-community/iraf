C--------------------------------------------------------------------------
        subroutine ftpkye(ounit,keywrd,rval,decim,comm,status)

C       write a real*4 value to a header record in E format
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
        character value*20

C       convert real to E format character string
        call ftr2e(rval,decim,value,status)

C       write the keyword record
        call ftpkey(ounit,keywrd,value,comm,status)
        end
