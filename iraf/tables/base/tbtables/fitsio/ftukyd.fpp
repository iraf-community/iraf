C--------------------------------------------------------------------------
        subroutine ftukyd(ounit,keywrd,dval,decim,comm,status)

C       update a double precision value header record in E format
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       dval    d  keyword value 
C       decim   i  number of decimal places to display in value field
C       comm    c  keyword comment (max. 47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Oct 1994

        character*(*) keywrd,comm
        double precision dval
        integer ounit,status,decim,tstat

        if (status .gt. 0)return
        tstat=status

C       try modifying the keyword, if it exists
        call ftmkyd(ounit,keywrd,dval,decim,comm,status)

        if (status .eq. 202)then
C               keyword doesn't exist, so create it
                status=tstat
                call ftpkyd(ounit,keywrd,dval,decim,comm,status)
        end if
        end
