C--------------------------------------------------------------------------
        subroutine ftukys(ounit,keywrd,strval,comm,status)

C       update a character string value header record 

C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       strval  c  keyword value 
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Oct 1994

        character*(*) keywrd,strval,comm
        integer ounit,status,tstat

        if (status .gt. 0)return

        tstat=status
C       try modifying the keyword, if it exists
        call ftmkys(ounit,keywrd,strval,comm,status)

        if (status .eq. 202)then
C               keyword doesn't exist, so create it
                status=tstat
C               note that this supports the HEASARC long-string conventions
                call ftpkls(ounit,keywrd,strval,comm,status)
        end if
        end
