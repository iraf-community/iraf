C--------------------------------------------------------------------------
        subroutine ftukyj(ounit,keywrd,intval,comm,status)

C       update an integer value header record 
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       intval  i  keyword value 
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Oct 1994

        character*(*) keywrd,comm
        integer ounit,status,intval,tstat

        if (status .gt. 0)return
        tstat=status

C       try modifying the keyword, if it exists
        call ftmkyj(ounit,keywrd,intval,comm,status)

        if (status .eq. 202)then
C               keyword doesn't exist, so create it
                status=tstat
                call ftpkyj(ounit,keywrd,intval,comm,status)
        end if
        end
