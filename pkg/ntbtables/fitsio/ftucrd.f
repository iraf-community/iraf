C--------------------------------------------------------------------------
        subroutine ftucrd(ounit,keywrd,card,status)

C       update a 80-character FITS header card/record
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       card    c  80-character FITS card image
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, May 1995

        character*(*) keywrd,card
        integer ounit,status,tstat

        if (status .gt. 0)return
        tstat=status

C       try modifying the card, if it exists
        call ftmcrd(ounit,keywrd,card,status)

        if (status .eq. 202)then
C               card doesn't exist, so create it
                status=tstat
                call ftprec(ounit,card,status)
        end if
        end
