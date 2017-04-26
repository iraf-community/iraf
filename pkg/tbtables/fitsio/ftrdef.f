C--------------------------------------------------------------------------
        subroutine ftrdef(ounit,status)

C       ReDEFine the structure of a data unit.  This routine re-reads
C       the CHDU header keywords to determine the structure and length of the
C       current data unit.  This redefines the start of the next HDU.
C
C       ounit   i  Fortran I/O unit number
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Oct 1993

        integer ounit,status
      
C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 20)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,dummy

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

C       see if we have write access to this file (no need to go on, if not)
        if (wrmode(ibuff))then
C           rewrite the header END card, and following blank fill
            call ftwend(ounit,status)
            if (status .gt. 0)return
 
C           now re-read the required keywords to determine the structure
            call ftrhdu(ounit,dummy,status)
        end if
        end
