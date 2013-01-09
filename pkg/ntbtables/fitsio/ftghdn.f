C----------------------------------------------------------------------
        subroutine ftghdn(iunit,hdunum)

C       return the number of the current header data unit.  The
C       first HDU (the primary array) is number 1.

C       iunit   i  fortran unit number
C       hdunum  i  returned number of the current HDU 
C
C       written by Wm Pence, HEASARC/GSFC, March, 1993

        integer iunit,hdunum

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

        hdunum=chdu(bufnum(iunit))
        end        
