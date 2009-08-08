C----------------------------------------------------------------------
        subroutine ftghad(iunit,curhdu,nxthdu)

C       return the starting byte address of the CHDU and the next HDU.

C       curhdu  i  starting address of the CHDU
C       nxthdu  i  starting address of the next HDU

C       written by Wm Pence, HEASARC/GSFC, May, 1995

        integer iunit,curhdu,nxthdu

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

        integer ibuff,hdunum

        ibuff=bufnum(iunit)
        hdunum=chdu(ibuff)
        curhdu=hdstrt(ibuff,hdunum)
        nxthdu=hdstrt(ibuff,hdunum+1)
        end        
