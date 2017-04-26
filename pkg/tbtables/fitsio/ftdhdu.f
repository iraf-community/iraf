C--------------------------------------------------------------------------
        subroutine ftdhdu(ounit,typhdu,status)

C       delete the current HDU (as long as it is not the primary array)

C       ounit   i  fortran output unit number
C       typhdu  i  type of the new CHDU, after deleting the old CHDU
C       status  i  returned error status (0=ok)

        integer ounit,typhdu,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 20)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
        character*1 buff(5760)
        common/ftheap/buff
C       END OF COMMON BLOCK DEFINITIONS:------------------------------------

        integer ibuff,nhdu,nblock

        if (status .gt. 0)return

C       get the number of the data buffer used for this unit
        ibuff=bufnum(ounit)

        nhdu=chdu(ibuff)
        if (nhdu .eq. 1)then
C            cannot delete the primary array
             status=301
             return
        end if

C       close the CHDU first, to flush buffers and free memory
        call ftchdu(ounit,status)

C       how many blocks to delete?
        nblock=(hdstrt(ibuff,nhdu+1)-hdstrt(ibuff,nhdu))/2880
        if (nblock .lt. 1)return

C       delete the blocks
        call ftdblk(ounit,nblock,1,status)
        if (status .gt. 0)return

C       try reinitializing the CHDU, if there is one
        call ftrhdu(ounit,typhdu,status)
        if (status .gt. 0)then
C            there is no HDU after the one we just deleted so move back one HDU
             status=0
             call ftcmsg
             call ftgext(ounit,nhdu-1,typhdu,status)
        end if
        end
