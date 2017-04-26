C----------------------------------------------------------------------
        subroutine ftmahd(iunit,extno,xtend,status)

C       Move to Absolute Header Data unit
C       move the i/o pointer to the specified HDU and initialize all
C       the common block parameters which describe the extension

C       iunit   i  fortran unit number
C       extno   i  number of the extension to point to.
C       xtend   i  returned type of extension:   0 = the primary HDU
C                                                1 = an ASCII table
C                                                2 = a binary table
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June, 1991

        integer iunit,extno,xtend,status

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

        integer ibuff,movto,tstat

        if (status .gt. 0)then
            return
        else if (extno .le. 0 .or. extno .ge. ne)then
            status=301
            return
        end if

        ibuff=bufnum(iunit)

C       check if we are already positioned to the correct HDU
        if (extno .eq. chdu(ibuff))then
C           just return the type of extension
            xtend=hdutyp(ibuff)
        else

C           now move to the extension, or the highest one we know about
10          movto=min(extno,maxhdu(ibuff)+1)

C           before closing out the CHDU, make sure the new extension exists
            call ftmbyt(iunit,hdstrt(ibuff,movto),.false.,status)
            if (status .gt. 0)return

C           close out the current HDU before moving to the new one
            call ftchdu(iunit,status)
            if (status .gt. 0)then
                call ftpmsg('FTMAHD could not close the'//
     &              ' current HDU before moving to the new HDU.')
                return
            end if

            call ftgext(iunit,movto,xtend,status)
            if (status .gt. 0)then
C               failed to move to new extension, so restore previous extension
                tstat=0
                call ftrhdu(iunit,movto,tstat)
                return
            end if

C           continue reading extensions until we get to the one we want
            if (movto .lt. extno)go to 10
        end if
        end        
