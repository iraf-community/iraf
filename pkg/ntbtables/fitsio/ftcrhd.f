C----------------------------------------------------------------------
        subroutine ftcrhd(iunit,status)

C       'CReate Header Data unit'
C       create, initialize, and move the i/o pointer to a new extension at 
C       the end of the FITS file.

C       iunit   i  fortran unit number
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June, 1991

        integer iunit,status

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

        integer ibuff

        if (status .gt. 0)return

C       close the current HDU
        call ftchdu(iunit,status)
        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       check that we haven't exceeded the maximum allowed number of extensions
        if (maxhdu(ibuff)+1 .ge. ne)then
                status=301
                return
        end if

C       move to the end of the highest known extension
        call ftmbyt(iunit,hdstrt(ibuff,maxhdu(ibuff)+1),.true.,status)

C       initialize various parameters about the CHDU
        maxhdu(ibuff)=maxhdu(ibuff)+1
        chdu(ibuff)=maxhdu(ibuff)
        nxthdr(ibuff)=hdstrt(ibuff,chdu(ibuff))
C       the logical location of the END record at the start of the header
        hdend(ibuff)=nxthdr(ibuff)
C       the data start location is undefined
        dtstrt(ibuff)=-2000000000
        end
