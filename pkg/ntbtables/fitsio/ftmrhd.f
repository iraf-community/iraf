C----------------------------------------------------------------------
        subroutine ftmrhd(iunit,extmov,xtend,status)

C       Move Relative Header Data unit
C       move the i/o pointer to the specified HDU and initialize all
C       the common block parameters which describe the extension

C       iunit   i  fortran unit number
C       extmov  i  number of the extension to point to, relative to the CHDU
C       xtend   i  returned type of extension:   0 = the primary HDU
C                                                1 = an ASCII table
C                                                2 = a binary table
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June, 1991

        integer iunit,extmov,xtend,status

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

        integer ibuff,extno

        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       calculate the absolute HDU number, then move to it
        extno=chdu(ibuff)+extmov
        call ftmahd(iunit,extno,xtend,status)
        end        
