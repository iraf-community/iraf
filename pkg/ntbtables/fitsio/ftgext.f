C----------------------------------------------------------------------
        subroutine ftgext(iunit,extno,xtend,status)

C       'Get Extension'
C       move i/o pointer to another extension (or the primary HDU) and
C       initialize all the common block parameters which describe the
C       extension

C       iunit   i  fortran unit number
C       extno   i  number of the extension to point to.
C       xtend   i  type of extension:   0 = the primary HDU
C                                       1 = an ASCII table
C                                       2 = a binary table
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

        integer ibuff,xchdu,xhdend,xmaxhd

        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       move to the beginning of the desired extension
        call ftmbyt(iunit,hdstrt(ibuff,extno),.false.,status)
        if (status .le. 0)then

C               temporarily save parameters
                xchdu=chdu(ibuff)
                xmaxhd=maxhdu(ibuff)
                xhdend=hdend(ibuff)

C               initialize various parameters about the CHDU
                chdu(ibuff)=extno
                maxhdu(ibuff)=max(extno,maxhdu(ibuff))
C               the location of the END record is currently unknown, so 
C               temporarily just set it to a very large number
                hdend(ibuff)=2000000000

C               determine the structure of the CHDU
                call ftrhdu(iunit,xtend,status)
                if (status .gt. 0)then
C                       couldn't read the extension so restore previous state
                        chdu(ibuff)= xchdu
                        maxhdu(ibuff)=xmaxhd
                        hdend(ibuff)= xhdend
                end if
        end if
        end
