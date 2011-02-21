C--------------------------------------------------------------------------
        subroutine ftdrec(ounit,pos,status)

C       delete keyword record at position POS from header
C
C       ounit   i  fortran output unit number
C       pos     i  position of keyword to be deleted (1 = first keyword)
C       OUTPUT PARAMETERS
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Jan 1995

        integer ounit,pos,status

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 20)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
C-------END OF COMMON BLOCK DEFINITIONS:------- -----------------------------

        character*80 keybuf,keytmp
        integer ibuff,i,j,nshift

        if (status .gt. 0)return

C       get the number of the data buffer used for this unit
        ibuff=bufnum(ounit)

        if (pos .lt. 1 .or. pos .gt. 
     &     (hdend(ibuff)-hdstrt(ibuff,chdu(ibuff)))/80)then
                status=203
                return
        end if

        nxthdr(ibuff)=hdstrt(ibuff,chdu(ibuff))+(pos-1)*80

C       calculate number of header records following the deleted record
        nshift=(hdend(ibuff)-nxthdr(ibuff))/80

C       go through header shifting each 80 byte record up one place to
C       fill in the gap created by the deleted keyword
        j=hdend(ibuff)
        keybuf=' '
        do 10 i=1,nshift
                j=j-80
C               read current record contents
                call ftmbyt(ounit,j,.false.,status)
                call ftgcbf(ounit,0,80,keytmp,status)
C               overwrite with new contents
                call ftmbyt(ounit,j,.false.,status)
                call ftpcbf(ounit,0,80,keybuf,status)
                keybuf=keytmp
10      continue

C       update end-of-header pointer
        hdend(ibuff)=hdend(ibuff)-80

100     continue
        end
