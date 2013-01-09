C--------------------------------------------------------------------------
        subroutine ftghsp(ounit,nexist,nmore,status)

C       Get Header SPace
C       return the number of additional keywords that will fit in the header
C
C       ounit   i  Fortran I/O unit number
C       nexist  i  number of keywords already present in the CHU
C       nmore   i  number of additional keywords that will fit in header
C                 -1 indicates that there is no limit to the number of keywords
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,nexist,nmore,status

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
        ibuff=bufnum(ounit)

        nexist=(hdend(ibuff)-hdstrt(ibuff,chdu(ibuff)))/80
        if (dtstrt(ibuff) .lt. 0)then
C               the max size of the header has not been defined, so there
C               is no limit to the number of keywords which may be written.
                nmore=-1                
        else
                nmore=(dtstrt(ibuff)-hdend(ibuff))/80-1
        end if
        end
