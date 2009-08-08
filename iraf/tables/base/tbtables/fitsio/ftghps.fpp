C--------------------------------------------------------------------------
        subroutine ftghps(iunit,nkeys,pos,status)

C       Get Header Position
C       get the number of keywords in the header and the current position 
C       in the header, i.e.,  the number of the next keyword record that 
C       would be read.  
C
C       iunit   i  Fortran I/O unit number
C       pos     i  current position in header (1 = beginning of header)
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Jan 1995

        integer iunit,nkeys,pos,status

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

        ibuff=bufnum(iunit)
        nkeys=(hdend(ibuff)-hdstrt(ibuff,chdu(ibuff)))/80
        pos=(nxthdr(ibuff)-hdstrt(ibuff,chdu(ibuff)))/80+1
        end
