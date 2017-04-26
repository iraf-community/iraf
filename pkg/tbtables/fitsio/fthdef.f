C--------------------------------------------------------------------------
        subroutine fthdef(ounit,moreky,status)

C       Header DEFinition
C       define the size of the current header unit; this simply lets
C       us determine where the data unit will start
C
C       ounit   i  Fortran I/O unit number
C       moreky  i  number of additional keywords to reserve space for
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,moreky,status

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
        
        integer ibuff,mkeys

        if (status .gt. 0)return

C       based on the number of keywords which have already been written,
C       plus the number of keywords to reserve space for, we then can
C       define where the data unit should start (it must start at the
C       beginning of a 2880-byte logical block).

        ibuff=bufnum(ounit)

        mkeys=max(moreky,0)
        dtstrt(ibuff)=((hdend(ibuff)+mkeys*80)/2880+1)*2880
        end
