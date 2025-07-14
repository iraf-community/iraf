C--------------------------------------------------------------------------
        subroutine ftdbg(ounit,label,nrows)


C       insert a 2880-byte block at the end of the current header or data.

C       ounit   i  fortran output unit number
C       nblock  i  number of blocks to insert
C       hdrdat  i  insert space in header (0) or data (1)
C       status  i  returned error status (0=ok)

        integer ounit,nrows,i,ib,ibuff
        character*(*) label

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 20)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
        character*1 buff(2880,2)
        common/ftheap/buff
C       END OF COMMON BLOCK DEFINITIONS:------------------------------------

C       get the number of the data buffer used for this unit
        ibuff = bufnum(ounit)     
        ib = bufnum(ounit)     

        print *, ' '
        print *, '############## ',label,' unit:',ounit,' ibuff:',ib
        print *, 'chdu hdutyp maxhdu nxthdr'
        print *, chdu(ibuff),hdutyp(ibuff),maxhdu(ibuff),nxthdr(ibuff)
        print *, 'dtstrt ',dtstrt(ib)

        do 10 i=1,nrows
            print *, 'hdstrt/end ',i,' ',hdstrt(ib,i),' ',hdend(ib)
10      continue
        print *, '############## '

        end
