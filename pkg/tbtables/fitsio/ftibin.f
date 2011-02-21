C--------------------------------------------------------------------------
        subroutine ftibin(ounit,nrows,nfield,ttype,tform,tunit,
     &                    extnam,pcount,status)

C       insert an binary table extension following the current HDU 

C       ounit   i  fortran output unit number
C       nrows   i  number of rows in the table
C       nfield  i  number of fields in the table
C       ttype   c  name of each field (array) (optional)
C       tform   c  format of each field (array)
C       tunit   c  units of each field (array) (optional)
C       extnam  c  name of table extension (optional)
C       pcount  i  size of special data area following the table (usually = 0)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0=OK)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,nrows,nfield,pcount,status
        character*(*) ttype(*),tform(*),tunit(*),extnam

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

        integer ibuff,nhdu,i,savstr,nblock,hsize,nkey

        if (status .gt. 0)return
        ibuff=bufnum(ounit)

C       close the current HDU to make sure END and fill values are written
        call ftchdu(ounit,status)
        if (status .gt. 0)return

C       save the starting address of the next HDU
        nhdu=chdu(ibuff)+1
        savstr=hdstrt(ibuff,nhdu)

C       count number of optional TUNITS keywords to be written
        nkey=0
        do 5 i=1,nfield
               if (tunit(i) .ne. ' ')nkey=nkey+1
5       continue
        if (extnam .ne. ' ')nkey=nkey+1
    
C       calc min size of header
        nblock=(9 + 2*nfield + nkey +35)/36
        hsize=nblock*2880

C       define a fake CHDU with a minimum header
        dtstrt(ibuff)=hdstrt(ibuff,chdu(ibuff))+hsize

C       define the size of the new HDU (this modifies hdstrt(ibuff,nhdu))
        call ftbdef(ounit,nfield,tform,pcount,nrows,status)

C       use start of next HDU to calc. how big this new HDU is
        nblock=(hdstrt(ibuff,nhdu)-hdstrt(ibuff,nhdu-1))/2880

C       reset the start of the next HDU back to it original value
        hdstrt(ibuff,nhdu)=savstr

C       insert the required number of blocks at the end of the real CHDU
C       (first define hdutyp so that the correct fill value will be used) 
        hdutyp(ibuff)=2
        call ftiblk(ounit,nblock,1,status)
        if (status .gt. 0)return

C       increment the number of HDUs in the file and their starting address
        maxhdu(ibuff)=maxhdu(ibuff)+1
        do 10 i=maxhdu(ibuff),nhdu,-1
                hdstrt(ibuff,i+1)=hdstrt(ibuff,i)
10      continue

C       again, reset the start of the next HDU back to it original value
        hdstrt(ibuff,nhdu)=savstr

C       flush the buffers holding data for the old HDU
        call ftflsh(ibuff,status)

C       recover common block space containing column descriptors for old HDU
        call ftfrcl(ounit,status)

C       move to the new (empty) HDU
        chdu(ibuff)=nhdu

C       set parameters describing an empty header
        hdutyp(ibuff)=2
        nxthdr(ibuff)=hdstrt(ibuff,nhdu)
        hdend(ibuff)= hdstrt(ibuff,nhdu)
        dtstrt(ibuff)=hdstrt(ibuff,nhdu)+hsize

C       write the header keywords
        call ftphbn(ounit,nrows,nfield,ttype,tform,tunit,extnam,
     &              pcount,status)

C       define the structure of the new HDU
        call ftbdef(ounit,nfield,tform,pcount,nrows,status)
        end
