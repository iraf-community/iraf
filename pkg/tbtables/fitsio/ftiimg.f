C--------------------------------------------------------------------------
        subroutine ftiimg(ounit,bitpix,naxis,naxes,status)

C       insert an IMAGE extension following the current HDU 

C       ounit   i  fortran output unit number
C       bitpix  i  number of bits per data value
C       naxis   i  number of axes in the data array
C       naxes   i  array giving the length of each data axis
C       status  i  returned error status (0=ok)

        integer ounit,bitpix,naxis,naxes(*),status

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

        integer ibuff,nhdu,i,savstr,nblock

        if (status .gt. 0)return
        ibuff=bufnum(ounit)

C       close the current HDU to make sure END and fill values are written
        call ftchdu(ounit,status)
        if (status .gt. 0)return

C       save the starting address of the next HDU
        nhdu=chdu(ibuff)+1
        savstr=hdstrt(ibuff,nhdu)

C       define a fake CHDU with a one block header
        dtstrt(ibuff)=hdstrt(ibuff,chdu(ibuff))+2880

C       define the size of the new HDU (this modifies hdstrt(ibuff,nhdu))
        call ftpdef(ounit,bitpix,naxis,naxes,0,1,status)

C       use start of next HDU to calc. how big this new HDU is
        nblock=(hdstrt(ibuff,nhdu)-hdstrt(ibuff,nhdu-1))/2880

C       reset the start of the next HDU back to it original value
        hdstrt(ibuff,nhdu)=savstr

C       insert the required number of blocks at the end of the real CHDU
C       (first define hdutyp so that the correct fill value will be used) 
        hdutyp(ibuff)=0
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

C       set parameters describing an empty 1 block header
        hdutyp(ibuff)=0
        nxthdr(ibuff)=hdstrt(ibuff,nhdu)
        hdend(ibuff)= hdstrt(ibuff,nhdu)
        dtstrt(ibuff)=hdstrt(ibuff,nhdu)+2888

C       write the header keywords
        call ftphpr(ounit,.true.,bitpix,naxis,naxes,0,1,.true.,status)

C       define the structure of the new HDU
        call ftpdef(ounit,bitpix,naxis,naxes,0,1,status)
        end
