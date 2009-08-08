C--------------------------------------------------------------------------
        subroutine ftdblk(ounit,nblock,hdrdat,status)

C       delete  2880-byte FITS blocks at the end of the current header or data

C       ounit   i  fortran output unit number
C       nblock  i  number of 2880-byte blocks to be deleted
C       hdrdat  i  delete space at end of header (0) or data (1)
C       status  i  returned error status (0=ok)

        integer ounit,nblock,hdrdat,status

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

        integer ibuff,jpoint,i,tstat

        if (status .gt. 0)return

C       get the number of the data buffer used for this unit
        ibuff=bufnum(ounit)

C       get address of first block to be deleted/overwritten
        if (hdrdat .eq. 0)then
            jpoint=dtstrt(ibuff)-2880*nblock
        else
            jpoint=hdstrt(ibuff,chdu(ibuff)+1)-2880*nblock
        end if

C       move each block up, until we reach the end of file
10      continue
C           move to the read start position
            tstat=status
            call ftmbyt(ounit,jpoint+nblock*2880,.false.,status)

C           read one 2880-byte FITS logical record 
            call ftgcbf(ounit,0,2880,buff,status)

C           check for end of file
            if (status .eq. 107)then
                status=tstat
                go to 20
            end if

C           move back to the write start postion
            call ftmbyt(ounit,jpoint,.false.,status)

C           write the 2880-byte FITS logical record
            call ftpcbf(ounit,0,2880,buff,status)

C           check for error 
            if (status .gt. 0)then
                call ftpmsg('Error deleting FITS blocks (FTDBLK)')
                return
            end if

C           increment pointer to next block and loop back
            jpoint=jpoint+2880
            go to 10
20      continue

C       now fill the last nblock blocks with zeros;  initialize the  buffer
        do 30 i=1,2880
            buff(i)=char(0)
30      continue

C       move back to the write start postion
        call ftmbyt(ounit,jpoint,.false.,status)

C       write the 2880-byte block NBLOCK times.
        do 40 i=1,nblock
            call ftpcbf(ounit,0,2880,buff,status)
40      continue

        if (hdrdat .eq. 0)then
C           recalculate the starting location of the current data unit, if moved
            dtstrt(ibuff)=dtstrt(ibuff)-2880*nblock
        end if

C       recalculate the starting location of all subsequent HDUs
        do 50 i=chdu(ibuff)+1,maxhdu(ibuff)+1
            hdstrt(ibuff,i)=hdstrt(ibuff,i)-2880*nblock
50      continue

        if (status .gt. 0)then
            call ftpmsg('Error deleting FITS block(s) (FTDBLK)')
        end if
        end
