C----------------------------------------------------------------------
        subroutine ftcpdt(iunit,ounit,status)

C       copies the data from the IUNIT CHDU to the data of the OUNIT CHDU.
C       This will overwrite any data already in the OUNIT CHDU.

C       iunit   i  fortran unit number of the input file to be copied
C       ounit   i  fortran unit number of the output file to be copied to
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Aug 1993

        integer iunit,ounit,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 20)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
        character*1 cbuff(2880), xdummy(2880)
        common/ftheap/cbuff,xdummy
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,obuff,nblock,i

        if (status .gt. 0)return

        if (iunit .eq. ounit)then
                status=101
                return
        end if

        ibuff=bufnum(iunit)
        obuff=bufnum(ounit)

C       determine HDU structure as defined by keywords in output file
        call ftrdef(ounit,status)

C       Calculate the number of bytes to be copied.  By definition there
C       will be an integral number of 2880-byte logical blocks to be copied
        nblock=(hdstrt(ibuff,chdu(ibuff)+1)-dtstrt(ibuff))/2880

        if (nblock .gt. 0)then
C           move to the beginning of the data in the input and output files
            call ftmbyt(iunit,dtstrt(ibuff),.false.,status)
            call ftmbyt(ounit,dtstrt(obuff),.true.,status)

C           now copy the data one block at a time
            do 30 i=1,nblock
                call ftgcbf(iunit,0,2880,cbuff,status)
                call ftpcbf(ounit,0,2880,cbuff,status)
30          continue
        end if
        end
