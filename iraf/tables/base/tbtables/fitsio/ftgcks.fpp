C----------------------------------------------------------------------
        subroutine ftgcks(iunit,datsum,chksum,status)

C       calculate and encode the checksums of the data unit and the total HDU 

C       iunit   i  fortran unit number
C       datsum  d  output  checksum for the data
C       chksum  d  output  checksum for the entire HDU 
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Sept, 1994

        integer iunit,status
        double precision datsum,chksum

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 20)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
C-------END OF COMMON BLOCK DEFINITIONS:------------------------------------

        integer ibuff,nrec

        if (status .gt. 0)return

C       calculate number of data records
        ibuff=bufnum(iunit)
        nrec=(hdstrt(ibuff,chdu(ibuff)+1)-dtstrt(ibuff))/2880

        datsum=0.
        if (nrec .gt. 0)then

C           move to the start of the data
            call ftmbyt(iunit,dtstrt(ibuff),.true.,status)

C           accumulate the 32-bit 1's complement checksum
            call ftcsum(iunit,nrec,datsum,status)
        end if

C       move to the start of the header
        call ftmbyt(iunit,hdstrt(ibuff,chdu(ibuff)),.true.,status)

C       calculate number of FITS blocks in the header
        nrec=(dtstrt(ibuff)-hdstrt(ibuff,chdu(ibuff)))/2880

C       accumulate the header into the checksum
        chksum=datsum
        call ftcsum(iunit,nrec,chksum,status)
        end
