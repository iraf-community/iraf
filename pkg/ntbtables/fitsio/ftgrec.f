C--------------------------------------------------------------------------
        subroutine ftgrec(iunit,nrec,record,status)

C       Read the Nth 80-byte header record 
C       This routine is useful for reading the entire header, one
C       record at a time.

C       iunit   i  Fortran I/O unit number
C       nrec    i  sequence number (starting with 1) of the record to read
C       OUTPUT PARAMETERS:
C       record  c  output 80-byte record
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,nrec,status
        character*80 record

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 20)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
C-------END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,nbyte,endhd
        character arec*8

        if (status .gt. 0)return

C       get the number of the data buffer used for this unit
        ibuff=bufnum(iunit)

C       calculate byte location of the record, and check if it is legal
        nbyte=hdstrt(ibuff,chdu(ibuff))+(nrec-1)*80

C       endhd=(hdend(ibuff)/2880+1)*2880
C       modified this on 4 Nov 1994 to allow for blanks before the END keyword
        endhd=max(hdend(ibuff),dtstrt(ibuff)-2880)

        if (nbyte .gt. endhd .or. nrec .le. 0)then
C               header record number is out of bounds
                status=203
                write(arec,1000)nrec
1000            format(i8)
                call ftpmsg('Cannot get Keyword number '//arec//'.'//
     &          '  It does not exist.')
                go to 100
        end if 

C       position the I/O pointer to the appropriate header keyword
        call ftmbyt(iunit,nbyte,.false.,status)

C       read the 80 byte record
        call ftgcbf(iunit,1,80,record,status)
        if (status .gt. 0)then
                write(arec,1000)nrec
                call ftpmsg('FTGREC could not read header keyword'//
     &            ' number '//arec//'.')
                return
        end if

C       update the keyword pointer position
        nxthdr(ibuff)=nbyte+80

100     continue
        end
