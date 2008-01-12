C--------------------------------------------------------------------------
        subroutine ftdrow(iunit,frow,nrows,status)

C       delete NROWS rows from a table, beginning with row FROW

C       iunit   i  Fortran I/O unit number
C       frow    i  row number after which the new rows will be inserted.
C                  Specify  0 to add rows to the beginning of the table.
C       nrows   i  number of rows to add to the table (must be greater than 0)
C       status  i  returned error status (0=ok)

        integer iunit,frow,nrows,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 20)
        parameter (nf = 3000)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
        integer tfield,tstart,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tstart(nb),tbcol(nf),rowlen(nb),
     &  tdtype(nf),trept(nf),tscale(nf),tzero(nf),tnull(nf),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,naxis1,naxis2,size,freesp,nblock,row
        character comm*8

        if (status .gt. 0)return

C       define the number of the buffer used for this file
        ibuff=bufnum(iunit)

C       test that the CHDU is an ASCII table or BINTABLE
        if (hdutyp(ibuff) .ne. 1 .and. hdutyp(ibuff) .ne. 2)then
                status=235
                call ftpmsg('Can only delete rows from TABLE or '//
     &          'BINTABLE extension (FTDROW)')
                return
        end if

C       get current size of the table
        call ftgkyj(iunit,'NAXIS1',naxis1,comm,status)
        call ftgkyj(iunit,'NAXIS2',naxis2,comm,status)

        if (nrows .lt. 0)then
                 status=306
                 call ftpmsg('Cannot delete negative number of ' //
     &           'rows in the table (FTDROW)')
                 return
        else if (frow+nrows-1 .gt. naxis2)then
                 status=307
                 call ftpmsg('Specified number of rows to delete '
     &           //'exceeds number of rows in table (FTDROW)')
                 return
        else if (nrows .eq. 0)then
                 return
        else if (frow .gt. naxis2)then
                status=307
                call ftpmsg('First row to delete is greater'//
     &            ' than the number of rows in the table (FTDROW)') 
                return
        else if (frow .le. 0)then
                status=307
                call ftpmsg('Delete starting row number is less '
     &          //'than 1 (FTDROW)')
                return
        end if
        
C       Calculate how many FITS blocks (2880 bytes) need to be deleted
        size=theap(ibuff)+scount(ibuff)
        freesp=((size+2879)/2880)*2880 - size + naxis1*nrows
        nblock=freesp/2880

C       shift the rows up
        row=frow+nrows
        call ftrwup(iunit,row,naxis2,nrows,status)

C       shift the heap up
        size=naxis1*nrows
        call fthpup(iunit,size,status)

        if (nblock .gt. 0)call ftdblk(iunit,nblock,1,status)

C       update the NAXIS2 keyword
        naxis2=naxis2-nrows
        call ftmkyj(iunit,'NAXIS2',naxis2,'&',status)
        end
