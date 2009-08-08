C----------------------------------------------------------------------
        subroutine ftpcne(ounit,colnum,frow,felem,nelem,array,nulval,
     &                    status)

C       write array of floating point pixels to the specified column
C       of a table.  Any input pixels equal to the value of NULVAL will
C       be replaced by the appropriate null value in the output FITS file. 

C       ounit   i  fortran unit number
C       colnum  i  number of the column to write to
C       frow    i  first row to write
C       felem   i  first element within the row to write
C       nelem   i  number of elements to write
C       array   r  array of data values to be written 
C       nulval  r  pixel value used to represent an undefine pixel
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1994

        integer ounit,colnum,frow,felem,nelem,status
        real array(*),nulval

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

        integer ibuff,repeat,first,ngood,nbad,i,fstelm,fstrow

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

C       get the column repeat count and calculate the absolute position within
C       the column of the first element to be written
        repeat=trept(colnum+tstart(ibuff))
        first=(frow-1)*repeat+felem-1

        ngood=0
        nbad=0
        do 10 i=1,nelem
            if (array(i) .ne. nulval)then
                ngood=ngood+1
                if (nbad .gt. 0)then
C                   write the previous consecutive set of null pixels
                    fstelm=i-nbad+first
C                   calculate the row and element of the first pixel to write
                    fstrow=(fstelm-1)/repeat+1
                    fstelm=fstelm-(fstrow-1)*repeat
                    call ftpclu(ounit,colnum,fstrow,fstelm,nbad,status)
                    nbad=0
                end if
            else
                nbad=nbad+1
                if (ngood .gt. 0)then
C                   write the previous consecutive set of good pixels
                    fstelm=i-ngood+first
C                   calculate the row and element of the first pixel to write
                    fstrow=(fstelm-1)/repeat+1
                    fstelm=fstelm-(fstrow-1)*repeat
                    call ftpcle(ounit,colnum,fstrow,fstelm,ngood,
     &                          array(i-ngood),status)
                    ngood=0
                end if
            end if
10      continue

C       finished;  now just write the last set of pixels
        if (nbad .gt. 0)then
C           write the consecutive set of null pixels
            fstelm=i-nbad+first
            fstrow=(fstelm-1)/repeat+1
            fstelm=fstelm-(fstrow-1)*repeat
            call ftpclu(ounit,colnum,fstrow,fstelm,nbad,status)
        else
C           write the consecutive set of good pixels
            fstelm=i-ngood+first
            fstrow=(fstelm-1)/repeat+1
            fstelm=fstelm-(fstrow-1)*repeat
            call ftpcle(ounit,colnum,fstrow,fstelm,ngood,
     &                  array(i-ngood),status)
        end if
        end
