C--------------------------------------------------------------------------
        subroutine fticol(iunit,numcol,ttype,tform,status)

C       insert a new column into an existing table

C       iunit   i  Fortran I/O unit number
C       numcol  i  number (position) for the new column; 1 = first column
C                  any existing columns will be moved up one position
C       ttype   c  name of column (value for TTYPEn keyword)
C       tform   c  column format (value for TFORMn keyword)
C       status  i  returned error status (0=ok)

        integer iunit,numcol,status
        character*(*) ttype,tform

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

        integer ibuff,colnum,typhdu,datcod,repeat,width,decims,delbyt
        integer naxis1,naxis2,size,freesp,nblock,tflds,tbc,fstbyt,i
        character comm*70,tfm*30,keynam*8

        if (status .gt. 0)return

C       define the number of the buffer used for this file
        ibuff=bufnum(iunit)

C       test that the CHDU is an ASCII table or BINTABLE
        typhdu=hdutyp(ibuff)
        if (typhdu .ne. 1 .and. typhdu .ne. 2)then
                status=235
                call ftpmsg('Can only append column to TABLE or '//
     &          'BINTABLE extension (FTICOL)')
                return
        end if

C       check that the column number is valid
        tflds=tfield(ibuff)
        if (numcol .lt. 1)then
            status=302
            return
        else if (numcol .gt. tflds)then
            colnum=tflds+1
        else
            colnum=numcol
        end if

C       parse the tform value and calc number of bytes to add to each row
C       make sure format characters are in upper case:
        tfm=tform
        call ftupch(tfm)

        if (typhdu .eq. 1)then
            call ftasfm(tfm,datcod,width,decims,status)
C           add one space between the columns
            delbyt=width+1
        else
            call ftbnfm(tfm,datcod,repeat,width,status)
            if (datcod .eq. 1)then
C               bit column; round up to a multiple of 8 bits
                delbyt=(repeat+7)/8
            else if (datcod .eq. 16)then
C               ASCII string column
                delbyt=repeat
            else
C               numerical data type
                delbyt=(datcod/10)*repeat
            end if
        end if

C       quit on error, or if column is zero byte wide (repeat=0)
        if (status .gt. 0 .or. delbyt .eq. 0)return

C       get current size of the table
        naxis1=rowlen(ibuff)
        call ftgkyj(iunit,'NAXIS2',naxis2,comm,status)

C       Calculate how many more FITS blocks (2880 bytes) need to be added
        size=theap(ibuff)+scount(ibuff)
        freesp=(delbyt*naxis2) - ((size+2879)/2880)*2880 + size
        nblock=(freesp+2879)/2880

C       insert the needed number of new FITS blocks at the end of the HDU
        if (nblock .gt. 0)call ftiblk(iunit,nblock,1,status)

C       shift the heap down, and update pointers to start of heap
        size=delbyt*naxis2
        call fthpdn(iunit,size,status)

C       calculate byte position in the row where to insert the new column
        if (colnum .gt. tflds)then
            fstbyt=naxis1
        else
            fstbyt=tbcol(colnum+tstart(ibuff))
        end if

C       insert DELBYT bytes in every row, at byte position FSTBYT
        call ftcins(iunit,naxis1,naxis2,delbyt,fstbyt,status)

        if (typhdu .eq. 1)then
C           adjust the TBCOL values of the existing columns
            do 10 i=1,tflds
                call ftkeyn('TBCOL',i,keynam,status)
                call ftgkyj(iunit,keynam,tbc,comm,status)
                if (tbc .gt. fstbyt)then
                     tbc=tbc+delbyt
                     call ftmkyj(iunit,keynam,tbc,'&',status)
                end if
10          continue
        end if

C       update the mandatory keywords
        call ftmkyj(iunit,'TFIELDS',tflds+1,'&',status)
        call ftmkyj(iunit,'NAXIS1',naxis1+delbyt,'&',status)

C       increment the index value on any existing column keywords
        call ftkshf(iunit,colnum,tflds,1,status)

C       add the required keywords for the new column
        comm='label for field'
        call ftpkns(iunit,'TTYPE',colnum,1,ttype,comm,status)

        comm='format of field'
        call ftpkns(iunit,'TFORM',colnum,1,tfm,comm,status)

        if (typhdu .eq. 1)then
            comm='beginning column of field '
            if (colnum .eq. tflds+1)then
C               allow for the space between preceding column
                tbc=fstbyt+2
            else
                tbc=fstbyt+1
            end if
            call ftpknj(iunit,'TBCOL',colnum,1,tbc,comm,status)
        end if

C       parse the header to initialize the new table structure
        call ftrdef(iunit,status)
        end
