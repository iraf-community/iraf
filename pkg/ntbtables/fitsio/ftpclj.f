C----------------------------------------------------------------------
        subroutine ftpclj(ounit,colnum,frow,felem,nelem,array,status)

C       write an array of integer data values to the specified column of
C       the table.  

C       ounit   i  fortran unit number
C       colnum  i  number of the column to write to
C       frow    i  first row to write
C       felem   i  first element within the row to write
C       nelem   i  number of elements to write
C       array   i  array of data values to be written 
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,frow,felem,nelem,status
        integer array(*)

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
        character cnull*16, cform*8
        common/ft0003/cnull(nf),cform(nf)
        character*1 chbuff(400),xdummy(5360)
        common/ftheap/chbuff,xdummy
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bufdim
        parameter (bufdim = 100)
        integer buffer(bufdim),bytpix,bstart,tcode,incre
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart,maxpix,ival
        real rval
        double precision scale,zero,dval,align
        character sval*40,wform*10,crow*9,cp1*9,cp2*9,ccol*4
        logical tofits,lval,descrp
        integer*2 i2val
        character*1 i1val
C       the following equivalence is required for the HP/UX PA-RISC complier
C       to force the buffer to be double word aligned.
        equivalence (align,buffer(1))

        if (status .gt. 0)return

C       check for zero length array or bad first row number
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                write(crow,2000)frow
2000            format(i9)
                call ftpmsg('Starting row number for table write '//
     &          'request is out of range:'//crow//' (FTPCLJ).')
                return
        end if

        ibuff=bufnum(ounit)

C       if HDU structure is not defined then scan the header keywords
        if (dtstrt(ibuff) .lt. 0)call ftrdef(ounit,status)

        descrp=.false.
        i1=1
        ntodo=nelem
        rstart=frow-1
        scale=tscale(colnum+tstart(ibuff))
        zero=tzero(colnum+tstart(ibuff))
        tcode=tdtype(colnum+tstart(ibuff)) 
C       the data are being scaled from internal format to FITS:
        tofits=.true.

C       calculate the maximum number of column pixels which fit in buffer
        bytpix=max(abs(tcode)/10,1)
        maxpix=bufdim/bytpix*4

C       incre is the byte offset between consecutive pixels
        incre=0
        if (tcode .eq. 16)then
C               this is an ASCII table; table elements cannot be vectors
                repeat=1
                estart=0
        else
C               this is a binary table
                if (felem .lt. 1)then
C                       illegal element number
                        status=308
                        write(crow,2000)felem
                    call ftpmsg('Starting element number for write '//
     &                 'request is out of range:'//crow//' (FTPCLJ).')
                        return
                else
                        estart=felem-1
                end if

                if (tcode .gt. 0)then
                        if (hdutyp(ibuff) .eq. 0)then
C                           if this is a primary array or image extension, then
C                           set repeat as large as needed to write all
C                           the pixels.  This prevents an error message if
C                           array size is not yet known.  The actual array
C                           dimension must be defined by the NAXISn keywords
C                           before closing this HDU. 
                            repeat=estart+nelem
                        else
                            repeat=trept(colnum+tstart(ibuff))
                        end if

                        if (felem .gt. repeat)then
C                               illegal element number
                                status=308
                                write(crow,2000)felem
                    call ftpmsg('Starting element number for write '//
     &                 'request is out of range:'//crow//' (FTPCLJ).')
                                return
                        end if
                        if (repeat .eq. 1 .and. nelem .gt. 1)then
C                               write multiple rows of data at one time
                                incre=rowlen(ibuff)
                                repeat=maxpix
                                estart=0
                        end if
                else
C                       this is a variable length descriptor column
                        descrp=.true.
                        tcode=-tcode
                        repeat=nelem+felem-1
C                       write the number of elements and the starting offset:
                        call ftpdes(ounit,colnum,frow,repeat,
     &                              nxheap(ibuff),status)
C                       move the i/o pointer to the start of the pixel sequence
                        bstart=dtstrt(ibuff)+nxheap(ibuff)+
     &                          theap(ibuff)+estart*bytpix
                        call ftmbyt(ounit,bstart,.true.,status)
C                       increment the empty heap starting address:
                        nxheap(ibuff)=nxheap(ibuff)+repeat*bytpix
                end if
        end if

C       process as many contiguous pixels as possible, up to buffer size
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)
     &             +tbcol(colnum+tstart(ibuff))+estart*bytpix
            call ftmbyt(ounit,bstart,.true.,status)
        end if

C       copy data to buffer, doing scaling and datatype conversion, if required
        if (tcode .eq. 21)then
C               column data type is I (I*2)
                call fti4i2(array(i1),itodo,scale,zero,tofits,
     &          ival,ival,i2val,lval,lval,buffer,status)
C               do any machine dependent data conversion and write the I*2 data
                call ftpi2b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 41)then
C               column data type is J (I*4)
                call fti4i4(array(i1),itodo,scale,zero,tofits,
     &              ival,ival,ival,lval,lval,buffer,status)
C               do any machine dependent conversion and write the I*4 data
                call ftpi4b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 42)then
C               column data type is E (R*4)
                call fti4r4(array(i1),itodo,scale,zero,tofits,
     &          ival,ival,rval,lval,lval,buffer,status)
C               do any machine dependent data conversion and write the R*4 data
                call ftpr4b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 82)then
C               column data type is D (R*8)
                call fti4r8(array(i1),itodo,scale,zero,tofits,
     &          ival,ival,dval,lval,lval,buffer,status)
C               do any machine dependent data conversion and write the R*8 data
                call ftpr8b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 11)then
C               column data type is B (byte)
                call fti4i1(array(i1),itodo,scale,zero,tofits,
     &          ival,ival,i1val,lval,lval,chbuff,status)
C               do any machine dependent data conversion and write the byte data
                call ftpi1b(ounit,itodo,incre,chbuff,status)
        else if (tcode .eq. 16 .and. hdutyp(ibuff) .eq. 1)then
C               this is an ASCII table column
                wform='(        )'
                wform(2:9)=cform(colnum+tstart(ibuff))
                if (cform(colnum+tstart(ibuff))(1:1) .eq. 'I')then
C                 column data type is integer
                  call fti4i4(array(i1),itodo,scale,zero,tofits,
     &            ival,ival,ival,lval,lval,ival,status)
C                 create the formated character string
                  write(sval,wform,err=900)ival
C                 write the character string to the FITS file
                  call ftpcbf(ounit,1,tnull(colnum+tstart(ibuff)),sval,
     &                        status)
                else if (cform(colnum+tstart(ibuff))(1:1) .eq. 'F'
     &            .or.  cform(colnum+tstart(ibuff))(1:1) .eq. 'E')then
C                 column data type is real
                  call fti4r4(array(i1),itodo,scale,zero,tofits,
     &            ival,ival,rval,lval,lval,rval,status)
C                 create the formated character string
                  write(sval,wform,err=900)rval
C                 write the character string to the FITS file
                  call ftpcbf(ounit,1,tnull(colnum+tstart(ibuff)),sval,
     &                        status)
                else if (cform(colnum+tstart(ibuff))(1:1) .eq. 'D')then
C                 column data type is double precision
                  call fti4r8(array(i1),itodo,scale,zero,tofits,
     &            ival,ival,dval,lval,lval,dval,status)
C                 create the formated character string
                  write(sval,wform,err=900)dval
C                 write the character string to the FITS file
                  call ftpcbf(ounit,1,tnull(colnum+tstart(ibuff)),sval,
     &                        status)
                else
C                 error: illegal ASCII table format code
                  status=311
                  write(ccol,2001)colnum
        call ftpmsg('Cannot write Integer*4 values to column'//ccol
     &   //' with TFORM = '//cform(colnum+tstart(ibuff))//' (FTPCLJ).')
                  return
                end if
        else
C               error illegal binary table data type code
                status=312
                write(ccol,2001)colnum
        call ftpmsg('Cannot write Integer*4 values to column'//ccol
     &   //' with TFORM = '//cform(colnum+tstart(ibuff))//' (FTPCLJ).')
                return
        end if

        if (status .gt. 0)then
          if (hdutyp(ibuff) .eq. 0)then
C            this is a primary array or image extension
             write(cp1,2000)felem+i1-1
             write(cp2,2000)felem+i1+itodo-2
             call ftpmsg('Error writing pixels'//cp1//' to'//cp2
     &                 // ' to the FITS image array (FTPCLJ).')
             if (frow .ne. 1)then
                write(cp1,2000)frow
                call ftpmsg('Error while writing group'//cp1//
     &          ' of the multigroup primary array.')
             end if
          else
             write(ccol,2001)colnum
2001         format(i4)
             if (descrp)then
C              this is a variable length descriptor column
               write(crow,2000)frow
               write(cp1,2000)felem+i1-1
               write(cp2,2000)felem+i1+itodo-2
               call ftpmsg('Error writing elements'//cp1//' to'//cp2
     &         //' in row'//crow)
               call ftpmsg(' of variable length vector column'//ccol
     &                    //' (FTPCLJ.')
             else if (trept(colnum+tstart(ibuff)) .eq. 1)then
C              this is not a vector column (simple case)
               write(cp1,2000)frow+i1-1
               write(cp2,2000)frow+i1+itodo-2
               call ftpmsg('Error writing rows'//cp1//' to'//cp2
     &           //' of column'//ccol//' (FTPCLJ).')
             else
C              this is a vector column (more complicated case)
               write(crow,2000)rstart+1
               write(cp1,2000)estart+1
               write(cp2,2000)itodo
               call ftpmsg('Error writing'//cp2//' elements to'
     &         //' column'//ccol)
               call ftpmsg(' starting at row'//crow
     &         //', element'//cp1//' (FTPCLJ).')
             end if
          end if
          return
        end if

C       find number of pixels left to do, and quit if none left
        ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        if (incre .eq. 0)then
                                rstart=rstart+1
                        else
                                rstart=rstart+repeat
                        end if
                end if
                go to 20
        end if

C       check for any overflows
        if (status .eq. -11)then
                status=412
                call ftpmsg('Numeric overflow error occurred writing '//
     &        'Integer*4 data to FITS file.')
        end if
        return

900     continue
C       error writing formatted data value to ASCII table
        write(ccol,2001)colnum
        write(cp1,2000)rstart+1
        call ftpmsg('Error writing colunm'//ccol//', row'//cp1//
     &  ' of the ASCII Table.')
        call ftpmsg('Tried to write value'//
     &              '" with format '//wform//' (FTPCLJ).')
        status=313
        end
