C----------------------------------------------------------------------
        subroutine ftgclb(iunit,colnum,frow,felem,nelem,eincr,
     &   nultyp,nulval,array,flgval,anynul,status)

C       read an array of byte data values from the specified column of
C       the table.  
C       This general purpose routine will handle null values in one
C       of two ways: if nultyp=1, then undefined array elements will be
C       set equal to the input value of NULVAL.  Else if nultyp=2, then
C       undefined array elements will have the corresponding FLGVAL element
C       set equal to .TRUE.  If NULTYP=1 and NULVAL=0, then no checks for
C       undefined values will be made, for maximum efficiency.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read from
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       eincr   i  element increment
C       nultyp  i  input code indicating how to handle undefined values
C       nulval  b  value that undefined pixels will be set to (if nultyp=1)
C       array   b  array of data values that are read from the FITS file
C       flgval  l  set .true. if corresponding element undefined (if nultyp=2)
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,eincr,nultyp,status
        character*1 array(*),nulval
        logical flgval(*),anynul

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bufdim
        parameter (bufdim = 100)
        integer buffer(bufdim),bytpix,bstart,tcode,i4null,nulchk,incre
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart,maxpix,ival
        integer offset,rskip,dstart,begcol,lenrow
        integer*2 i2null
        character*1 i1null
        real rval
        double precision scale,zero,dval
        character sval*40,sform*13,snull*16
        logical tofits,descrp,trans
        character crow*9,cp1*9,cp2*9,ccol*4
        
        if (status .gt. 0)return

C       check for zero length array or bad first row number
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                write(crow,2000)frow
2000            format(i9)
                call ftpmsg('Starting row number for table read '//
     &          'request is out of range:'//crow//' (FTGCLB).')
                return
        end if

        descrp=.false.
        i1=1
        ntodo=nelem
        rstart=frow-1
        anynul=.false.
        ibuff=bufnum(iunit)
        dstart=dtstrt(ibuff)
        lenrow=rowlen(ibuff)
        begcol=tbcol(colnum+tstart(ibuff))
        tcode=tdtype(colnum+tstart(ibuff))
        scale=tscale(colnum+tstart(ibuff))
        zero=tzero(colnum+tstart(ibuff))
C       the data are being scaled from FITS to internal format 
        tofits=.false.

C       calculate the maximum number of column pixels which fit in buffer
        bytpix=max(abs(tcode)/10,1)
C       check for important special case: no datatype conversion required
        if (abs(tcode) .eq. 11)then
                maxpix=nelem
        else
                maxpix=bufdim/bytpix*4
        end if

C       determine the repeat count and the first element position
C       incre is the byte offset between consecutive pixels
        incre=bytpix*eincr               
        if (tcode .eq. 16)then
C           this is an ASCII table; each element will be read one at a time
                repeat=1
                estart=0
C               construct the read format, and get the null value string
C               Microsoft Fortran 5.0 can't handle:
C               sform='(BN,'//cform(colnum+tstart(ibuff))//')'
                sform='(BN,        )'
                sform(5:12)=cform(colnum+tstart(ibuff))
                snull=cnull(colnum+tstart(ibuff))                
                sval=' '
        else
C           this is a binary table
                if (felem .lt. 1)then
C                       illegal element number
                        status=308
                        write(crow,2000)felem
                call ftpmsg('Starting element number for read '//
     &          'request is out of range:'//crow//' (FTGCLB).')
                        return
                 end if
                 estart=felem-1

                if (tcode .gt. 0)then
                        repeat=trept(colnum+tstart(ibuff))
                        if (felem .gt. repeat)then
C                               illegal element number
                                status=308
                                write(crow,2000)felem
                call ftpmsg('Starting element number for read '//
     &          'request is out of range:'//crow//' (FTGCLB).')
                                return
                        end if
                        if (repeat .eq. 1 .and. nelem .gt. 1)then
C                           read multiple rows of data at one time by
C                           fooling it into thinking that this is a vector
C                           column with a large value of bytes per pixel
                                dstart=dstart+rstart*lenrow
                                rstart=0
                                estart=0
                                repeat=maxpix*eincr
                                incre=lenrow*eincr 
                                lenrow=lenrow*repeat
                        end if
                else
C                       this is a variable length descriptor column
                        descrp=.true.
                        tcode=-tcode
C                       read the number of elements and the starting offset:
                        call ftgdes(iunit,colnum,frow,repeat,
     &                              offset,status)
                        if (repeat .eq. 0)then
C                               error: null length vector
                                 status=318
                                return
                        else if (estart+(nelem-1)*eincr+1 .gt. repeat)
     %                      then
C                               error: trying to read beyond end of record
                                status=319
                                return
                        end if
C                       define the starting point of the row
                        dstart=dstart+offset+theap(ibuff)
                        rstart=0
                        begcol=0
                end if
        end if

C       determine if we have to check for null values
        if (nultyp .eq. 1 .and. ichar(nulval) .eq. 0)then
C               user doesn't want to check for nulls
                nulchk=0
        else
C           user does want to check for null values
C           see if the null value has been defined for this column
            nulchk=0
            if (tcode .eq. 11)then
C               check if byte datatype null value is defined, 
                if (tnull(colnum+tstart(ibuff)).ne.123454321)then
                        i1null=char(tnull(colnum+tstart(ibuff)))
                        nulchk=nultyp
                end if
            else if (tcode .eq. 21)then
C               check if I*2 datatype null value is defined, 
                if (tnull(colnum+tstart(ibuff)).ne.123454321)then
                        i2null=tnull(colnum+tstart(ibuff))
                        nulchk=nultyp
                end if
            else if (tcode .eq. 41)then
C               check if I*4 datatype null value is defined, 
                if (tnull(colnum+tstart(ibuff)).ne.123454321)then
                        i4null=tnull(colnum+tstart(ibuff))
                        nulchk=nultyp
                end if
            else if (tcode .eq. 42 .or. tcode .eq. 82)then
C               have to check floating point data for NaN values
                nulchk=nultyp
            end if
        end if

        if (nulchk .eq. 0 .and. scale .eq. 1. .and. zero .eq. 0.)then
                trans=.false.
        else
                trans=.true.
        end if

C       process as many contiguous pixels as possible, up to buffer size
20      itodo=min(ntodo,(repeat-estart-1)/eincr+1,maxpix)

C       move the i/o pointer to the start of the sequence of pixels
        bstart=dstart+rstart*lenrow+begcol+estart*bytpix
        call ftmbyt(iunit,bstart,.false.,status)

C       read the data from FITS file, doing datatype conversion and scaling
        if (tcode .eq. 21)then
C               column data type is I (I*2)
C               read the data and do any machine dependent data conversion
                call ftgi2b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call fti2i1(buffer,itodo,scale,zero,tofits,
     &          nulchk,i2null,nulval,flgval(i1),anynul,array(i1),status)
        else if (tcode .eq. 41)then
C               column data type is J (I*4)
C               read the data and do any machine dependent data conversion
                call ftgi4b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call fti4i1(buffer,itodo,scale,zero,tofits,
     &          nulchk,i4null,nulval,flgval(i1),anynul,array(i1),status)
        else if (tcode .eq. 42)then
C               column data type is E (R*4)
C               read the data and do any machine dependent data conversion
                call ftgr4b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call ftr4i1(buffer,itodo,scale,zero,tofits,
     &          nulchk,nulval,flgval(i1),anynul,array(i1),status)
        else if (tcode .eq. 82)then
C               column data type is D (R*8)
C               read the data and do any machine dependent data conversion
                call ftgr8b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call ftr8i1(buffer,itodo,scale,zero,tofits,
     &          nulchk,nulval,flgval(i1),anynul,array(i1),status)
        else if (tcode .eq. 11)then
C               column data type is B (byte)
C               read the data and do any machine dependent data conversion
C               note that we can use the input array directly
                call ftgi1b(iunit,itodo,incre,array(i1),status)
C               check for null values, and do scaling and datatype conversion
                if (trans)then
                  call fti1i1(array(i1),itodo,scale,zero,tofits,nulchk,
     &            i1null,nulval,flgval(i1),anynul,array(i1),status)
                end if
        else if (tcode .eq. 16 .and. hdutyp(ibuff) .eq. 1)then
C               this is an ASCII table column; get the character string
                call ftgcbf(iunit,1,tnull(colnum+tstart(ibuff)),sval,
     &                      status)
                if (status .gt. 0)return

C               check for null
                if (sval(1:16) .eq. snull)then
                        anynul=.true.
                        if (nultyp .eq. 1)then
                                array(i1)=nulval
                        else
                                flgval(i1)=.true.
                        end if
                        go to 30
                end if

C               now read the value, then do scaling and datatype conversion
                if (sform(5:5) .eq. 'I')then
                        read(sval,sform,err=900)ival
                        call fti4i1(ival,itodo,scale,zero,tofits,0,
     &                 i4null,nulval,flgval(i1),anynul,array(i1),status)     
                else if (sform(5:5).eq.'F'.or. sform(5:5).eq.'E')then
                        read(sval,sform,err=900)rval
                        call ftr4i1(rval,itodo,scale,zero,tofits,
     &                  0,nulval,flgval(i1),anynul,array(i1),status)
                else if (sform(5:5) .eq. 'D')then
                        read(sval,sform,err=900)dval
                        call ftr8i1(dval,itodo,scale,zero,tofits,
     &                  0,nulval,flgval(i1),anynul,array(i1),status)
                else
C                       error: illegal ASCII table format code
                        status=311
                        write(ccol,2001)colnum
        call ftpmsg('Cannot read byte (I*1) values from column'//ccol
     &   //' with TFORM = '//cform(colnum+tstart(ibuff))//' (FTGCLB).')
                        return
                end if
        else
C               error illegal binary table data type code
                status=312
                write(ccol,2001)colnum
        call ftpmsg('Cannot read byte (I*1) values from column'//ccol
     &   //' with TFORM = '//cform(colnum+tstart(ibuff))//' (FTGCLB).')
                return
        end if

C       find number of pixels left to do, and quit if none left
30      ntodo=ntodo-itodo

        if (status .gt. 0)then
          if (hdutyp(ibuff) .eq. 0)then
C            this is a primary array or image extension
             write(cp1,2000)felem+i1-1
             write(cp2,2000)felem+i1+itodo-2
             call ftpmsg('Error reading pixels'//cp1//' to'//cp2
     &                 // ' of the FITS image array (FTGCLB).')
             if (frow .ne. 1)then
                write(cp1,2000)frow
                call ftpmsg('Error while reading group'//cp1//
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
               call ftpmsg('Error reading elements'//cp1//' to'//cp2
     &         //' in row'//crow)
               call ftpmsg(' of variable length vector column'//ccol
     &                    //' (FTGCLB.')
             else if (trept(colnum+tstart(ibuff)) .eq. 1)then
C              this is not a vector column (simple case)
               write(cp1,2000)frow+i1-1
               write(cp2,2000)frow+i1+itodo-2
               call ftpmsg('Error reading rows'//cp1//' to'//cp2
     &           //' of column'//ccol//' (FTGCLB).')
             else
C              this is a vector column (more complicated case)
               write(crow,2000)rstart+1
               write(cp1,2000)estart+1
               write(cp2,2000)itodo
               call ftpmsg('Error reading'//cp2//' elements from'
     &         //' column'//ccol)
               call ftpmsg(' starting at row'//crow
     &         //', element'//cp1//' (FTGCLB).')
             end if
          end if
          return
        end if

        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo
                estart=estart+itodo*eincr
                rskip=estart/repeat
                rstart=rstart+rskip
                estart=estart-rskip*repeat
                go to 20
        end if

C       check for any overflows
        if (status .eq. -11)then
               status=412
               call ftpmsg('Numeric overflow error occurred reading '//
     &        'Byte data from FITS file.')
        end if
        return

900     continue
C       error reading formatted data value from ASCII table
        write(ccol,2001)colnum
        write(cp1,2000)rstart+1
        call ftpmsg('Error reading colunm'//ccol//', row'//cp1//
     &  ' of the ASCII Table.')
        call ftpmsg('Tried to read "'//sval(1:20)//
     &              '" with format '//sform//' (FTGCLB).')
        status=315
        end
