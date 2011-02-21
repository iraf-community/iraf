C--------------------------------------------------------------------------
        subroutine ftgclm(iunit,colnum,frow,felem,nelem,eincr,
     &   nultyp,nulval,array,flgval,anynul,status)

C       read an array of double complex data values from the specified 
C       column of the table.  
C       This general purpose routine will handle null values in one
C       of two ways: if nultyp=1, then undefined array elements will be
C       set equal to the input value of NULVAL.  Else if nultyp=2, then
C       undefined array elements will have the corresponding FLGVAL element
C       set equal to .TRUE.  If NULTYP=1 and NULVAL=0, then no checks for
C       undefined values will be made, for maximum efficiency.
C       The binary table column being read to must have datatype 'M'
C       and no datatype conversion will be perform if it is not.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read from
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of (pairs) elements to read
C       eincr   i  element increment
C       nultyp  i  input code indicating how to handle undefined values
C       nulval  d  value that undefined pixels will be set to (if nultyp=1)
C       array   d  array of data values that are read from the FITS file
C       flgval  l  set .true. if corresponding element undefined (if nultyp=2)
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,eincr,nultyp,status
        double precision array(*),nulval(2)
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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bytpix,bstart,tcode,nulchk,incre
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart
        integer offset,rskip,dstart,begcol,lenrow,i,j
        logical scaled,descrp
        double precision scale,zero
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
     &          'request is out of range:'//crow//' (FTGCLM).')
                return
        end if
        if (felem .lt. 1)then
C               illegal element number
                status=308
                write(crow,2000)felem
                call ftpmsg('Starting element number for read '//
     &          'request is out of range:'//crow//' (FTGCLM).')
                return
        end if

        i1=1
        ntodo=nelem
        estart=felem-1
        rstart=frow-1
        anynul=.false.
        ibuff=bufnum(iunit)
        dstart=dtstrt(ibuff)
        lenrow=rowlen(ibuff)
        begcol=tbcol(colnum+tstart(ibuff))
        tcode=tdtype(colnum+tstart(ibuff))
        scale=tscale(colnum+tstart(ibuff))
        zero=tzero(colnum+tstart(ibuff))
        bytpix=16

C       determine the repeat count and the first element position
C       incre is the byte offset between consecutive pixels
        incre=bytpix*eincr               

        if (tcode .eq. 163)then
                descrp=.false.
                repeat=trept(colnum+tstart(ibuff))
                if (felem .gt. repeat)then
C                        illegal element number
                         status=308
                         write(crow,2000)felem
                call ftpmsg('Starting element number for read '//
     &          'request is out of range:'//crow//' (FTGCLM).')
                         return
                end if
                if (repeat .eq. 1 .and. nelem .gt. 1)then
C                        read multiple rows of data at one time by
C                        fooling it into thinking that this is a vector
C                        column with a large value of bytes per pixel
                                dstart=dstart+rstart*lenrow
                                rstart=0
                                estart=0
                                repeat=nelem*eincr
                                incre=lenrow*eincr 
                                lenrow=lenrow*repeat
                end if
        else if (tcode .eq. -163)then
C               this is a variable length descriptor column
C               read the number of elements and the starting offset:
                descrp=.true.
                call ftgdes(iunit,colnum,frow,repeat,offset,status)
                if (repeat .eq. 0)then
C                               error: null length vector
                                 status=318
                                return
                else if (estart+(nelem-1)*eincr+1 .gt. repeat) then
C                               error: trying to read beyond end of record
                                status=319
                                return
                end if
C               define the starting point of the row
                dstart=dstart+offset+theap(ibuff)
                rstart=0
                begcol=0
        else
C               column must be double complex data type
                status=312
                write(ccol,2001)colnum
                call ftpmsg('Column'//ccol//' does not have '//
     &          'Double Precision Complex (M) data type (FTGCLM).')
                return
        end if

C       determine if we have to check for null values
        if (nultyp .eq. 1 .and. nulval(1) .eq. 0 .and.
     &      nulval(2) .eq. 0)then
C               user doesn't want to check for nulls
                nulchk=0
        else
C           user does want to check for null values
            nulchk=nultyp
        end if

C       check if scaling is required
        if (scale .eq. 1.0 .and. zero .eq. 0.)then
                scaled=.false.
        else
                scaled=.true.
        end if

C       process as many contiguous pixels as possible, up to buffer size
20      itodo=min(ntodo,(repeat-estart-1)/eincr+1)

C       move the i/o pointer to the start of the sequence of pixels
        bstart=dstart+rstart*lenrow+begcol+estart*bytpix
        call ftmbyt(iunit,bstart,.false.,status)

C       read the data 
        if (incre .eq. 16)then
C               the data values are contiguous in the FITS file
C               multiply itodo*2 because we are getting pairs of values
                call ftgr8b(iunit,itodo*2,8,array(i1),status)
        else
C               have to read each complex double pair one by one
                j=i1
                call ftgr8b(iunit,2,8,array(j),status)
                j=j+2
                do 25 i=2,itodo
                    call ftmoff(iunit,incre-16,.false.,status)
                    call ftgr8b(iunit,2,8,array(j),status)
                    j=j+2
25              continue
        end if

C       find number of pixels left to do, and process them
30      ntodo=ntodo-itodo

        if (status .gt. 0)then
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
     &                    //' (FTGCLM.')
             else if (trept(colnum+tstart(ibuff)) .eq. 1)then
C              this is not a vector column (simple case)
               write(cp1,2000)frow+i1-1
               write(cp2,2000)frow+i1+itodo-2
               call ftpmsg('Error reading rows'//cp1//' to'//cp2
     &           //' of column'//ccol//' (FTGCLM).')
             else
C              this is a vector column (more complicated case)
               write(crow,2000)rstart+1
               write(cp1,2000)estart+1
               write(cp2,2000)itodo
               call ftpmsg('Error reading'//cp2//' elements from'
     &         //' column'//ccol)
               call ftpmsg(' starting at row'//crow
     &         //', element'//cp1//' (FTGCLM).')
             end if
            return
        end if

        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo*2
                estart=estart+itodo*eincr
                rskip=estart/repeat
                rstart=rstart+rskip
                estart=estart-rskip*repeat
                go to 20
        end if

C       check for null values and/or scale the values
        if (nulchk .ne. 0 .or. scaled)then
          call ftnulm(array,nelem,nulchk,nulval,flgval,anynul,
     &     scaled,scale,zero)
        end if
        end
