C----------------------------------------------------------------------
        subroutine ftpclu(ounit,colnum,frow,felem,nelem,status)

C       set elements of a table to be undefined

C       ounit   i  fortran unit number
C       colnum  i  number of the column to write to
C       frow    i  first row to write
C       felem   i  first element within the row to write
C       nelem   i  number of elements to write
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,frow,felem,nelem,status

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
        character snull*500
        character*1 xdummy(5260)
        common/ftheap/snull,xdummy
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bytpix,bstart,i4null,tcode,nchars,i,offset,nulval
        integer ibuff,ntodo,itodo,repeat,rstart,estart
        integer*2 i2null,l1null
        real r4null
        double precision r8null
        logical descrp
        character*1 i1null
        character crow*9,cp1*9,cp2*9,ccol*4

        if (status .gt. 0)return

C       check for zero length array
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                write(crow,2000)frow
2000            format(i9)
                call ftpmsg('Starting row number for table write '//
     &         'request is out of range:'//crow//' (FTPCLU).')
                return
        end if
        ibuff=bufnum(ounit)

C       if HDU structure is not defined then scan the header keywords
        if (dtstrt(ibuff) .lt. 0)call ftrdef(ounit,status)

        tcode=tdtype(colnum+tstart(ibuff))
        bytpix=max(abs(tcode)/10,1)

        descrp=.false.
        ntodo=nelem
        rstart=frow-1

        if (felem .lt. 1)then
C               illegal element number
                status=308
                write(crow,2000)felem
                call ftpmsg('Starting element number for write '//
     &         'request is out of range:'//crow//' (FTPCLU).')
                return
        else
                estart=felem-1
        end if

        if (tcode .eq. 16)then
C               this is an ASCII field
                repeat=trept(colnum+tstart(ibuff))
                if (felem .gt. repeat)then
                        status=308
                        write(crow,2000)felem
                call ftpmsg('Starting element number for write '//
     &         'request is out of range:'//crow//' (FTPCLU).')
                        return
                end if

                if (cnull(colnum+tstart(ibuff))(1:1) .eq. char(1))then
C                       error: null value has not been defined
                        status=314
                call ftpmsg('Null value string for ASCII table'//
     &          ' column has not yet been defined (FTPCLU).')
                        return
                end if
C               the TNULL parameter stores the width of the character field
                bytpix=tnull(colnum+tstart(ibuff))
        else
C               this is a binary table
                nulval=tnull(colnum+tstart(ibuff))

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
                                return
                        end if
                else
C                       this is a variable length descriptor column
                        descrp=.true.
                        tcode=-tcode
C                       read the number of elements and the starting offset:
                        call ftgdes(ounit,colnum,frow,repeat,
     &                              offset,status)
                        if (ntodo+estart .gt. repeat)then
C                               error:  tried to write past end of record
                                status=319
                                return
                        end if

C                       move the i/o pointer to the start of the pixel sequence
                        bstart=dtstrt(ibuff)+offset+
     &                          theap(ibuff)+estart*bytpix
                        call ftmbyt(ounit,bstart,.true.,status)
                end if

                if (tcode.eq.11 .or. tcode.eq.21 .or. tcode.eq.41)then
                        if (nulval .eq. 123454321)then
C                               error: null value has not been defined
                                status=314
                call ftpmsg('Null value for integer'//
     &          ' column has not yet been defined (FTPCLU).')
                                return
                        end if
                else
C                       set the floating point Not-a-Number values
                        call ftsrnn(r4null)
                        call ftsdnn(r8null)
                end if

        end if

C       process as many contiguous pixels as possible
20      itodo=min(ntodo,repeat-estart)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)
     &             +tbcol(colnum+tstart(ibuff))+estart*bytpix
            call ftmbyt(ounit,bstart,.true.,status)
        end if

C       write the appropriate null value to the pixels
        if (tcode .eq. 21)then
C               column data type is I (I*2)
                do 5 i=1,itodo
                        i2null=nulval
                        call ftpi2b(ounit,1,0,i2null,status)
5               continue
        else if (tcode .eq. 41)then
C               column data type is J (I*4)
                do 15 i=1,itodo
                        i4null=nulval
                        call ftpi4b(ounit,1,0,i4null,status)
15              continue
        else if (tcode .eq. 42)then
C               column data type is E (R*4)
                do 25 i=1,itodo
                        call ftpbyt(ounit,4,r4null,status)
25              continue
        else if (tcode .eq. 82 .or. tcode .eq. 83)then
C               column data type is D (R*8), or C complex 2 x R*4
                do 35 i=1,itodo
                        call ftpbyt(ounit,8,r8null,status)
35              continue
        else if (tcode .eq. 16)then
C               this is an ASCII table column
                snull=cnull(colnum+tstart(ibuff))
C               write up to 500 characters in the column, remainder unchanged
C               (500 is the maximum size string allowed in IBM AIX compiler)
                nchars=min(bytpix,500)
                do 45 i=1,itodo
                        call ftpcbf(ounit,1,nchars,snull,status)
45              continue
        else if (tcode .eq. 11)then
C               column data type is B (byte)
                i1null=char(nulval)
                do 55 i=1,itodo
                        call ftpcbf(ounit,0,1,i1null,status)
55              continue
        else if (tcode .eq. 163)then
C               column data type is double complex (M)
                do 65 i=1,itodo*2
                        call ftpbyt(ounit,8,r8null,status)
65              continue
        else if (tcode .eq. 14)then
C               column data type is logical (L)
                l1null=0
                do 85 i=1,itodo
                        call ftpbyt(ounit,1,l1null,status)
85              continue
        end if


        if (status .gt. 0)then
          if (hdutyp(ibuff) .eq. 0)then
C            this is a primary array or image extension
             write(cp1,2000)felem+nelem-ntodo
             write(cp2,2000)felem+nelem-ntodo+itodo-1
             call ftpmsg('Error writing Nulls to pixels'
     &       //cp1//' to'//cp2//' in the FITS array (FTPCLU).')
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
               write(cp1,2000)felem
               write(cp2,2000)felem+nelem-1
               call ftpmsg('Error writing Nulls to elements'//cp1//
     &         ' to'//cp2 //' in row'//crow)
               call ftpmsg(' of variable length vector column'//ccol
     &                    //' (FTPCLU.')
             else if (trept(colnum+tstart(ibuff)) .eq. 1)then
C              this is not a vector column (simple case)
               write(cp1,2000)frow
               write(cp2,2000)frow+nelem-1
               call ftpmsg('Error writing Nulls to rows'//cp1//' to'
     &         //cp2//' of column'//ccol//' (FTPCLU).')
             else
C              this is a vector column (more complicated case)
               write(crow,2000)rstart+1
               write(cp1,2000)estart+1
               write(cp2,2000)itodo
               call ftpmsg('Error writing'//cp2//' Null elements to'
     &         //' column'//ccol)
               call ftpmsg(' starting at row'//crow
     &         //', element'//cp1//' (FTPCLU).')
             end if
          end if
          return
        end if

C       find number of pixels left to do, and quit if none left
        ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        rstart=rstart+1
                end if
                go to 20
        end if
        end
