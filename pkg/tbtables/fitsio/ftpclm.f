C----------------------------------------------------------------------
        subroutine ftpclm(ounit,colnum,frow,felem,nelem,array,status)

C       write an array of double precision complex data values to the 
C       specified column of the table.  
C       The binary table column being written to must have datatype 'M'
C       and no datatype conversion will be perform if it is not.

C       ounit   i  fortran unit number
C       colnum  i  number of the column to write to
C       frow    i  first row to write
C       felem   i  first element within the row to write
C       nelem   i  number of elements to write
C       array   dcmp  array of data values to be written 
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,frow,felem,nelem,status
C       array is really double precison complex
        double precision array(*)

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

        double precision buffer(100)
        integer bytpix,bstart,tcode
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart,maxpix
        double precision scale,zero
        logical descrp,scaled
        character crow*9,cp1*9,cp2*9,ccol*4

        if (status .gt. 0)return

C       check for zero length array or bad first row number
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                write(crow,2000)frow
2000            format(i9)
                call ftpmsg('Starting row number for table write '//
     &          'request is out of range:'//crow//' (FTPCLM).')
                return
        end if

        ibuff=bufnum(ounit)

C       if HDU structure is not defined then scan the header keywords
        if (dtstrt(ibuff) .lt. 0)call ftrdef(ounit,status)

        i1=1
C       multiply by 2, because the complex data type has pairs of values
        ntodo=nelem*2
        rstart=frow-1
        scale=tscale(colnum+tstart(ibuff))
        zero=tzero(colnum+tstart(ibuff))
        if (scale .eq. 1. .and. zero .eq. 0.)then
                scaled=.false.
        else
                scaled=.true.
        end if
        tcode=tdtype(colnum+tstart(ibuff)) 

        if (felem .lt. 1)then
C               illegal element number
                status=308
                write(crow,2000)felem
                call ftpmsg('Starting element number for write '//
     &          'request is out of range:'//crow//' (FTPCLM).')
                return
        else
C               multiply by 2 because the complex data type has pairs of values
                estart=(felem-1)*2
        end if

C       calculate the maximum number of column pixels which fit in buffer
        bytpix=8
        maxpix=100

        if (tcode .eq. 163)then
                repeat=trept(colnum+tstart(ibuff))*2
                if (felem*2 .gt. repeat)then
C                       illegal element number
                        status=308
                        write(crow,2000)felem
                   call ftpmsg('Starting element number for write '//
     &             'request is out of range:'//crow//' (FTPCLM).')
                        return
                end if
                descrp=.false.
        else if (tcode .eq. -163)then
C               this is a variable length descriptor column
                descrp=.true.
                repeat=nelem+felem-1
C               write the number of elements and the starting offset:
                call ftpdes(ounit,colnum,frow,repeat,
     &                              nxheap(ibuff),status)
                repeat=repeat*2
C               move the i/o pointer to the start of the pixel sequence
                bstart=dtstrt(ibuff)+nxheap(ibuff)+
     &                  theap(ibuff)+estart*bytpix
                call ftmbyt(ounit,bstart,.true.,status)
C               increment the empty heap starting address:
                nxheap(ibuff)=nxheap(ibuff)+repeat*bytpix
        else
C               error illegal binary table data type code
                status=312
                return
        end if

C       process as many contiguous pixels as possible, up to buffer size
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)
     &             +tbcol(colnum+tstart(ibuff))+estart*bytpix
            call ftmbyt(ounit,bstart,.true.,status)
        end if

C       scale data into buffer,
        call ftuscm(array(i1),itodo,scaled,scale,zero,buffer)

C       do any machine dependent data conversion and write the R*8 data
        call ftpr8b(ounit,itodo,0,buffer,status)

        if (status .gt. 0)then
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
     &                    //' (FTPCLM.')
             else if (trept(colnum+tstart(ibuff)) .eq. 1)then
C              this is not a vector column (simple case)
               write(cp1,2000)frow+i1-1
               write(cp2,2000)frow+i1+itodo-2
               call ftpmsg('Error writing rows'//cp1//' to'//cp2
     &           //' of column'//ccol//' (FTPCLM).')
             else
C              this is a vector column (more complicated case)
               write(crow,2000)rstart+1
               write(cp1,2000)estart+1
               write(cp2,2000)itodo
               call ftpmsg('Error writing'//cp2//' elements to'
     &         //' column'//ccol)
               call ftpmsg(' starting at row'//crow
     &         //', element'//cp1//' (FTPCLM).')
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
                        rstart=rstart+1
                end if
                go to 20
        end if
        end
