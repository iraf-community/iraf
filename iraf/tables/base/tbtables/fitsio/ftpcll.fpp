C----------------------------------------------------------------------
        subroutine ftpcll(ounit,colnum,frow,felem,nelem,lray,status)

C       write an array of logical values to the  specified column of the table.
C       The binary table column being written to must have datatype 'L'
C       and no datatype conversion will be perform if it is not.

C       ounit   i  fortran unit number
C       colnum  i  number of the column to write to
C       frow    i  first row to write
C       felem   i  first element within the row to write
C       nelem   i  number of elements to write
C       lray    l  array of data values to be written
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,frow,felem,nelem,status
        logical lray(*)

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

        integer bstart,maxpix,i
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart,tcode
        character*1 buffer(80)
        character crow*9,cp1*9,cp2*9,ccol*4
        logical descrp
        
        if (status .gt. 0)return

C       check for zero length array
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                write(crow,2000)frow
2000            format(i9)
                call ftpmsg('Starting row number for table write '//
     &          'request is out of range:'//crow//' (FTPCLL).')
                return
        end if

        if (felem .lt. 1)then
C               illegal element number
                status=308
                write(crow,2000)felem
                call ftpmsg('Starting element number for write '//
     &          'request is out of range:'//crow//' (FTPCLL).')
                return
        else
                estart=felem-1
        end if

        ibuff=bufnum(ounit)
C       if HDU structure is not defined then scan the header keywords
        if (dtstrt(ibuff) .lt. 0)call ftrdef(ounit,status)

        i1=1
        ntodo=nelem
        rstart=frow-1
        maxpix=80

C       column must be logical data type
        tcode=tdtype(colnum+tstart(ibuff)) 
        if (tcode .eq. 14)then
                descrp=.false.
                repeat=trept(colnum+tstart(ibuff))
                if (felem .gt. repeat)then
C                       illegal element number
                        status=308
                        write(crow,2000)felem
                call ftpmsg('Starting element number for write '//
     &          'request is out of range:'//crow//' (FTPCLL).')
                        return
                end if
        else if (tcode .eq. -14)then
                descrp=.true.
                repeat=nelem+estart
C               write the number of elements and the starting offset:
                call ftpdes(ounit,colnum,frow,repeat,
     &                              nxheap(ibuff),status)
C               move the i/o pointer to the start of the pixel sequence
                bstart=dtstrt(ibuff)+nxheap(ibuff)+
     &                          theap(ibuff)+estart
                call ftmbyt(ounit,bstart,.true.,status)
C               increment the empty heap starting address:
                nxheap(ibuff)=nxheap(ibuff)+repeat
        else 
C               error illegal data type code 
                status=310
                return
        end if

C       process as many contiguous pixels as possible
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &      tbcol(colnum+tstart(ibuff))+estart
            call ftmbyt(ounit,bstart,.true.,status)
        end if

C       create the buffer of logical bytes
        do 10 i=1,itodo
                if (lray(i1))then
                        buffer(i)='T'
                else
                        buffer(i)='F'
                end if
                i1=i1+1
10      continue

C       write out the buffer
        call ftpcbf(ounit,1,itodo,buffer,status)

        if (status .gt. 0)then
          write(cp1,2000)i1
          write(cp2,2000)i1+itodo-1
          call ftpmsg('Error while writing values'//cp1//' to'//cp2)
          write(ccol,2001)colnum
2001      format(i4)
          write(cp1,2000)frow
          write(cp2,2000)felem
          if (felem .eq. 1)then
               call ftpmsg('of column'//ccol//', starting at row'//cp1
     &                    //' (FTPCLL).')
          else
               call ftpmsg('of column'//ccol//', starting at row'//cp1
     &         //', element'//cp2//' (FTPCLL).')    
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
