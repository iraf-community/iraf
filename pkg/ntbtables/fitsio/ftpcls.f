C----------------------------------------------------------------------
        subroutine ftpcls(ounit,colnum,frow,felem,nelem,sray,status)

C       write an array of character string values to the  specified column of 
C       the table.
C       The binary or ASCII table column being written to must have datatype 'A'

C       ounit   i  fortran unit number
C       colnum  i  number of the column to write to
C       frow    i  first row to write
C       felem   i  first element within the row to write
C       nelem   i  number of elements to write
C       sray    c  array of data values to be written
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,frow,felem,nelem,status
        character*(*) sray(*)

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

        integer bstart,strlen,c1,c2,repeat,twidth
        integer ibuff,i1,ntodo,rstart,estart,nchars,clen,tcode
        character sbuff*80,blank*80,crow*9,cp1*9,cp2*9,ccol*4
        logical small,fill
        
        if (status .gt. 0)return

C       check for zero length array
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                write(crow,2000)frow
2000            format(i9)
                call ftpmsg('Starting row number for table write '//
     &          'request is out of range:'//crow//' (FTPCLS).')
                return
        end if
        if (felem .lt. 1)then
C               illegal element number
                status=308
                write(crow,2000)felem
                call ftpmsg('Starting element number for write '//
     &          'request is out of range:'//crow//' (FTPCLS).')
                return
        end if

        ibuff=bufnum(ounit)

C       if HDU structure is not defined then scan the header keywords
        if (dtstrt(ibuff) .lt. 0)call ftrdef(ounit,status)

        blank=' '
        i1=1

C       column must be character string data type
        tcode=tdtype(colnum+tstart(ibuff)) 
        if (tcode .eq. 16)then
C               for ASCII columns, TNULL actually stores the field width
                twidth=tnull(colnum+tstart(ibuff)) 
                ntodo=nelem
                rstart=frow-1
                repeat=trept(colnum+tstart(ibuff))
                estart=felem-1
                if (estart .ge. repeat)then
C                       illegal element number
                        status=308
                        write(crow,2000)felem
                call ftpmsg('Starting element number for write '//
     &          'request is out of range:'//crow//' (FTPCLS).')
                        return
                end if
                bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)
     &                 +tbcol(colnum+tstart(ibuff))+estart*twidth
        else if (tcode .eq. -16)then
C               this is a variable length descriptor field
C               the length of the output string is defined by nelem
                twidth=nelem
                ntodo=1
                repeat=1
C               write the number of string length and the starting offset:
                call ftpdes(ounit,colnum,frow,twidth,
     &                              nxheap(ibuff),status)
C               calc the i/o pointer position for the start of the string
                bstart=dtstrt(ibuff)+nxheap(ibuff)+theap(ibuff)
C               increment the empty heap starting address:
                nxheap(ibuff)=nxheap(ibuff)+twidth
        else
C               error: not a character string column
                status=309
                return
        end if

C       move the i/o pointer to the start of the sequence of pixels
        call ftmbyt(ounit,bstart,.true.,status)

C       is the input string short enough to completely fit in buffer?
        strlen=len(sray(1))
        if (strlen .gt. 80 .and. twidth .gt. 80)then
                small=.false.
        else
                small=.true.
        end if

C       do we need to pad the FITS string field with trailing blanks?
        if (twidth .gt. strlen)then
                fill=.true.
        else
                fill=.false.
        end if

C       process one string at a time
20      continue
        nchars=min(strlen,twidth)
        if (small)then
C               the whole input string fits in the temporary buffer
                sbuff=sray(i1)
C               output the string
                call ftpcbf(ounit,1,nchars,sbuff,status)
        else
C               have to write the string in several pieces
                c1=1
                c2=80
30              sbuff=sray(i1)(c1:c2)
C               output the string
                clen=c2-c1+1
                call ftpcbf(ounit,1,clen,sbuff,status)
                nchars=nchars-clen
                if (nchars .gt. 0)then
                        c1=c1+80
                        c2=min(c2+80,c1+nchars-1)
                        go to 30
                end if
        end if

C       pad any remaining space in the column with blanks
        if (fill)then
                nchars=twidth-strlen
40              clen=min(nchars,80)
                call ftpcbf(ounit,1,clen,blank,status)
                nchars=nchars-80
                if (nchars .gt. 0)go to 40
        end if

        if (status .gt. 0)then
          write(cp1,2000)i1
          call ftpmsg('Error while writing ASCII string to ')
             write(ccol,2001)colnum
2001         format(i4)
             write(cp1,2000)rstart+1
             write(cp2,2000)estart+1
             if (felem .eq. 1)then
               call ftpmsg('column'//ccol//', row'//cp1
     &                    //' (FTPCLS).')
             else
               call ftpmsg('column'//ccol//', row'//cp1
     &         //', element'//cp2//' (FTPCLS).')    
             end if
          return
        end if

C       find number of pixels left to do, and quit if none left
        ntodo=ntodo-1
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+1
                estart=estart+1
                if (estart .eq. repeat)then
                        estart=0
                        rstart=rstart+1
                        bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &                  tbcol(colnum+tstart(ibuff))
C                       move the i/o pointer 
                        call ftmbyt(ounit,bstart,.true.,status)
                end if
                go to 20
        end if
        end
