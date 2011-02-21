C----------------------------------------------------------------------
        subroutine ftgcfl(iunit,colnum,frow,felem,nelem,lray,
     &          flgval,anynul,status)

C       read an array of logical values from a specified column of the table.
C       The binary table column being read from must have datatype 'L'
C       and no datatype conversion will be perform if it is not.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       lray    l  returned array of data values that is read
C       flgval  l  set .true. if corresponding element undefined
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical lray(*),flgval(*),anynul

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

        integer bstart,maxpix,tcode,offset
        integer ibuff,i,i1,ntodo,itodo,repeat,rstart,estart
        character*1 buffer(80)
        logical descrp
        
        if (status .gt. 0)return

        ibuff=bufnum(iunit)
C       check for zero length array
        if (nelem .le. 0)then
                return
        else if (frow .lt. 1)then
C               error: illegal first row number
                status=307
        else if (felem .lt. 1)then
C                       illegal element number
                        status=308
        end if

        if (status .gt. 0)return

C       initialize the null flag array
        do 5 i=1,nelem
                flgval(i)=.false.
5       continue
        anynul=.false.

        i1=0
        ntodo=nelem
        rstart=frow-1
        estart=felem-1
        maxpix=80
        tcode=tdtype(colnum+tstart(ibuff))

        if (tcode .eq. 14)then
                repeat=trept(colnum+tstart(ibuff))
                if (felem .gt. repeat)then
C                               illegal element number
                                status=308
                                return
                end if
                descrp=.false.
        else if (tcode .eq. -14)then
C               this is a variable length descriptor column
                descrp=.true.
C               read the number of elements and the starting offset:
                call ftgdes(iunit,colnum,frow,repeat,
     &                              offset,status)
                if (repeat .eq. 0)then
C                       error: null length vector
                        status=318
                        return
                else if (estart+ntodo .gt. repeat)then
C                       error: trying to read beyond end of record
                        status=319
                        return
                end if
C               move the i/o pointer to the start of the pixel sequence
                bstart=dtstrt(ibuff)+offset+
     &                          theap(ibuff)+estart
                call ftmbyt(iunit,bstart,.true.,status)
        else
C               column must be logical data type
                status=312
                return
        end if

C       process as many contiguous pixels as possible
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &             tbcol(colnum+tstart(ibuff))+estart
            call ftmbyt(iunit,bstart,.false.,status)
        end if

C       get the array of logical bytes
        call ftgcbf(iunit,1,itodo,buffer,status)
        if (status .gt. 0)return

C       decode the 'T' and 'F' characters, and look for nulls (0)
        do 10 i=1,itodo
                if (buffer(i) .eq. 'T')then
                        lray(i1+i)=.true.
                else if (buffer(i) .eq. 'F')then
                        lray(i1+i)=.false.
                else if (ichar(buffer(i)) .eq. 0)then
                        flgval(i1+i)=.true.
                        anynul=.true.
                else
                        status=316
                        return
                end if
10      continue
        
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
