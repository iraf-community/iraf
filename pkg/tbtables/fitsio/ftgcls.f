C----------------------------------------------------------------------
        subroutine ftgcls(iunit,colnum,frow,felem,nelem,nultyp,nulval,
     &    sray,flgval,anynul,status)

C       read an array of character string values from the specified column of 
C       the table.
C       The binary or ASCII table column being read must have datatype 'A'
C       This general purpose routine will handle null values in one
C       of two ways: if nultyp=1, then undefined array elements will be
C       set equal to the input value of NULVAL.  Else if nultyp=2, then
C       undefined array elements will have the corresponding FLGVAL element
C       set equal to .TRUE.  If NULTYP=1 and NULVAL=0, then no checks for
C       undefined values will be made, for maximum efficiency.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read from
C       frow    i  first row to read
C       felem   i  first element within row to read
C       nelem   i  number of elements to read
C       nultyp  i  input code indicating how to handle undefined values
C       nulval  c  value that undefined pixels will be set to (if nultyp=1)
C       sray    c  array of data values to be read
C       flgval  l  set .true. if corresponding element undefined (if nultyp=2)
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,nultyp,status
        logical flgval(*),anynul
        character*(*) sray(*),nulval

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

        integer bstart,nulchk,twidth,tread,tcode,offset,repeat
        integer ibuff,i1,ntodo,rstart,estart,lennul,strlen,nulfil
        character snull*16, crow*9,cp1*9,cp2*9,ccol*4
        
        if (status .gt. 0)return

C       check for zero length array
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                write(crow,2000)frow
2000            format(i9)
                call ftpmsg('Starting row number for table read '//
     &          'request is out of range:'//crow//' (FTGCLS).')
                return
        else if (felem .lt. 1)then
C                       illegal element number
                        status=308
                        write(crow,2000)felem
                call ftpmsg('Starting element number for read '//
     &          'request is out of range:'//crow//' (FTGCLS).')
                        return
        end if

        anynul=.false.
        ibuff=bufnum(iunit)
        i1=1

C       column must be character string data type

        tcode=tdtype(colnum+tstart(ibuff)) 
        if (tcode .eq. 16)then
C               for ASCII columns, TNULL actually stores the field width
                twidth=tnull(colnum+tstart(ibuff)) 
                ntodo=nelem
                rstart=frow-1
                repeat=trept(colnum+tstart(ibuff))
                if (felem .gt. repeat)then
C                       illegal element number
                        status=308
                        write(crow,2000)felem
                call ftpmsg('Starting element number for read '//
     &          'request is out of range:'//crow//' (FTGCLS).')
                        return
                end if
                estart=felem-1
                bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)
     &                 +tbcol(colnum+tstart(ibuff))+estart*twidth
        else if (tcode .eq. -16)then
C               this is a variable length descriptor field
                ntodo=1
C               read the string length and the starting offset:
                call ftgdes(iunit,colnum,frow,twidth,offset,status)
C               calc the i/o pointer position for the start of the string
                bstart=dtstrt(ibuff)+offset+theap(ibuff)
        else
C               error: not a character string column
                status=309
                call ftpmsg('Cannot to read character string'//
     &          ' from a non-character column of a table (FTGCLS).')
                return
        end if

C       define the max. number of charcters to be read: either
C       the length of the variable length field, or the length
C       of the character string variable, which ever is smaller
        strlen=len(sray(1))
        tread=min(twidth,strlen)

C       move the i/o pointer to the start of the sequence of pixels
        call ftmbyt(iunit,bstart,.false.,status)

        if (status .gt. 0)then
            call ftpmsg('Failed to move to starting position '//
     &      'to read character string(s)  (FTGCLS).')
            return
        end if

        lennul=0
C       determine if we have to check for null values
        if (nultyp .eq. 1 .and. nulval .eq. ' ')then
C               user doesn't want to check for nulls
                nulchk=0
        else
                nulchk=nultyp
                snull=cnull(colnum+tstart(ibuff))
C               lennul = length of the string to check for null values
                lennul=min(len(sray(1)),8)
        end if

C       process one string at a time
20      continue
C       get the string of characters
        sray(i1)=' '
        call ftgcbf(iunit,1,tread,sray(i1),status)
        if (status .gt. 0)return

C       check for null value, if required
        if (nulchk .ne. 0)then
                if (ichar(sray(i1)(1:1)) .eq. 0 .or.
     &              sray(i1)(1:lennul) .eq. snull(1:lennul))then
                        if (nulchk .eq. 1)then
                                sray(i1)=nulval
                                anynul=.true.
                        else
                                flgval(i1)=.true.
                                anynul=.true.
                        end if
                end if
        end if

C       check for null terminated string; pad out with blanks if found
        nulfil=index(sray(i1),char(0))
        if (nulfil .gt. 1)then
                sray(i1)(nulfil:len(sray(1)))=' '               
        end if

        if (status .gt. 0)then
          write(cp1,2000)i1
             write(ccol,2001)colnum
2001         format(i4)
             write(cp1,2000)rstart+1
             write(cp2,2000)estart+1
             if (felem .eq. 1)then
                call ftpmsg('Error while reading ASCII string from '//
     &           'column'//ccol//', row'//cp1//' (FTGCLS).')
             else
                call ftpmsg('Error reading string from '//
     &           'column'//ccol//', row'//cp1
     &         //', element'//cp2//' (FTGCLS).')    
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
                        rstart=rstart+1
                        estart=0
                end if
C               move to the start of the next string; need to do
C               this every time in case we didn't read all the characters
C               from the previous string.
                bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)
     &                 +tbcol(colnum+tstart(ibuff))+estart*twidth
C               move the i/o pointer
                call ftmbyt(iunit,bstart,.false.,status)
                go to 20
        end if
        end
