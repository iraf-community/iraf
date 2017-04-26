C--------------------------------------------------------------------------
        subroutine ftbdef(ounit,nfield,tform,pcount,nrows,status)

C       Binary table data DEFinition
C       define the structure of the binary table data unit
C
C       ounit   i  Fortran I/O unit number
C       nfield  i  number of fields in the table
C       tform   C  the data format of the column
C       nrows   i  number of rows in the table
C       pcount  i  size in bytes of the special data block following the table
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,nfield,nrows,pcount,status
        character*(*) tform(*)

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne,nf
        parameter (nb = 20)
        parameter (ne = 200)
        parameter (nf = 3000)
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
C-------END OF COMMON BLOCK DEFINITIONS-----------------------------------
        
        integer ibuff,i,j,width

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

        if (dtstrt(ibuff) .lt. 0)then
C               freeze the header at its current size
                call fthdef(ounit,0,status)
                if (status .gt. 0)return
        end if

        hdutyp(ibuff)=2
        tfield(ibuff)=nfield

        if (nxtfld + nfield .gt. nf)then
C               too many columns open at one time; exceeded array dimensions
                status=111
                return
        end if

        tstart(ibuff)=nxtfld
        nxtfld=nxtfld+nfield

        if (nfield .eq. 0)then
C           no data; the next HDU begins in the next logical block 
            hdstrt(ibuff,chdu(ibuff)+1)=dtstrt(ibuff)
            scount(ibuff)=0
            theap(ibuff)=0
            nxheap(ibuff)=0
        else
C           initialize the table column parameters
            do 5 i=1,nfield
                tscale(i+tstart(ibuff))=1.
                tzero(i+tstart(ibuff))=0.
C               choose special value to indicate that null value is not defined
                tnull(i+tstart(ibuff))=123454321
C               reset character NUL string, in case it has been 
C               previously defined from an ASCII table extension
                cnull(i+tstart(ibuff))=char(0)

C               parse the tform strings to get the data type and repeat count
                call ftbnfm(tform(i),tdtype(i+tstart(ibuff)),
     &                      trept(i+tstart(ibuff)),width,status)
                if (tdtype(i+tstart(ibuff)) .eq. 1)then
C                  treat Bit datatype as if it were a Byte datatype
                   tdtype(i+tstart(ibuff))=11
                   trept(i+tstart(ibuff))=(trept(i+tstart(ibuff))+7)/8
                else if (tdtype(i+tstart(ibuff)) .eq. 16)then
C                       store ASCII unit string length in TNULL parameter
                        tnull(i+tstart(ibuff))=width
                end if
                if (status .gt. 0)return
5           continue

C           determine byte offset of the beginning of each field and row length
            call ftgtbc(nfield,tdtype(1+tstart(ibuff)),trept(1+
     &           tstart(ibuff)),tbcol(1+tstart(ibuff)),rowlen(ibuff),
     &                  status)

C           FITSIO deals with ASCII columns as arrays of strings, not
C           arrays of characters, so need to change the repeat count
C           to indicate the number of strings in the field, not the 
C           total number of characters in the field.
            do 10 i=1,nfield
                if (tdtype(i+tstart(ibuff)) .eq. 16)then
                    j=trept(i+tstart(ibuff))/tnull(i+tstart(ibuff))
                    trept(i+tstart(ibuff))=max(j,1)
                end if
10          continue

C           initialize the heap offset (=nrows x ncolumns)
C           store the size of the special data area, if any
            scount(ibuff)=pcount
            theap(ibuff)=nrows*rowlen(ibuff)
            nxheap(ibuff)=0

C           calculate the start of the next header unit, based on the
C           size of the data unit (table + special data)
            hdstrt(ibuff,chdu(ibuff)+1)=
     &       dtstrt(ibuff)+(rowlen(ibuff)*nrows+pcount+2879)/2880*2880
        end if
        end
