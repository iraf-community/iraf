C--------------------------------------------------------------------------
        subroutine ftbini(iunit,status)

C       initialize the parameters defining the structure of a binary table 

C       iunit   i  Fortran I/O unit number
C       OUTPUT PARAMETERS:
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,status

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
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
C-------END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer lenrow,nrows,pcnt,tfld,nkey,ibuff,i,j,nblank
        character keynam*8,value*70,comm*72,cnaxis*8,clen*8,rec*80

        if (status .gt. 0)return

C       define the number of the buffer used for this file
        ibuff=bufnum(iunit)

C       store the type of HDU (2 = Binary table extension)
        hdutyp(ibuff)=2

C       temporarily set the location of the end of the header to a huge number
        hdend(ibuff)=2000000000
        hdstrt(ibuff,chdu(ibuff)+1)=2000000000

C       check that this is a valid binary table, and get parameters
        call ftgtbn(iunit,rowlen(ibuff),nrows,pcnt,tfld,status)
        if (status .gt. 0)go to 900

        if (tfld .gt. nf)then
C               arrays not dimensioned large enough for this many fields
                status=111
        call ftpmsg('This Binary table has too many fields '//
     & 'to be read with FITSIO (FTBINI).')
                go to 900
         end if

C       store the number of fields in the common block
        tfield(ibuff)=tfld

        if (nxtfld + tfld .gt. nf)then
C               too many columns open at one time; exceeded array dimensions
                status=111
                return
        end if

        tstart(ibuff)=nxtfld
        nxtfld=nxtfld+tfld

C       initialize the table field parameters
        do 5 i=1,tfld
                tscale(i+tstart(ibuff))=1.
                tzero(i+tstart(ibuff))=0.
                tnull(i+tstart(ibuff))=123454321
                tdtype(i+tstart(ibuff))=-9999
                trept(i+tstart(ibuff))=0
C               reset character NUL string, in case it has been previously
C               defined from an ASCII table extension
                cnull(i+tstart(ibuff))=char(0)
5       continue

C       initialize the default heap starting address (immediately following
C       the table data) and set the next empty heap address 
C       PCOUNT specifies the amount of special data following the table
        scount(ibuff)=pcnt
        theap(ibuff)=rowlen(ibuff)*nrows
        nxheap(ibuff)=pcnt

C       now read through the rest of the header looking for table column
C       definition keywords, and the END keyword.

        nkey=8
8       nblank=0
10      nkey=nkey+1
        call ftgrec(iunit,nkey,rec,status)
        if (status .eq. 107)then
C               if we hit the end of file, then set status = no END card found
                status=210
        call ftpmsg('Required END keyword not found in Binary table'//
     &  ' header (FTBINI).')
                go to 900
        else if (status .gt. 0)then
                go to 900
        end if
        keynam=rec(1:8)
        comm=rec(9:80)

        if (keynam(1:1) .eq. 'T')then
C               get the binary table parameter (if it is one)
                call ftpsvc(rec,value,comm,status)
                call ftgbtp(ibuff,keynam,value,status)
        else if (keynam .eq. ' ' .and. comm .eq. ' ')then
                nblank=nblank+1
                go to 10
        else if (keynam .eq. 'END')then
                go to 20
        end if
        go to 8

20      continue

C       test that all the required keywords were found
        do 25 i=1,tfld
            if (tdtype(i+tstart(ibuff)) .eq. -9999)then
                status=232
                call ftkeyn('TFORM',i,keynam,status)
                call ftpmsg('Required '//keynam//
     &                      ' keyword not found (FTAINI).')
                return
            end if
25      continue


C       now we know everything about the table; just fill in the parameters:
C       the 'END' record begins 80 bytes before the current position, ignoring
C       any trailing blank keywords just before the END keyword
        hdend(ibuff)=nxthdr(ibuff)-80*(nblank+1)

C       the data unit begins at the beginning of the next logical block
        dtstrt(ibuff)=((nxthdr(ibuff)-80)/2880+1)*2880

C       reset header pointer to the first keyword
        nxthdr(ibuff)=hdstrt(ibuff,chdu(ibuff))

C       the next HDU begins in the next logical block after the data
        hdstrt(ibuff,chdu(ibuff)+1)=
     &  dtstrt(ibuff)+(rowlen(ibuff)*nrows+pcnt+2879)/2880*2880

C       determine the byte offset of the beginning of each field and row length
        if (tfld .gt. 0)then
           call ftgtbc(tfld,tdtype(1+tstart(ibuff)),
     &     trept(1+tstart(ibuff)),tbcol(1+tstart(ibuff)),lenrow,status)

C          FITSIO deals with ASCII columns as arrays of strings, not
C          arrays of characters, so need to change the repeat count
C          to indicate the number of strings in the field, not the 
C          total number of characters in the field.
           do 30 i=1,tfld
                if (tdtype(i+tstart(ibuff)) .eq. 16)then
                    j=trept(i+tstart(ibuff))/tnull(i+tstart(ibuff))
                    trept(i+tstart(ibuff))=max(j,1)
                end if
30         continue
           if (status .gt. 0)go to 900

C          check that the sum of the column widths = NAXIS2 value
           if (rowlen(ibuff) .ne. lenrow)then
                status=241
                write(cnaxis,1001)rowlen(ibuff)
                write(clen,1001)lenrow
1001            format(i8)
           call ftpmsg('NAXIS1 ='//cnaxis//' not equal'//
     &     ' to the sum of the column widths ='//clen//' (FTBINI).')
           end if
        end if

900     continue
        end
