C--------------------------------------------------------------------------
        subroutine ftaini(iunit,status)

C       initialize the parameters defining the structure of an ASCII table 

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

        integer nrows,tfld,nkey,ibuff,i,nblank
        character keynam*8,value*70,comm*72,rec*80
        character cnum*3,cbcol*10,caxis1*10

        if (status .gt. 0)return

C       define the number of the buffer used for this file
        ibuff=bufnum(iunit)

C       store the type of HDU (1 = ASCII table extension)
        hdutyp(ibuff)=1

C       temporarily set the location of the end of the header to a huge number
        hdend(ibuff)=2000000000
        hdstrt(ibuff,chdu(ibuff)+1)=2000000000

C       check that this is a valid ASCII table, and get parameters
        call ftgttb(iunit,rowlen(ibuff),nrows,tfld,status)
        if (status .gt. 0)go to 900

        if (tfld .gt. nf)then
C               arrays not dimensioned large enough for this many fields
                status=111
        call ftpmsg('This ASCII table has too many fields '//
     & 'to be read with FITSIO (FTAINI).')
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
C               choose special value to indicate that null value is not defined
                cnull(i+tstart(ibuff))=char(1)
C               pre-set required keyword values to a null value
                tbcol(i+tstart(ibuff))=-1
                tdtype(i+tstart(ibuff))=-9999
5       continue

C       initialize the fictitious heap starting address (immediately following
C       the table data) and a zero length heap.  This is used to find the
C       end of the table data when checking the fill values in the last block. 
C       there is no special data following an ASCII table
        scount(ibuff)=0
        theap(ibuff)=rowlen(ibuff)*nrows
        nxheap(ibuff)=0

C       now read through the rest of the header looking for table column
C       definition keywords, and the END keyword.

        nkey=8
8       nblank=0
10      nkey=nkey+1
        call ftgrec(iunit,nkey,rec,status)
        if (status .eq. 107)then
C               if we hit the end of file, then set status = no END card found
                status=210
        call ftpmsg('Required END keyword not found in ASCII table'//
     &  ' header (FTAINI).')
                go to 900
        else if (status .gt. 0)then
                go to 900
        end if
        keynam=rec(1:8)
        comm=rec(9:80)

        if (keynam(1:1) .eq. 'T')then
C               get the ASCII table parameter (if it is one)
                call ftpsvc(rec,value,comm,status)
                call ftgatp(ibuff,keynam,value,status)
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
            if (tbcol(i+tstart(ibuff)) .eq. -1)then
                status=231
                call ftkeyn('TBCOL',i,keynam,status)
                call ftpmsg('Required '//keynam//
     &                      ' keyword not found (FTAINI).')
                return
            else if (tbcol(i+tstart(ibuff)) .lt. 0 .or.
     &               tbcol(i+tstart(ibuff)) .ge. rowlen(ibuff)
     &               .and. rowlen(ibuff) .ne. 0)then
                status=234
                call ftkeyn('TBCOL',i,keynam,status)
                call ftpmsg('Value of the '//keynam//
     &                      ' keyword is out of range (FTAINI).')
                return

C               check that column fits within the table
            else if (tbcol(i+tstart(ibuff))+tnull(i+tstart(ibuff)) .gt.
     &               rowlen(ibuff) .and. rowlen(ibuff) .ne. 0)then
                    status=236
                    write(cnum,1000)i
                    write(cbcol,1001)tbcol(i+tstart(ibuff))+1
                    write(caxis1,1001)rowlen(ibuff)
1000                format(i3)
1001                format(i10)
                    call ftpmsg('Column '//cnum//' will not fit '//
     &             'within the specified width of the ASCII table.')

                    call ftpmsg('TFORM='//cform(i+tstart(ibuff))//
     &              '  TBCOL='//cbcol//'  NAXIS1='//caxis1)
                    return
            else if (tdtype(i+tstart(ibuff)) .eq. -9999)then
                status=232
                call ftkeyn('TFORM',i,keynam,status)
                call ftpmsg('Required '//keynam//
     &                      ' keyword not found (FTAINI).')
                return
            end if
25      continue

C       now we know everything about the table; just fill in the parameters:
C       the 'END' record begins 80 bytes before the current position,
C       ignoring any trailing blank keywords just before the END keyword
        hdend(ibuff)=nxthdr(ibuff)-80*(nblank+1)

C       the data unit begins at the beginning of the next logical block
        dtstrt(ibuff)=((nxthdr(ibuff)-80)/2880+1)*2880

C       reset header pointer to the first keyword
        nxthdr(ibuff)=hdstrt(ibuff,chdu(ibuff))

C       the next HDU begins in the next logical block after the data
        hdstrt(ibuff,chdu(ibuff)+1)=
     &  dtstrt(ibuff)+(rowlen(ibuff)*nrows+2879)/2880*2880

900     continue
        end
