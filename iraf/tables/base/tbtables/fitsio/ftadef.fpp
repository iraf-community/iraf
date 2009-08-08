C--------------------------------------------------------------------------
        subroutine ftadef(ounit,lenrow,nfield,bcol,tform,nrows,status)

C       Ascii table data DEFinition
C       define the structure of the ASCII table data unit
C
C       ounit   i  Fortran I/O unit number
C       lenrow  i  length of a row, in characters
C       nfield  i  number of fields in the table
C       bcol    i  starting position of each column, (starting with 1)
C       tform   C  the data format of the column
C       nrows   i  number of rows in the table
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,lenrow,nfield,bcol(*),nrows,status
        character*(*) tform(*)

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------
        
        integer ibuff,i,j,clen,c2
        character ctemp*24, cnum*3,cbcol*10,caxis1*10

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

        if (dtstrt(ibuff) .lt. 0)then
C               freeze the header at its current size
                call fthdef(ounit,0,status)
                if (status .gt. 0)return
        end if

        hdutyp(ibuff)=1
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
            clen=len(tform(1))
            do 20 i=1,nfield
                tscale(i+tstart(ibuff))=1.
                tzero(i+tstart(ibuff))=0.
C               choose special value to indicate null values are not defined
                cnull(i+tstart(ibuff))=char(1)
                cform(i+tstart(ibuff))=tform(i)
                tbcol(i+tstart(ibuff))=bcol(i)-1
                tdtype(i+tstart(ibuff))=16
C               the repeat count is always one for ASCII tables
                trept(i+tstart(ibuff))=1
C               store the width of the field in TNULL
                c2=0
                do 10 j=2,clen
                        if (tform(i)(j:j) .ge. '0' .and.
     &                     tform(i)(j:j) .le. '9')then
                                c2=j
                        else
                                go to 15
                        end if
10              continue
15              continue
                if (c2 .eq. 0)then
C                       no explicit width, so assume width of 1 character
                        tnull(i+tstart(ibuff))=1
                else
                    call ftc2ii(tform(i)(2:c2),tnull(i+tstart(ibuff))
     &                          ,status)
                    if (status .gt. 0)then
C                        error parsing TFORM to determine field width
                         status=261
                         ctemp=tform(i)
                         call ftpmsg('Error parsing TFORM to get field'
     &                    //' width: '//ctemp)
                         return
                    end if
                end if

C               check that column fits within the table
                if (tbcol(i+tstart(ibuff))+tnull(i+tstart(ibuff)) 
     &            .gt. lenrow .and. lenrow .ne. 0)then
                    status=236
                    write(cnum,1000)i
                    write(cbcol,1001)bcol(i)
                    write(caxis1,1001)lenrow
1000                format(i3)
1001                format(i10)
                    call ftpmsg('Column '//cnum//' will not fit '//
     &             'within the specified width of the ASCII table.')

                    call ftpmsg('TFORM='//cform(i+tstart(ibuff))//
     &              '  TBCOL='//cbcol//'  NAXIS1='//caxis1)
                    return
                 end if
20           continue

C           calculate the start of the next header unit, based on the
C           size of the data unit
            rowlen(ibuff)=lenrow
            hdstrt(ibuff,chdu(ibuff)+1)=
     &          dtstrt(ibuff)+(lenrow*nrows+2879)/2880*2880

C       initialize the fictitious heap starting address (immediately following
C       the table data) and a zero length heap.  This is used to find the
C       end of the table data when checking the fill values in the last block. 
C           ASCII tables have no special data area
            scount(ibuff)=0
            theap(ibuff)=rowlen(ibuff)*nrows
            nxheap(ibuff)=0
        end if
        end
