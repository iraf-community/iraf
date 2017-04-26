C----------------------------------------------------------------------
        subroutine ftptbs(iunit,frow,fchar,nchars,svalue,status)

C       write a consecutive string of characters to an ascii or binary
C       table. This will span multiple rows of the table if NCHARS+FCHAR is
C       greater than the length of a row.

C       iunit   i  fortran unit number
C       frow    i  starting row number (1st row = 1)
C       fchar   i  starting character/byte in the row to write (1st character=1)
C       nchars  i  number of characters/bytes to write (can span multiple rows)
C       svalue  c  string of characters to write
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Dec 1991

        integer iunit,frow,fchar,nchars,status
        character*(*) svalue

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

        integer ibuff,bstart
        
        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       check for errors
        if (nchars .le. 0)then
C               zero or negative number of character requested
                return
        else if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        else if (fchar .lt. 1)then
C               error: illegal starting character
                status=308
                return
        end if
        
C       move the i/o pointer to the start of the sequence of characters
        bstart=dtstrt(ibuff)+(frow-1)*rowlen(ibuff)+fchar-1
        call ftmbyt(iunit,bstart,.true.,status)

C       put the string of characters
        call ftpcbf(iunit,1,nchars,svalue,status)
        end
