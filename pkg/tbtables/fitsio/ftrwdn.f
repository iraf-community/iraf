C--------------------------------------------------------------------------
        subroutine ftrwdn(iunit,frow,lrow,nshift,status)

C       shift rows in a table down by NROWS rows, inserting blank rows

C       iunit   i  Fortran I/O unit number
C       frow    i  rows *AFTER* this one are to be moved down
C       lrow    i  last row to be moved down (last row of the table)
C       nshift  i  how far to shift the rows
C       status  i  returned error status (0=ok)

        integer iunit,frow,lrow,nshift,status

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
        character*1 buff(2880,2)
        common/ftheap/buff
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,kshift,nchar,fchar,in,out,i,j,irow,tin,jrow
        integer lstptr,inptr,outptr,nseg
        character cfill*1
        
        if (status .gt. 0)return

C       don't have to do anything if inserting blank rows at end of the table
        if (frow .eq. lrow)return

C       define the number of the buffer used for this file
        ibuff=bufnum(iunit)

C       select appropriate fill value
        if (hdutyp(ibuff) .eq. 1)then
C           fill  header or ASCII table with space
            cfill=char(32)
        else
C           fill image or bintable data area with Null (0)      
            cfill=char(0)
        end if

C       how many rows will fit in the single buffer?
        kshift=2880/rowlen(ibuff)

C **********************************************************************
C       CASE #1: optimal case where the NSHIFT number of rows will all
C       fit in the 2880-byte work buffer simultaneously.  The rows can
C       be shifted down in one efficient pass through the table.
C **********************************************************************
        if (kshift .ge. nshift)then
    
        kshift=nshift
        nchar=kshift*rowlen(ibuff)
        fchar=1

C       initialize the first buffer
        in=2
        out=1

        do 5 i=1,2880
            buff(i,1)=cfill
5       continue

        do 10 irow=frow+1,lrow,kshift

C           read the row(s) to be shifted
            call ftgtbs(iunit,irow,fchar,nchar,buff(1,in),status)

C           overwrite these row(s) with the previous row(s)
            call ftptbs(iunit,irow,fchar,nchar,buff(1,out),status)

C           swap the input and output buffer pointers and move to next rows
            tin=in
            in=out
            out=tin
            jrow=irow
10      continue

C       write the last row(s) out
        irow=jrow+kshift
        nchar=(lrow-jrow+1)*rowlen(ibuff)
        
        call ftptbs(iunit,irow,fchar,nchar,buff(1,out),status)
        return

C **********************************************************************
C       CASE #2: One or more rows of the table will fit in the work buffer,
C       but cannot fit all NSHIFT rows in the buffer at once.  Note that
C       since we do not need 2 buffers, as in the previous case, we can
C       combine both buffers into one single 2880*2 byte buffer, to handle
C       wider tables.  This algorithm copies then moves blocks of contiguous
C       rows at one time, working upwards from the bottom of the table.
C **********************************************************************
        else if (rowlen(ibuff) .le. 5760)then

C       how many rows can we move at one time?
        kshift=5760/rowlen(ibuff)
        fchar=1

C       initialize pointers
        lstptr=lrow
        inptr=lrow-kshift+1

20      if (inptr .le. frow)inptr=frow+1
        nchar=(lstptr-inptr+1)*rowlen(ibuff)
        outptr=inptr+nshift

C       read the row(s) to be shifted
        call ftgtbs(iunit,inptr,fchar,nchar,buff,status)

C       write the row(s) to the new location
        call ftptbs(iunit,outptr,fchar,nchar,buff,status)

C       If there are more rows, update pointers and repeat
        if (inptr .gt. frow+1)then
            lstptr=lstptr-kshift
            inptr =inptr -kshift
            go to 20
        end if

C       initialize the buffer with the fill value
        do 25 i=1,2880
            buff(i,1)=cfill
            buff(i,2)=cfill
25      continue

C       fill the empty rows with blanks or nulls
        nchar=rowlen(ibuff)
        do 30 i=1,nshift
            outptr=frow+i
            call ftptbs(iunit,outptr,fchar,nchar,buff,status)
30      continue
        return

C **********************************************************************
C       CASE #3:  Cannot fit a whole row into the work buffer, so have
C       to move each row in pieces.   
C **********************************************************************
        else

        nseg=(rowlen(ibuff)+5759)/5760
        nchar=5760

        do 60 j=1,nseg
            fchar=(j-1)*5760+1
            if (j .eq. nseg)nchar=rowlen(ibuff)-(nseg-1)*5760

            do 40 i=lrow,frow+1,-1
C               read the row to be shifted
                call ftgtbs(iunit,i,fchar,nchar,buff,status)

C               write the row(s) to the new location
                call ftptbs(iunit,i+nshift,fchar,nchar,buff,status)
40          continue

C           initialize the buffer with the fill value
            do 45 i=1,2880
                buff(i,1)=cfill
                buff(i,2)=cfill
45          continue

C           fill the empty rows with blanks or nulls
            do 50 i=1,nshift
                outptr=frow+i
                call ftptbs(iunit,outptr,fchar,nchar,buff,status)
50          continue
60      continue

        end if
        end
