C--------------------------------------------------------------------------
        subroutine ftrwup(iunit,frow,lrow,nshift,status)

C       shift rows in a table up by NROWS rows, overwriting the rows above

C       iunit   i  Fortran I/O unit number
C       frow    i  first row to be moved up
C       lrow    i  last row to be moved up (last row of the table)
C       nshift  i  how far to shift the rows (number of rows)
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
        character*1 buff(5760)
        common/ftheap/buff
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,kshift,nchar,fchar,i,j
        integer lstptr,inptr,outptr,nseg
        character cfill*1

        if (status .gt. 0)return

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

C **********************************************************************
C       CASE #1: One or more rows of the table will fit in the work buffer,
C **********************************************************************
        if (rowlen(ibuff) .le. 5760)then

C       how many rows can we move at one time?
        kshift=5760/rowlen(ibuff)
        fchar=1

C       check if we just need to clear the last NSHIFT rows of the table
        if (frow .eq. lrow+1)go to 25

C       initialize pointers
        inptr=frow
        lstptr=inptr+kshift

20      if (lstptr .gt. lrow)lstptr=lrow
        nchar=(lstptr-inptr+1)*rowlen(ibuff)
        outptr=inptr-nshift

C       read the row(s) to be shifted
        call ftgtbs(iunit,inptr,fchar,nchar,buff,status)

C       write the row(s) to the new location
        call ftptbs(iunit,outptr,fchar,nchar,buff,status)

C       If there are more rows, update pointers and repeat
        if (lstptr .lt. lrow)then
            inptr =inptr +kshift
            lstptr=lstptr+kshift
            go to 20
        end if

C       initialize the buffer with the fill value
25      continue
        do 30 i=1,5760
            buff(i)=cfill
30      continue

C       fill the empty rows at the bottom of the table with blanks or nulls
        nchar=rowlen(ibuff)
        do 35 i=1,nshift
            outptr=lrow-nshift+i
            call ftptbs(iunit,outptr,fchar,nchar,buff,status)
35      continue
        return

C **********************************************************************
C       CASE #2:  Cannot fit a whole row into the work buffer, so have
C       to move each row in pieces.   
C **********************************************************************
        else

        nseg=(rowlen(ibuff)+5759)/5760
        nchar=5760

        do 60 j=1,nseg
            fchar=(j-1)*5760+1
            if (j .eq. nseg)nchar=rowlen(ibuff)-(nseg-1)*5760

C           check if we just need to clear the last NSHIFT rows of the table
            if (frow .eq. lrow+1)go to 45

            do 40 i=frow,lrow
C               read the row to be shifted
                call ftgtbs(iunit,i,fchar,nchar,buff,status)

C               write the row(s) to the new location
                call ftptbs(iunit,i-nshift,fchar,nchar,buff,status)
40          continue

C           initialize the buffer with the fill value
45          continue
            do 50 i=1,5760
                buff(i)=cfill
50          continue

C           fill the empty rows with blanks or nulls
            do 55 i=1,nshift
                outptr=lrow-nshift+i
                call ftptbs(iunit,outptr,fchar,nchar,buff,status)
55          continue
60      continue
        end if
        end
