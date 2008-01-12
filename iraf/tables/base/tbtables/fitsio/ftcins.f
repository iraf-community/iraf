C--------------------------------------------------------------------------
        subroutine ftcins(iunit,naxis1,naxis2,delbyt,fstbyt,status)

C       insert DELBYT bytes after byte fstbyt in every row of the table

C       iunit   i  Fortran I/O unit number
C       naxis1  i  width in bytes of existing table
C       naxis2  i  number of rows in the table
C       delbyt  i  how many bytes to insert in each row
C       fstbyt  i  byte position in the row to insert the bytes (0=row start)
C       status  i  returned error status (0=ok)

        integer iunit,naxis1,naxis2,delbyt,fstbyt,status

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

        integer ibuff,i,i1,irow,newlen,fbyte,nseg,nbytes
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

        newlen=naxis1+delbyt

        if (newlen .le. 5760)then
C ***********************************************************************
C       CASE #1: optimal case where whole new row fits in the work buffer
C ***********************************************************************
C           write the correct fill value into the buffer
            do 10 i=1,delbyt
                buff(i)=cfill
10          continue
            i1=delbyt+1

C           first move the trailing bytes (if any) in the last row
            fbyte=fstbyt+1
            nbytes=naxis1-fstbyt
            call ftgtbs(iunit,naxis2,fbyte,nbytes,buff(i1),status)

C           set row length to its new value
            rowlen(ibuff)=newlen

C           write the row (with leading fill bytes) in the new place
            nbytes=nbytes+delbyt
            call ftptbs(iunit,naxis2,fbyte,nbytes,buff,status)

C           reset row length to its original value
            rowlen(ibuff)=naxis1

C           now move the rest of the rows
            do 20 irow=naxis2-1,1,-1
C               read the row to be shifted (work backwards through the table)
                call ftgtbs(iunit,irow,fbyte,naxis1,buff(i1),status)

C               set row length to its new value
                rowlen(ibuff)=newlen

C               write the row (with the leading fill bytes) in the new place
                call ftptbs(iunit,irow,fbyte,newlen,buff,status)

C               reset row length to its original value
                rowlen(ibuff)=naxis1
20          continue

        else
C ************************************************************************
C       CASE #2:  whole row doesn't fit in work buffer; move row in pieces
C ************************************************************************
C           first copy the data, then go back and write fill into the new column
C           start by copying the trailing bytes (if any) in the last row

            nbytes=naxis1-fstbyt
            nseg=(nbytes+5759)/5760
            fbyte=(nseg-1)*5760+fstbyt+1
            nbytes=naxis1-fbyte+1

            do 25 i=1,nseg
                call ftgtbs(iunit,naxis2,fbyte,nbytes,buff,status)

C               set row length to its new value
                rowlen(ibuff)=newlen

C               write the row in the new place
                call ftptbs(iunit,naxis2,fbyte+delbyt,nbytes,
     &                      buff,status)

C               reset row length to its original value
                rowlen(ibuff)=naxis1
                          
                fbyte=fbyte-5760
                nbytes=5760
25          continue

C           now move the rest of the rows
            nseg=(naxis1+5759)/5760

            do 40 irow=naxis2-1,1,-1
                fbyte=(nseg-1)*5760+fstbyt+1
                nbytes=naxis1-(nseg-1)*5760
                do 30 i=1,nseg
C                   read the row to be shifted (work backwards thru the table)
                    call ftgtbs(iunit,irow,fbyte,nbytes,buff,status)

C                   set row length to its new value
                    rowlen(ibuff)=newlen

C                   write the row in the new place
                    call ftptbs(iunit,irow,fbyte+delbyt,nbytes,
     &                          buff,status)

C                   reset row length to its original value
                    rowlen(ibuff)=naxis1

                    fbyte=fbyte-5760
                    nbytes=5760
30              continue
40          continue

C           now write the fill values into the new column
            nbytes=min(delbyt,5760)
            do 50 i=1,nbytes
                    buff(i)=cfill
50          continue

            nseg=(delbyt+5759)/5760

C           set row length to its new value
            rowlen(ibuff)=newlen

            do 70 irow=1,naxis2
                fbyte=fstbyt+1
                nbytes=delbyt-((nseg-1)*5760)
                do 60 i=1,nseg
C                   write the fill
                    call ftptbs(iunit,irow,fbyte,nbytes,buff,status)
                    fbyte=fbyte+nbytes
                    nbytes=5760
60              continue
70          continue

C           reset the rowlength
            rowlen(ibuff)=naxis1
        end if
        end
