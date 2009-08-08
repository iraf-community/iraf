C--------------------------------------------------------------------------
        subroutine ftcdel(iunit,naxis1,naxis2,delbyt,fstbyt,status)

C       delete a specified column by shifting the rows

C       iunit   i  Fortran I/O unit number
C       naxis1  i  width in bytes of existing table
C       naxis2  i  number of rows in the table
C       delbyt  i  how many bytes to delete in each row
C       fstbyt  i  byte position in the row to delete the bytes (0=row start)
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

        integer ibuff,i,i1,i2,irow,newlen,nseg,nbytes,remain

        if (status .gt. 0)return

C       define the number of the buffer used for this file
        ibuff=bufnum(iunit)

        newlen=naxis1-delbyt

        if (newlen .le. 5760)then
C ***********************************************************************
C       CASE #1: optimal case where whole new row fits in the work buffer
C ***********************************************************************
            i1=fstbyt+1
            i2=i1+delbyt
            do 10 irow=1,naxis2-1
C               read the row to be shifted 
                call ftgtbs(iunit,irow,i2,newlen,buff,status)

C               set row length to its new value
                rowlen(ibuff)=newlen

C               write the row in the new place
                call ftptbs(iunit,irow,i1,newlen,buff,status)

C               reset row length to its original value
                rowlen(ibuff)=naxis1
10          continue

C           now do the last row
            remain=naxis1-(fstbyt+delbyt)
            if (remain .gt. 0)then
C               read the row to be shifted 
                call ftgtbs(iunit,naxis2,i2,remain,buff,status)

C               set row length to its new value
                rowlen(ibuff)=newlen

C               write the row in the new place
                call ftptbs(iunit,naxis2,i1,remain,buff,status)

C               reset row length to its original value
                rowlen(ibuff)=naxis1
            end if
        else
C ************************************************************************
C       CASE #2:  whole row doesn't fit in work buffer; move row in pieces
C ************************************************************************
            nseg=(newlen+5759)/5760

            do 40 irow=1,naxis2-1
                i1=fstbyt+1
                i2=i1+delbyt
                nbytes=newlen-(nseg-1)*5760

                do 30 i=1,nseg
C                   read the row to be shifted
                    call ftgtbs(iunit,irow,i2,nbytes,buff,status)

C                   set row length to its new value
                    rowlen(ibuff)=newlen

C                   write the row in the new place
                    call ftptbs(iunit,irow,i1,nbytes,buff,status)

C                   reset row length to its original value
                    rowlen(ibuff)=naxis1

                    i1=i1+nbytes
                    i2=i2+nbytes
                    nbytes=5760
30              continue
40          continue

C           now do the last row
            remain=naxis1-(fstbyt+delbyt)
            if (remain .gt. 0)then
                nseg=(remain+5759)/5760
                i1=fstbyt+1
                i2=i1+delbyt
                nbytes=remain-(nseg-1)*5760

                do 50 i=1,nseg
C                   read the row to be shifted 
                    call ftgtbs(iunit,naxis2,i2,nbytes,buff,status)

C                   set row length to its new value
                    rowlen(ibuff)=newlen

C                   write the row in the new place
                    call ftptbs(iunit,naxis2,i1,nbytes,buff,status)

C                   reset row length to its original value
                    rowlen(ibuff)=naxis1

                    i1=i1+nbytes
                    i2=i2+nbytes
                    nbytes=5760
50              continue
            end if
        end if
        end
