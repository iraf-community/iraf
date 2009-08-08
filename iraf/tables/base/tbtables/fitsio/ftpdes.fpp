C----------------------------------------------------------------------
        subroutine ftpdes(ounit,colnum,rownum,nelem,offset,status)

C       write the descriptor values to a binary table.  This is only
C       used for column which have TFORMn = 'P', i.e., for variable
C       length arrays.

C       ounit   i  fortran unit number
C       colnum  i  number of the column to write to
C       rownum  i  number of the row to write
C       nelem   i  input number of elements
C       offset  i  input byte offset of the first element
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Nov 1991

        integer ounit,colnum,rownum,nelem,offset,status

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

        integer ibuff,bstart,iray(2)

        if (status .gt. 0)return
        if (rownum .lt. 1)then
C               error: illegal row number
                status=307
                return
        end if

        ibuff=bufnum(ounit)

C       check that this is really a 'P' type column
        if (tdtype(colnum+tstart(ibuff)) .ge. 0)then
                status=317
                return
        end if

C       move to the specified column and row:
        bstart=dtstrt(ibuff)+(rownum-1)*rowlen(ibuff)
     &         +tbcol(colnum+tstart(ibuff))
        call ftmbyt(ounit,bstart,.true.,status)

C       now write the number of elements and the offset to the table:
        iray(1)=nelem
        iray(2)=offset
        call ftpi4b(ounit,2,0,iray,status)
        end
