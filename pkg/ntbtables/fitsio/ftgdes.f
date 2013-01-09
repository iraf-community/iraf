C----------------------------------------------------------------------
        subroutine ftgdes(iunit,colnum,rownum,nelem,offset,status)

C       read the descriptor values from a binary table.  This is only
C       used for column which have TFORMn = 'P', i.e., for variable
C       length arrays.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       rownum  i  number of the row to read
C       nelem   i  output number of elements
C       offset  i  output byte offset of the first element
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Nov 1991

        integer iunit,colnum,rownum,nelem,offset,status

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

        ibuff=bufnum(iunit)

C       check that this is really a 'P' type column
        if (tdtype(colnum+tstart(ibuff)) .ge. 0)then
                status=317
                return
        end if

C       move to the specified column and row:
        bstart=dtstrt(ibuff)+(rownum-1)*rowlen(ibuff)
     &         +tbcol(colnum+tstart(ibuff))
        call ftmbyt(iunit,bstart,.true.,status)

C       now read the number of elements and the offset to the table:
        call ftgi4b(iunit,2,0,iray,status)
        nelem=iray(1)
        offset=iray(2)
        end
