C--------------------------------------------------------------------------
        subroutine fttscl(ounit,colnum,bscale,bzero,status)

C       Table column SCaLing factor definition
C       Define the scaling factor for a table column.
C
C       ounit   i  Fortran I/O unit number
C       colnum  i  number of the column to be defined
C       bscale  d  scaling factor
C       bzero   d  scaling zero point
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,status
        double precision bscale,bzero

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------
        
        integer ibuff

        if (status .gt. 0)return

        if (bscale .eq. 0.)then
C               illegal bscale value
                status=322
                return
        end if

        ibuff=bufnum(ounit)

C       if HDU structure is not defined then scan the header keywords
        if (dtstrt(ibuff) .lt. 0)call ftrdef(ounit,status)
        if (status .gt. 0)return

C       test for proper HDU type
        if (hdutyp(ibuff) .eq. 0)then
            status=235
            return
        end if

        if (colnum .gt. tfield(ibuff) .or. colnum .lt. 1)then
             status=302
             return
        end if
            
        tscale(colnum+tstart(ibuff))=bscale
        tzero(colnum+tstart(ibuff))=bzero
        end
