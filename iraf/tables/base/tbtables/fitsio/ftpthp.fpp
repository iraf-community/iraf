C--------------------------------------------------------------------------
        subroutine ftpthp(ounit,heap,status)

C       Define the starting address for the heap for a binary table.
C       The default address is NAXIS1 * NAXIS2.  It is in units of
C       bytes relative to the beginning of the regular binary table data.
C       This subroutine also writes the appropriate THEAP keyword to the
C       FITS header.

C       ounit   i  Fortran I/O unit number
C       heap   i  starting address of the heap
C       OUTPUT PARAMETERS:
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, Nov 1991

        integer ounit,heap,status

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
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
C-------END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff

        if (status .gt. 0)return
        ibuff=bufnum(ounit)
        theap(ibuff)=heap        

C       write the keyword
        call ftukyj(ounit,'THEAP',heap,'Byte offset of heap area',
     &              status)
        end
