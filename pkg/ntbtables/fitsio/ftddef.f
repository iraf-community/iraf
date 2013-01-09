C--------------------------------------------------------------------------
        subroutine ftddef(ounit,bytlen,status)

C       Data DEFinition
C       re-define the length of the data unit
C       this simply redefines the start of the next HDU
C
C       ounit   i  Fortran I/O unit number
C       bytlen  i  new length of the data unit, in bytes
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,bytlen,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne,nf
        parameter (nf = 3000)
        parameter (nb = 20)
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
        
        integer ibuff

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

        if (dtstrt(ibuff) .lt. 0)then
C               freeze the header at its current size
                call fthdef(ounit,0,status)
        end if

        hdstrt(ibuff,chdu(ibuff)+1)=
     &          dtstrt(ibuff)+(bytlen+2879)/2880*2880

C       initialize the fictitious heap starting address (immediately following
C       the array data) and a zero length heap.  This is used to find the
C       end of the data when checking the fill values in the last block. 
        scount(ibuff)=0
        theap(ibuff)=bytlen
        nxheap(ibuff)=0
        end
