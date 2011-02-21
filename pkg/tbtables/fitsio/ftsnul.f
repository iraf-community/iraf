C--------------------------------------------------------------------------
        subroutine ftsnul(ounit,colnum,nulval,status)

C       ascii table Column NULl value definition
C       Define the null value for an ASCII table column.
C
C       ounit   i  Fortran I/O unit number
C       colnum  i  number of the column to be defined
C       nulval  c  the string to be use to signify undefined data
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,status
        character*(*) nulval

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
        character cnull*16, cform*8
        common/ft0003/cnull(nf),cform(nf)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------
        
        integer ibuff

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

C       if HDU structure is not defined then scan the header keywords
        if (dtstrt(ibuff) .lt. 0)call ftrdef(ounit,status)
        if (status .gt. 0)return

C       test for proper HDU type
        if (hdutyp(ibuff) .ne. 1)then
            status=226
            return
        end if

        if (colnum .gt. tfield(ibuff) .or. colnum .lt. 1)then
             status=302
             return
        end if

        cnull(colnum+tstart(ibuff))=nulval
        end
