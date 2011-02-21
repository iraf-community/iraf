C--------------------------------------------------------------------------
        subroutine ftgacl(iunit,colnum,xtype,xbcol,xunit,xform,
     &        xscal,xzero,xnull,xdisp,status)

C       Get information about an Ascii CoLumn
C       returns the parameters which define the column

C       iunit   i  Fortran i/o unit number
C       colnum  i  number of the column (first column = 1)
C       xtype   c  name of the column
C       xbcol   i  starting character in the row of the column
C       xunit   c  physical units of the column
C       xform   c  Fortran-77 format of the column
C       xscal   d  scaling factor for the column values
C       xzero   d  scaling zero point for the column values
C       xnull   c  value used to represent undefined values in the column
C       xdisp   c  display format for the column (if different from xform
C       status  i  returned error status

        integer iunit,colnum,xbcol,status
        double precision xscal,xzero
        character*(*) xtype,xunit,xform,xnull,xdisp

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
        character cnull*16, cform*8
        common/ft0003/cnull(nf),cform(nf)
C       END OF COMMON BLOCK DEFINITIONS------------------------------------

        integer ibuff,nfound

        if (status .gt. 0)return

        if (colnum .lt. 1 .or. colnum .gt. 999)then
C               illegal column number
                status=302
                return
        end if

        ibuff=bufnum(iunit)

C       get the parameters which are stored in the common block
        xbcol=tbcol(colnum+tstart(ibuff))+1
        xform=cform(colnum+tstart(ibuff))
        xscal=tscale(colnum+tstart(ibuff))
        xzero=tzero(colnum+tstart(ibuff))
        xnull=cnull(colnum+tstart(ibuff))

C       read remaining values from the header keywords
        xtype=' '
        call ftgkns(iunit,'TTYPE',colnum,1,xtype,nfound,status)
        xunit=' '
        call ftgkns(iunit,'TUNIT',colnum,1,xunit,nfound,status)
        xdisp=' '
        call ftgkns(iunit,'TDISP',colnum,1,xdisp,nfound,status)
        end        
