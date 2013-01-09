C--------------------------------------------------------------------------
        subroutine ftgbcl(iunit,colnum,xtype,xunit,dtype,rcount,
     &        xscal,xzero,xnull,xdisp,status)

C       Get information about a Binary table CoLumn
C       returns the parameters which define the column

C       iunit   i  Fortran i/o unit number
C       colnum  i  number of the column (first column = 1)
C       xtype   c  name of the column
C       xunit   c  physical units of the column
C       dtype   c  datatype of the column
C       rcount  i  repeat count of the column
C       xscal   d  scaling factor for the column values
C       xzero   d  scaling zero point for the column values
C       xnull   i  value used to represent undefined values in integer column
C       xdisp   c  display format for the column
C       status  i  returned error status

        integer iunit,colnum,rcount,xnull,status
        double precision xscal,xzero
        character*(*) xtype,xunit,dtype,xdisp

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
C       END OF COMMON BLOCK DEFINITIONS------------------------------------

        integer ibuff,nfound,tcode
        logical descrp
        character ctemp*2,fwide*4

        if (status .gt. 0)return

        if (colnum .lt. 1 .or. colnum .gt. 999)then
C               illegal column number
                status=302
                return
        end if

        ibuff=bufnum(iunit)

C       get the parameters which are stored in the common block
        rcount=trept(colnum+tstart(ibuff))
        xscal=tscale(colnum+tstart(ibuff))
        xzero=tzero(colnum+tstart(ibuff))
        xnull=tnull(colnum+tstart(ibuff))

C       translate the numeric data type code
        dtype=' '
        tcode=tdtype(colnum+tstart(ibuff)) 
        if (tcode .lt. 0)then
                descrp=.true.
                tcode=-tcode
        else
                descrp=.false.
        end if

        if (tcode .eq. 21)then
                dtype='I'
        else if (tcode .eq. 41)then
                dtype='J'
        else if (tcode .eq. 42)then
                dtype='E'
        else if (tcode .eq. 82)then
                dtype='D'
        else if (tcode .eq. 16)then
C               this is an ASCII field; width of field is stored in TNULL
                write(fwide,1000)tnull(colnum+tstart(ibuff))
1000            format(i4)
                if (tnull(colnum+tstart(ibuff)) .gt. 999)then
                    dtype='A'//fwide
                else if (tnull(colnum+tstart(ibuff)) .gt. 99)then
                    dtype='A'//fwide(2:4)
                else if (tnull(colnum+tstart(ibuff)) .gt. 9)then
                    dtype='A'//fwide(3:4)
                else if (tnull(colnum+tstart(ibuff)) .gt. 0)then
                    dtype='A'//fwide(4:4)
                else
                    dtype='A'
                end if
        else if (tcode .eq. 14)then
                dtype='L'
        else if (tcode .eq. 1)then
                dtype='X'
        else if (tcode .eq. 11)then
                dtype='B'
        else if (tcode .eq. 83)then
                dtype='C'
        else if (tcode .eq. 163)then
                dtype='M'
        end if

        if (descrp)then
                ctemp='P'//dtype(1:1)
                dtype=ctemp
        end if

C       read remaining values from the header keywords
        xtype=' '
        call ftgkns(iunit,'TTYPE',colnum,1,xtype,nfound,status)
        xunit=' '
        call ftgkns(iunit,'TUNIT',colnum,1,xunit,nfound,status)
        xdisp=' '
        call ftgkns(iunit,'TDISP',colnum,1,xdisp,nfound,status)
        end        
