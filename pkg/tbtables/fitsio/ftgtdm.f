C----------------------------------------------------------------------
        subroutine ftgtdm(iunit,colnum,maxdim,naxis,naxes,status)

C       parse the TDIMnnn keyword to get the dimensionality of a column

C       iunit   i  fortran unit number to use for reading
C       colnum  i  column number to read
C       maxdim  i  maximum no. of dimensions to read; dimension of naxes
C       OUTPUT PARAMETERS:
C       naxis   i  number of axes in the data array
C       naxes   i  array giving the length of each data axis
C       status  i  output error status (0=OK)
C
C       written by Wm Pence, HEASARC/GSFC, October 1993

        integer iunit,colnum,maxdim,naxis,naxes(*),status

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
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
C-------END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,nfound,c1,c2,clast,dimval
        logical last
        character*120 tdim

        if (status .gt. 0)return

C       define the number of the buffer used for this file
        ibuff=bufnum(iunit)

        if (colnum .lt. 1 .or. colnum .gt. tfield(ibuff))then
C               illegal column number
                status=302
                return
        end if

        nfound=0
C       try getting the TDIM keyword value
        call ftgkns(iunit,'TDIM',colnum,1,tdim,nfound,status)

        if (nfound .ne. 1)then
C               no TDIM keyword found
                naxis=1
                naxes(1)=trept(colnum+tstart(ibuff))
                return
        end if

        naxis=0
C       first, find the opening ( and closing )
        c1=index(tdim,'(')+1
        c2=index(tdim,')')-1
        if (c1 .eq. 1 .or. c2 .eq. -1)go to 900

        last=.false.
C       find first non-blank character
10      if (tdim(c1:c1) .ne. ' ')go to 20
        c1=c1+1
        go to 10

C       find the comma separating the dimension sizes
20      clast=index(tdim(c1:c2),',')+c1-2
        if (clast .eq. c1-2)then
                last=.true.
                clast=c2
        end if

C       read the string of characters as the (integer) dimension size
        call ftc2ii(tdim(c1:clast),dimval,status)
        if (status .gt. 0)then
             call ftpmsg('Error in FTGTDM parsing dimension string: '
     &       //tdim)
             go to 900
        end if

        naxis=naxis+1
        if (naxis .le. maxdim)naxes(naxis)=dimval

        if (last)return

        c1=clast+2
        go to 10

C       could not parse the tdim value
900     status=263
        end
