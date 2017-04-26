C----------------------------------------------------------------------
        subroutine ftgcxd(iunit,colnum,frow,nrow,fbit,nbit,
     &             dvalue,status)

C       read any consecutive bits from an 'X' or 'B' column as an unsigned
C       n-bit integer

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       nrow    i  number of rows to read
C       fbit    i  first bit within the row to read
C       nbit    i  number of bits to read
C       dvalue  d  returned value(s)
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Nov 1994

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

        integer iunit,colnum,fbit,nbit,frow,nrow,status
        integer i,k,istart,itodo,ntodo,row,ibuff
        double precision dvalue(*),power,dval
        logical lray(64)

        if (status .gt. 0)return

        ibuff=bufnum(iunit)
        if ((fbit+nbit+6)/8 .gt. trept(colnum+tstart(ibuff)))then
            call ftpmsg('Asked to read more bits than exist in'//
     &      ' the column (ftgcxd)')
            status=308
            return
        end if

        row=frow-1
        do 30 k=1,nrow
            row=row+1
            dval=0.
            power=1.0D+00
            istart=fbit+nbit
            ntodo=nbit

10          itodo=min(ntodo,64)
            istart=istart-itodo

C           read up to 64 bits at a time
C           get the individual bits
            call ftgcx(iunit,colnum,row,istart,itodo,lray,status)
            if (status .gt. 0)return

C           reconstruct the positive integer value
            do 20 i=itodo,1,-1
                if (lray(i))dval=dval+power
                power=power*2.0D+00
20          continue

            ntodo=ntodo-itodo
            if (itodo .gt. 0)go to 10
            dvalue(k)=dval
30      continue
        end
