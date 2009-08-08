C----------------------------------------------------------------------
        subroutine ftgcxi(iunit,colnum,frow,nrow,fbit,nbit,
     &                    ivalue,status)

C       read any consecutive bits from an 'X' or 'B' column as an unsigned
C       n-bit integer, unless nbits=16 in which case the 16 bits
C       are interpreted as a 16-bit signed 2s complement word

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       nrow    i  number of rows to read
C       fbit    i  first bit within the row to read
C       nbit    i  number of bits to read
C       ivalue  i*2  returned integer value(s)
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

        integer iunit,colnum,fbit,nbit,frow,nrow,status,i,j,k,row,ibuff
        integer*2 ivalue(*),ival,power2(16)
        logical lray(16)
        save power2
        data power2/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,
     &  16384,0/

        if (status .gt. 0)return

        ibuff=bufnum(iunit)

        if (nbit .gt. 16)then
            call ftpmsg('Cannot read more than 16 bits (ftgcxi)')
            status=308
            return
        else if ((fbit+nbit+6)/8 .gt. trept(colnum+tstart(ibuff)))then
            call ftpmsg('Asked to read more bits than exist in'//
     &      ' the column (ftgcxi)')
            status=308
            return
        end if


        row=frow-1
        do 30 k=1,nrow
            row=row+1
C           get the individual bits
            call ftgcx(iunit,colnum,row,fbit,nbit,lray,status)
            if (status .gt. 0)return
            ival=0
            j=0
            if (nbit .eq. 16 .and. lray(1))then
C               interprete this as a 16 bit negative integer
                do 10 i=16,2,-1
                    j=j+1
                    if (.not. lray(i))ival=ival+power2(j)
10              continue
C               make 2's complement
                ivalue(k)=-ival-1
            else
C               reconstruct the positive integer value
                do 20 i=nbit,1,-1
                    j=j+1
                    if (lray(i))ival=ival+power2(j)
20              continue
                ivalue(k)=ival
            end if
30      continue
        end
