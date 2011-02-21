C----------------------------------------------------------------------
        subroutine ftgcxj(iunit,colnum,frow,nrow,fbit,nbit,
     &             jvalue,status)

C       read any consecutive bits from an 'X' or 'B' column as an unsigned
C       n-bit integer, unless nbits=32 in which case the 32 bits
C       are interpreted as a 32-bit signed 2s complement word

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       nrow    i  number of rows to read
C       fbit    i  first bit within the row to read
C       nbit    i  number of bits to read
C       jvalue  i  returned integer value(s)
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

        integer iunit,colnum,fbit,nbit,frow,nrow,status,i,j,k,row,jval
        integer jvalue(*),power2(32),ibuff
        logical lray(32)
        save power2
        data power2/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,
     &  16384,32768,65536,131072,262144,524288,1048576,2097152,4194304,
     &  8388608,16777216,33554432,67108864,134217728,268435456,536870912
     &  ,1073741824,0/

        if (status .gt. 0)return

        ibuff=bufnum(iunit)

        if (nbit .gt. 32)then
            call ftpmsg('Cannot read more than 32 bits (ftgcxj)')
            status=308
            return
        else if ((fbit+nbit+6)/8 .gt. trept(colnum+tstart(ibuff)))then
            call ftpmsg('Asked to read more bits than exist in'//
     &      ' the column (ftgcxj)')
            status=308
            return
        end if

        row=frow-1
        do 30 k=1,nrow
            row=row+1
C           get the individual bits
            call ftgcx(iunit,colnum,row,fbit,nbit,lray,status)
            if (status .gt. 0)return

            jval=0
            j=0
            if (nbit .eq. 32 .and. lray(1))then
C               interprete this as a 32 bit negative integer
                do 10 i=32,2,-1
                    j=j+1
                    if (.not. lray(i))jval=jval+power2(j)
10              continue
C               make 2's complement
                jvalue(k)=-jval-1
            else
C               reconstruct the positive integer value
                do 20 i=nbit,1,-1
                    j=j+1
                    if (lray(i))jval=jval+power2(j)
20              continue
                jvalue(k)=jval
            end if
30      continue
        end
