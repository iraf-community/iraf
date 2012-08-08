C----------------------------------------------------------------------
        subroutine ftgcx(iunit,colnum,frow,fbit,nbit,lray,status)

C       read an array of logical values from a specified bit or byte
C       column of the binary table.  A logical .true. value is returned
C       if the corresponding bit is 1, and a logical .false. value is
C       returned if the bit is 0.
C       The binary table column being read from must have datatype 'B'
C       or 'X'. This routine ignores any undefined values in the 'B' array.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       fbit    i  first bit within the row to read
C       nbit    i  number of bits to read
C       lray    l  returned array of logical data values that is read
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Mar 1992

        integer iunit,colnum,frow,fbit,nbit,status
        logical lray(*)

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

        integer bstart,offset,tcode,fbyte,bitloc,ndone
        integer ibuff,i,ntodo,repeat,rstart,estart,buffer
        logical descrp,log8(8)
        character*1 cbuff
      
        if (status .gt. 0)return

        ibuff=bufnum(iunit)
        tcode=tdtype(colnum+tstart(ibuff))

C       check input parameters
        if (nbit .le. 0)then
                return
        else if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        else if (fbit .lt. 1)then
C               illegal element number
                status=308
                return
        end if

        fbyte=(fbit+7)/8
        bitloc=fbit-(fbit-1)/8*8
        ndone=0
        ntodo=nbit
        rstart=frow-1
        estart=fbyte-1

        if (tcode .eq. 11)then
                repeat=trept(colnum+tstart(ibuff))
                if (fbyte .gt. repeat)then
C                       illegal element number
                        status=308
                        return
                end if
                descrp=.false.
C               move the i/o pointer to the start of the sequence of pixels
                bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &          tbcol(colnum+tstart(ibuff))+estart
        else if (tcode .eq. -11)then
C               this is a variable length descriptor column
                descrp=.true.
C               read the number of elements and the starting offset:
                call ftgdes(iunit,colnum,frow,repeat,
     &                              offset,status)
                repeat=(repeat+7)/8
                if (repeat .eq. 0)then
C                       error: null length vector
                        status=318
                        return
                else if ((fbit+nbit+6)/8 .gt. repeat)then
C                       error: trying to read beyond end of record
                        status=319
                        return
                end if
                bstart=dtstrt(ibuff)+offset+
     &                          theap(ibuff)+estart
        else
C               column must be byte or bit data type
                status=312
                return
        end if

C       move the i/o pointer to the start of the pixel sequence
        call ftmbyt(iunit,bstart,.false.,status)

C       get the next byte
20      call ftgcbf(iunit,0,1,cbuff,status)
        buffer=ichar(cbuff)
        if (buffer .lt. 0)buffer=buffer+256

C       decode the bits within the byte into an array of logical values
        call ftgbit(buffer,log8)

        do 10 i=bitloc,8
                ndone=ndone+1
                lray(ndone)=log8(i)
                if (ndone .eq. ntodo)go to 100
10      continue
        
C       not done, so get the next byte
        if (.not. descrp)then
                estart=estart+1
                if (estart .eq. repeat)then
C                       move the i/o pointer to the next row of pixels
                        estart=0
                        rstart=rstart+1
                        bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &                         tbcol(colnum+tstart(ibuff))+estart
                        call ftmbyt(iunit,bstart,.false.,status)
                end if
        end if
        bitloc=1
        go to 20

100     continue
        end
