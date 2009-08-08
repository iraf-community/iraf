C----------------------------------------------------------------------
        subroutine ftpclx(iunit,colnum,frow,fbit,nbit,lray,status)

C       write an array of logical values to a specified bit or byte
C       column of the binary table.   If the LRAY parameter is .true.,
C       then the corresponding bit is set to 1, otherwise it is set
C       to 0.
C       The binary table column being written to must have datatype 'B'
C       or 'X'. 

C       iunit   i  fortran unit number
C       colnum  i  number of the column to write to
C       frow    i  first row to write
C       fbit    i  first bit within the row to write
C       nbit    i  number of bits to write
C       lray    l  array of logical data values corresponding to the bits
C                        to be written
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Mar 1992
C       modified by Wm Pence May 1992 to remove call to system dependent
C                                     bit testing and setting routines.

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

        integer bstart,offset,tcode,fbyte,bitloc,ndone,tstat
        integer ibuff,i,ntodo,repeat,rstart,estart,buffer
        logical descrp,wrbit(8),setbit(8)
        character*1 cbuff
        character crow*9

        if (status .gt. 0)return

        ibuff=bufnum(iunit)
        tcode=tdtype(colnum+tstart(ibuff))

C       check input parameters
        if (nbit .le. 0)then
                return
        else if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                write(crow,2000)frow
2000            format(i9)
                call ftpmsg('Starting row number for table write '//
     &          'request is out of range:'//crow//' (FTPCLX).')
                return
        else if (fbit .lt. 1)then
C               illegal element number
                status=308
                write(crow,2000)fbit
                call ftpmsg('Starting element number for write '//
     &          'request is out of range:'//crow//' (FTPCLX).')
                return
        end if

        fbyte=(fbit+7)/8
        bitloc=fbit-(fbit-1)/8*8
        ndone=0
        ntodo=nbit
        rstart=frow-1
        estart=fbyte-1

        if (tcode .eq. 11)then
                descrp=.false.
C               N.B: REPEAT is the number of bytes, not number of bits
                repeat=trept(colnum+tstart(ibuff))
                if (fbyte .gt. repeat)then
C                               illegal element number
                                status=308
                                write(crow,2000)fbit
                    call ftpmsg('Starting element number for write '//
     &              'request is out of range:'//crow//' (FTPCLX).')
                                return
                end if
C               calc the i/o pointer location to start of sequence of pixels
                bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &          tbcol(colnum+tstart(ibuff))+estart
        else if (tcode .eq. -11)then
C               this is a variable length descriptor column
                descrp=.true.
C               only bit arrays (tform = 'X') are supported for variable
C               length arrays.  REPEAT is the number of BITS in the array.        
                repeat=estart+ntodo
                offset=nxheap(ibuff)
C               write the number of elements and the starting offset:
                call ftpdes(iunit,colnum,frow,repeat,
     &                              offset,status)
C               calc the i/o pointer location to start of sequence of pixels
                bstart=dtstrt(ibuff)+offset+
     &                          theap(ibuff)+estart
C               increment the empty heap starting address (in bytes):
                repeat=(repeat+7)/8
                nxheap(ibuff)=nxheap(ibuff)+repeat
        else
C               column must be byte or bit data type
                status=310
                return
        end if

C       move the i/o pointer to the start of the pixel sequence
        call ftmbyt(iunit,bstart,.true.,status)
        tstat=0

C       read the next byte (we may only be modifying some of the bits)
20      call ftgcbf(iunit,0,1,cbuff,status)
        if (status .eq. 107)then
C            hit end of file trying to read the byte, so just set byte = 0
             status=tstat
             cbuff=char(0)
        end if

        buffer=ichar(cbuff)
        if (buffer .lt. 0)buffer=buffer+256
C       move back, to be able to overwrite the byte
        call ftmbyt(iunit,bstart,.true.,status)

C       reset flags indicating which bits are to be set
        wrbit(1)=.false.
        wrbit(2)=.false.
        wrbit(3)=.false.
        wrbit(4)=.false.
        wrbit(5)=.false.
        wrbit(6)=.false.
        wrbit(7)=.false.
        wrbit(8)=.false.

C       flag the bits that are to be set 
        do 10 i=bitloc,8
                wrbit(i)=.true.
                ndone=ndone+1
                if(lray(ndone))then
                        setbit(i)=.true.
                else
                        setbit(i)=.false.
                end if
                if (ndone .eq. ntodo)go to 100
10      continue

C       set or reset the bits within the byte
        call ftpbit(setbit,wrbit,buffer)

C       write the new byte
        cbuff=char(buffer)
        call ftpcbf(iunit,0,1,cbuff,status)
        
C       not done, so get the next byte
        bstart=bstart+1
        if (.not. descrp)then
                estart=estart+1
                if (estart .eq. repeat)then
C                       move the i/o pointer to the next row of pixels
                        estart=0
                        rstart=rstart+1
                        bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &                         tbcol(colnum+tstart(ibuff))+estart
                        call ftmbyt(iunit,bstart,.true.,status)
                end if
        end if
        bitloc=1
        go to 20

100     continue
C       set or reset the bits within the byte
        call ftpbit(setbit,wrbit,buffer)

C       write the new byte
        cbuff=char(buffer)
        call ftpcbf(iunit,0,1,cbuff,status)
        end
