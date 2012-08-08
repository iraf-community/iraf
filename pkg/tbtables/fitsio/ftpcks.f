C----------------------------------------------------------------------
        subroutine ftpcks(iunit,status)

C       Create or update the checksum keywords in the CHU.  These keywords
C       provide a checksum verification of the FITS HDU based on the ASCII
C       coded 1's complement checksum algorithm developed by Rob Seaman at NOAO.

C       iunit   i  fortran unit number
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Sept, 1994

        integer iunit,status

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
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
C-------END OF COMMON BLOCK DEFINITIONS-----------------------------------

        double precision sum,dsum,odsum
        integer ibuff,nrec,dd,mm,yy,dummy,i,tstat
        character datstr*8,string*16,comm*40,oldcks*16,datsum*20
        logical complm

        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       generate current date string to put into the keyword comment
        call ftgsdt(dd,mm,yy,status)
        if (status .gt. 0)return

        datstr='  /  /  '
        write(datstr(1:2),1001)dd
        write(datstr(4:5),1001)mm
        write(datstr(7:8),1001)yy
1001    format(i2)

C       replace blank with leading 0 in each field if required
        if (datstr(1:1) .eq. ' ')datstr(1:1)='0'
        if (datstr(4:4) .eq. ' ')datstr(4:4)='0'
        if (datstr(7:7) .eq. ' ')datstr(7:7)='0'

C       get the checksum keyword, if it exists, otherwise initialize it
        tstat=status
        call ftgkys(iunit,'CHECKSUM',oldcks,comm,status)
        if (status .eq. 202)then
          status=tstat
          oldcks=' '
          comm='encoded HDU checksum updated on '//datstr
          call ftpkys(iunit,'CHECKSUM','0000000000000000',comm,status)
        end if

C       get the DATASUM keyword and convert it to a double precision value
C       if it exists, otherwise initialize it
        tstat=status
        call ftgkys(iunit,'DATASUM',datsum,comm,status)
        if (status .eq. 202)then
          status=tstat
          odsum=0.
C         set the CHECKSUM keyword as undefined
          oldcks=' '
          comm='data unit checksum updated on '//datstr
          call ftpkys(iunit,'DATASUM','         0',comm,status)
        else
C         decode the datasum into a double precision variable
          do 10 i=1,20
            if (datsum(i:i) .ne. ' ')then
                call ftc2dd(datsum(i:20),odsum,status)
                if (status .eq. 409)then
C                   couldn't read the keyword; assume it is out of date
                    status=tstat
                    odsum=-1.
                end if
                go to 15
            end if
10        continue
          odsum=0.
        end if

C       rewrite the header END card, and following blank fill
15      call ftwend(iunit,status)
        if (status .gt. 0)return
 
C       now re-read the required keywords to determine the structure
        call ftrhdu(iunit,dummy,status)

C       write the correct data fill values, if they are not already correct
        call ftpdfl(iunit,status)

C       calc. checksum of the data records; first, calc number of data records
        nrec=(hdstrt(ibuff,chdu(ibuff)+1)-dtstrt(ibuff))/2880
        dsum=0.

        if (nrec .gt. 0)then
C           move to the start of the data
            call ftmbyt(iunit,dtstrt(ibuff),.true.,status)

C           accumulate the 32-bit 1's complement checksum
            call ftcsum(iunit,nrec,dsum,status)
        end if

        if (dsum .ne. odsum)then
C               modify the DATASUM keyword with the correct value 
                comm='data unit checksum updated on '//datstr
C               write the datasum into an I10 integer string
                write(datsum,2000)dsum
2000            format(f11.0)
                call ftmkys(iunit,'DATASUM',datsum(1:10),comm,status)
C               set the CHECKSUM keyword as undefined
                oldcks=' '
        end if

C       if DATASUM was correct, check if CHECKSUM is still OK
        if (oldcks .ne. ' ')then

C           move to the start of the header
            call ftmbyt(iunit,hdstrt(ibuff,chdu(ibuff)),.true.,status)

C           accumulate the header checksum into the previous data checksum
            nrec= (dtstrt(ibuff)-hdstrt(ibuff,chdu(ibuff)))/2880
            sum=dsum
            call ftcsum(iunit,nrec,sum,status)

C           encode the COMPLEMENT of the checksum into a 16-character string
            complm=.true.
            call ftesum(sum,complm,string)

C           return if the checksum is correct
            if (string .eq. '0000000000000000')then
                return
            else if (oldcks .eq. '0000000000000000')then
C               update the CHECKSUM keyword value with the checksum string
                call ftmkys(iunit,'CHECKSUM',string,'&',status)
                return
            end if
        end if

C       Zero the checksum and compute the new value
        comm='encoded HDU checksum updated on '//datstr
        call ftmkys(iunit,'CHECKSUM','0000000000000000',comm,status)

C       move to the start of the header
        call ftmbyt(iunit,hdstrt(ibuff,chdu(ibuff)),.true.,status)

C       accumulate the header checksum into the previous data checksum
        nrec= (dtstrt(ibuff)-hdstrt(ibuff,chdu(ibuff)))/2880
        sum=dsum
        call ftcsum(iunit,nrec,sum,status)

C       encode the COMPLEMENT of the checksum into a 16-character string
        complm=.true.
        call ftesum(sum,complm,string)

C       update the CHECKSUM keyword value with the checksum string
        call ftmkys(iunit,'CHECKSUM',string,'&',status)
        end
