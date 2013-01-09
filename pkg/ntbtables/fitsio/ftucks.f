C----------------------------------------------------------------------
        subroutine ftucks(iunit,status)

C       Update the CHECKSUM keyword value.  This assumes that the DATASUM
C       keyword exists and has the correct value.

C       iunit   i  fortran unit number
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, May, 1995

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

        double precision sum,dsum
        integer ibuff,nrec,dd,mm,yy,i,tstat
        character datstr*8,string*16,comm*40,datsum*20,oldcks*16
        logical complm

        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       get the DATASUM keyword value
        call ftgkys(iunit,'DATASUM',datsum,comm,status)
        if (status .eq. 202)then
             call ftpmsg('DATASUM keyword not found (FTUCKS)')
             return
        end if

C       decode the datasum string into a double precision variable
        do 10 i=1,20
            if (datsum(i:i) .ne. ' ')then
                call ftc2dd(datsum(i:20),dsum,status)
                go to 15
            end if
10      continue
        dsum=0.

C       generate current date string to put into the keyword comment
15      call ftgsdt(dd,mm,yy,status)
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

C       get the CHECKSUM keyword value if it exists
        tstat=status
        call ftgkys(iunit,'CHECKSUM',oldcks,comm,status)
        if (status .eq. 202)then
          status=tstat
          oldcks='0000000000000000'
          comm='encoded HDU checksum updated on '//datstr
          call ftpkys(iunit,'CHECKSUM','0000000000000000',comm,status)
        end if

C       rewrite the header END card, and following blank fill
        call ftwend(iunit,status)
        if (status .gt. 0)return
 
C       move to the start of the header
        call ftmbyt(iunit,hdstrt(ibuff,chdu(ibuff)),.true.,status)

C       accumulate the header checksum into the previous data checksum
        nrec= (dtstrt(ibuff)-hdstrt(ibuff,chdu(ibuff)))/2880
        sum=dsum
        call ftcsum(iunit,nrec,sum,status)

C       encode the COMPLEMENT of the checksum into a 16-character string
        complm=.true.
        call ftesum(sum,complm,string)

C       return if the checksum is correct
        if (string .eq. '0000000000000000')return

        if (oldcks .eq. '0000000000000000')then
C               update the CHECKSUM keyword value with the checksum string
                call ftmkys(iunit,'CHECKSUM',string,'&',status)
        else

C           Zero the checksum and compute the new value
            comm='encoded HDU checksum updated on '//datstr
            call ftmkys(iunit,'CHECKSUM','0000000000000000',comm,status)

C           move to the start of the header
            call ftmbyt(iunit,hdstrt(ibuff,chdu(ibuff)),.true.,status)

C           accumulate the header checksum into the previous data checksum
            sum=dsum
            call ftcsum(iunit,nrec,sum,status)

C           encode the COMPLEMENT of the checksum into a 16-character string
            complm=.true.
            call ftesum(sum,complm,string)

C           update the CHECKSUM keyword value with the checksum string
            call ftmkys(iunit,'CHECKSUM',string,'&',status)
        end if
        end
