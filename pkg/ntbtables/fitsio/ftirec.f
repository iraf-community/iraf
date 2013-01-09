C--------------------------------------------------------------------------
        subroutine ftirec(ounit,pos,record,status)

C       insert a 80-char keyword record into the header at the pos-th keyword
C       position (i.e., immediately before the current keyword at position POS.
C
C       ounit   i  fortran output unit number
C       pos     i  keyword will be inserted at this position (1 = 1st keyword)
C       record  c*80  keyword record
C       OUTPUT PARAMETERS
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Jan 1995

        character*(*) record
        integer ounit,pos,status

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 20)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
C-------END OF COMMON BLOCK DEFINITIONS:------- -----------------------------

        character*80 outrec, inrec
        integer ibuff, fkey, lkey, i, nexthd, nkey

        if (status .gt. 0)return

C       get the number of the data buffer used for this unit
        ibuff=bufnum(ounit)

C       calculate number of existing keywords
        nkey=(hdend(ibuff)-hdstrt(ibuff,chdu(ibuff)))/80

        if (pos .eq. nkey+1)then
C               simply append the record to the header
                call ftprec(ounit,record,status)
                return
        else if (pos .lt. 1 .or. pos .gt.  nkey)then
                status=203
                return
        end if

        outrec=record

C       move to the insert position
        nexthd=hdstrt(ibuff,chdu(ibuff))+(pos-1)*80
        call ftmbyt(ounit,nexthd,.false.,status)
        nxthdr(ibuff)=nexthd

C       calculated the first and last keyword to be rewritten
        fkey=pos
        lkey=fkey + (hdend(ibuff)-nexthd)/80 - 1

C       now sequentially read each keyword and overwrite it with the previous
        do 10 i=fkey,lkey
                call ftgrec(ounit,i,inrec,status)
                call ftmodr(ounit,outrec,status)
                outrec=inrec
10      continue

C       finally, write the last keyword
        call ftprec(ounit,outrec,status)

C       reset the next keyword pointer to follow the inserted keyword
        nxthdr(ibuff)=nexthd+80
        end
