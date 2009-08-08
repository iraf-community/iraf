C--------------------------------------------------------------------------
        subroutine ftprec(ounit,record,status)

C       write a 80 character record to the FITS header
C
C       ounit   i  fortran output unit number
C       record  c  input 80 character header record
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) record
        character*80  rec
        integer ounit,status,ibuff

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

        if (status .gt. 0)return

C       get the number of the data buffer used for this unit
        ibuff=bufnum(ounit)

        if (dtstrt(ibuff) .gt. 0 
     &    .and.(dtstrt(ibuff)-hdend(ibuff)) .le. 80)then
C               not enough room in the header for another keyword

C               try getting more header space
                call ftiblk(ounit,1,0,status)
                if (status .gt. 0)then
                        go to 900
                end if
        end if
                
        rec=record

C       make sure keyword name is in upper case
        call ftupch(rec(1:8))

C       test that keyword name contains only legal characters
        call fttkey(rec(1:8),status)

C       test that the rest of the record contains only legal values
        call fttrec(rec(9:80),status)

C       position the I/O pointer to the end of the header
        call ftmbyt(ounit,hdend(ibuff),.true.,status)

C       append the 80 characters to the output buffer:
        call ftpcbf(ounit,1,80,rec,status)
        if (status .gt. 0)go to 900

C       increment the pointer to the last header record
        hdend(ibuff)=hdend(ibuff)+80
        nxthdr(ibuff)=hdend(ibuff)

900     continue
        end
