C--------------------------------------------------------------------------
        subroutine ftmodr(ounit,record,status)

C       modify the preceeding 80 character record in the FITS header
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

        rec=record

C       make sure keyword name is in upper case
        call ftupch(rec(1:8))

C       test that keyword name contains only legal characters
        call fttkey(rec(1:8),status)

C       move the I/O pointer back to the beginning of the preceeding keyword
        call ftmbyt(ounit,nxthdr(ibuff)-80,.false.,status)

C       overwrite the 80 characters to the output buffer:
        call ftpcbf(ounit,1,80,rec,status)
        end
