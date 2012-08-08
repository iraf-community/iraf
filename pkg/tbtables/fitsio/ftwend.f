C----------------------------------------------------------------------
        subroutine ftwend(iunit,status)

C       write the END card, and following fill values in the CHDU

C       iunit   i  fortran unit number
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Aug 1994

        integer iunit,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 20)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,nblank,i,endpos
        character*80 rec

        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       calc the data starting position if not currently defined
        if (dtstrt(ibuff) .lt. 0)then
                dtstrt(ibuff)=(hdend(ibuff)/2880 + 1)*2880
        end if

C       calculate the number of blank keyword slots in the header
        endpos=hdend(ibuff)
        nblank=(dtstrt(ibuff)-endpos)/80
C       move the i/o pointer to the end of the header keywords
        call ftmbyt(iunit,endpos,.true.,status)

C       fill all the slots with blanks
        rec=' '
        do 10 i=1,nblank
                call ftpcbf(iunit,1,80,rec,status)
10      continue

C               The END keyword must either be placed 
C               immediately after the last keyword that was written 
C               (as indicated by the HDEND parameter), or must be in the
C               first 80 bytes of the FITS record immediately preceeding
C               the data unit, whichever is further in the file.
C               The latter will occur if the user reserved room for more
C               header keywords which have not (yet) been filled.

C       move pointer to where the END card should be
        endpos=max(endpos,dtstrt(ibuff)-2880)
        call ftmbyt(iunit,endpos,.true.,status)

C       write the END record to the output buffer:
        rec='END'
        call ftpcbf(iunit,1,80,rec,status)

        if (status .gt. 0)then
            call ftpmsg('Error while writing END card (FTWEND).')
        end if
        end
