C----------------------------------------------------------------------
        subroutine ftchfl(iunit,status)

C       Check Header Fill values
C       Check that the header unit is correctly filled with blanks from the
C       END card to the end of the current FITS 2880-byte block

C       iunit   i  fortran unit number
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June, 1994

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
        logical gotend

        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       calculate the number of blank keyword slots in the header
        endpos=hdend(ibuff)
        nblank=(dtstrt(ibuff)-endpos)/80
C       move the i/o pointer to the end of the header keywords
        call ftmbyt(iunit,endpos,.true.,status)
C       find the END card (there may be blank keywords perceeding it)

        gotend=.false.
        do 10 i=1,nblank
                call ftgcbf(iunit,1,80,rec,status)
                if (rec(1:8) .eq. 'END     ')then
                       if (gotend)then
C                          there is a duplicate END record
                           status=254
             call ftpmsg('Warning: Header fill area contains '//
     &       'duplicate END card:')
                       end if
                       gotend=.true.
                       if (rec(9:80) .ne. ' ')then
C                          END keyword has extra characters
                           status=253
            call ftpmsg('Warning: END keyword contains '//
     &      'extraneous non-blank characters:')
                       end if
                 else if (gotend)then
                       if (rec .ne. ' ')then
C                          The fill area contains extraneous characters
                           status=254
             call ftpmsg('Warning: Header fill area contains '//
     &       'extraneous non-blank characters:')
                        end if
                end if

                if (status .gt. 0)then
                           call ftpmsg(rec)
                           return
                end if
10      continue
        end
