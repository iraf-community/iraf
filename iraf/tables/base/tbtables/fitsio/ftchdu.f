C----------------------------------------------------------------------
        subroutine ftchdu(iunit,status)

C       Close Header Data Unit
C       If we have write access to the file, then close the current HDU by:
C                 -padding remaining space in the header with blanks
C                 -writing the END keyword in the CHU
C                 -check the data fill values, and rewrite them if not correct
C                 -flushing the current buffer to disk
C                 -recover common block space containing column descriptors

C       iunit   i  fortran unit number
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June, 1991

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

        integer ibuff

C       ignore input status and close HDU regardless of input status value

        ibuff=bufnum(iunit)

C       see if we have write access to this file
        if (wrmode(ibuff))then
C           rewrite the header END card and the following blank fill, and
C           insure that the internal data structure matches the keywords
            call ftrdef(iunit,status)

C           write the correct data fill values, if they are not already correct
            call ftpdfl(iunit,status)
        end if

C       set current column name buffer as undefined
        call ftrsnm

C       flush the buffers holding data for this HDU
        call ftflsh(ibuff,status)

C       recover common block space containing column descriptors for this HDU
        call ftfrcl(iunit,status)

        if (status .gt. 0)then
            call ftpmsg('Error while closing current HDU (FTCHDU).')
        end if
        end
