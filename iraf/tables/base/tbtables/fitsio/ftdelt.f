C--------------------------------------------------------------------------
        subroutine ftdelt(iunit,status)

C       delete a FITS file that was previously opened with ftopen or ftinit
C
C       iunit   i  Fortran I/O unit number
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, July 1994

        integer iunit,status,ibuff

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

C       ignore input status, and delete file regardless of status value

        ibuff=bufnum(iunit)

C       set current column name buffer as undefined
        call ftrsnm

C       flush the buffers holding data for this HDU
        call ftflsh(ibuff,status)

C       recover common block space containing column descriptors for this HDU
        call ftfrcl(iunit,status)

C       delete the file
        call ftclsx(iunit,.false.,status)
        end
