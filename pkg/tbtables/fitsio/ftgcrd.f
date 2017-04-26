C--------------------------------------------------------------------------
        subroutine ftgcrd(iunit,keynam,card,status)

C       Read the 80 character card image of a specified header keyword record

C       iunit   i  Fortran I/O unit number
C       keynam  c  name of keyword to be read
C       OUTPUT PARAMETERS:
C       card    c  80 character card image that was read
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June, 1991

        character*(*) keynam
        integer iunit,status,i,j,ibuff,maxkey,start
        character*(*) card
        character kname*8
        character*80 keybuf

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

        card=' '
        if (status .gt. 0)go to 100

C       get the number of the data buffer used for this unit
        ibuff=bufnum(iunit)

C       make sure keyword name is in uppercase
        kname=keynam
        call ftupch(kname)

C       Start by searching for keyword from current pointer position to the end.
C       Calculate the maximum number of keywords to be searched:
        start=nxthdr(ibuff)
        maxkey=(hdend(ibuff)-start)/80

        do 20 j=1,2
C           position I/O pointer to the next header keyword
            if (maxkey .gt. 0)then
                call ftmbyt(iunit,start,.false.,status)
            end if

            do 10 i=1,maxkey
                call ftgcbf(iunit,1,80,keybuf,status)
                if (status .gt. 0)go to 100
                if (keybuf(1:8) .eq. kname)then
C                       setheader pointer to the following keyword
                        nxthdr(ibuff)=start+i*80
                        card=keybuf
                        return
                end if
10          continue

C           didn't find keyword yet, so now search from top down to starting pt.
C           calculate max number of keywords to be searched and reset nxthdr
            maxkey=(start-hdstrt(ibuff,chdu(ibuff)))/80
            start=hdstrt(ibuff,chdu(ibuff))
20      continue                        

C       keyword was not found
        status=202

C       don't write to error stack because this innoculous error happens a lot
C       call ftpmsg('Could not find the '//kname//' keyword to read.')

100     continue
        end
