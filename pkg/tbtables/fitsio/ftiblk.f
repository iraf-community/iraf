C--------------------------------------------------------------------------
        subroutine ftiblk(ounit,nblock,hdrdat,status)

C       insert a 2880-byte block at the end of the current header or data.

C       ounit   i  fortran output unit number
C       nblock  i  number of blocks to insert
C       hdrdat  i  insert space in header (0) or data (1)
C       status  i  returned error status (0=ok)

        integer ounit,nblock,hdrdat,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 20)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
        character*1 buff(2880,2)
        common/ftheap/buff
C       END OF COMMON BLOCK DEFINITIONS:------------------------------------

        integer ibuff,ipoint,jpoint,i,tstat,thdu,nshift,in,out,tin
        character*1 cfill

        if (status .gt. 0)return
        tstat=status

C       get the number of the data buffer used for this unit
        ibuff=bufnum(ounit)

C       set the appropriate fill value
        if (hdrdat .eq. 0 .or. hdutyp(ibuff) .eq. 1)then
C               fill  header or ASCII table with space
                cfill=char(32)
        else
C               fill with Null (0) in image or bintable data area        
                cfill=char(0)
        end if

C       find position in file to insert new block
        if (hdrdat .eq. 0)then
            ipoint=dtstrt(ibuff)
        else
            ipoint=hdstrt(ibuff,chdu(ibuff)+1)
        end if


        if (nblock .eq. 1 .and. hdrdat .eq. 0)then
C******************************************************************
C  Don't use this algoritm, even though it may be faster (but initial
C  tests showed it didn't make any difference on a SUN) because it is
C  less safe than the other more general algorithm.  If there is
C  not enough disk space available for the added block, this faster
C  algorithm won't fail until it tries to move the last block, thus leaving
C  the FITS file in a corrupted state.   The other more general
C  algorithm tries to add a new empty block to the file as the
C  first step.  If this fails, it still leaves the current FITS
C  file unmodified, which is better for the user.  
C******************************************************************
C  (Note added later:)
C  Will use this algorithm anyway when inserting one block in a FITS
C  header because the more general algorithm results in a status=252 error
C  in cases where the number of rows in a table has not yet been defined
C******************************************************************
C           use this more efficient algorithm if just adding a single block
C           initialize the first buffer
            do 5 i=1,2880
               buff(i,1)=cfill
5           continue

            in=2
            out=1

C           move to the read start position
10          call ftmbyt(ounit,ipoint,.false.,status)

C           read one 2880-byte FITS logical record into the input buffer
            call ftgcbf(ounit,0,2880,buff(1,in),status)

C           check for End-Of-File
            if (status .eq. 107)go to 20

C           move back to the write start postion
            call ftmbyt(ounit,ipoint,.false.,status)

C           write the 2880-byte FITS logical record stored in the output buffer
            call ftpcbf(ounit,0,2880,buff(1,out),status)

C           check for error during write (the file may not have write access)
            if (status .gt. 0)return

C           swap the input and output buffer pointers and move to next block
            tin=in
            in=out
            out=tin
            ipoint=ipoint+2880

C           now repeat the process until we reach the End-Of-File
            go to 10

C           we have reached the end of file; now append the last block
20          status=tstat

C           move back to the write start postion
            call ftmbyt(ounit,ipoint,.true.,status)

C           write the 2880-byte FITS logical record stored in the output buffer
            call ftpcbf(ounit,0,2880,buff(1,out),status)

        else
C           use this general algorithm for adding arbitrary number of blocks

C           first, find the end of file
            thdu=chdu(ibuff)

30          call ftmahd(ounit,maxhdu(ibuff)+1,i,status)

            if (status .eq. 107)then
                status=tstat
C               move back to the current extension
                call ftmahd(ounit,thdu,i,status)
                go to 100
            else if (status .le. 0)then
                go to 30
            else
                call ftpmsg('Error while seeking End of File (FTIBLK)')
                return
            end if

C           calculate number of 2880-byte blocks that have to be shifted down
100         continue
            nshift=(hdstrt(ibuff,maxhdu(ibuff)+1)-ipoint)/2880
            jpoint=hdstrt(ibuff,maxhdu(ibuff)+1)-2880

C           move all the blocks, one at a time, starting at end of file and
C           working back to the insert position
            do 110 i=1,nshift

C               move to the read start position
                call ftmbyt(ounit,jpoint,.false.,status)

C               read one 2880-byte FITS logical record
                call ftgcbf(ounit,0,2880,buff,status)

C               move forward to the write start postion
                call ftmbyt(ounit,jpoint+nblock*2880,.true.,status)

C               write the 2880-byte FITS logical record
                call ftpcbf(ounit,0,2880,buff,status)

C               check for error 
                if (status .gt. 0)then
                    call ftpmsg('Error inserting empty FITS block(s) '//
     &              '(FTIBLK)')
                    return
                end if
                jpoint=jpoint-2880
110         continue

            do 120 i=1,2880
                buff(i,1)=cfill
120         continue
             
C           move back to the write start postion
            call ftmbyt(ounit,ipoint,.true.,status)

            do 130 i=1,nblock
C               write the 2880-byte FITS logical record 
                call ftpcbf(ounit,0,2880,buff,status)
130         continue
        end if

        if (hdrdat .eq. 0)then
C               recalculate the starting location of the current data unit
                dtstrt(ibuff)=dtstrt(ibuff)+2880*nblock
        end if

C       recalculate the starting location of all subsequent HDUs
        do 140 i=chdu(ibuff)+1,maxhdu(ibuff)+1
                    hdstrt(ibuff,i)=hdstrt(ibuff,i)+2880*nblock
140     continue
        if (status .gt. 0)then
            call ftpmsg('Error inserting FITS block(s) (FTIBLK)')
        end if
        end
