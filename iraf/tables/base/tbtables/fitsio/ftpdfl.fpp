C----------------------------------------------------------------------
        subroutine ftpdfl(iunit,status)

C       Write the Data Unit Fill values if they are not already correct
C       Fill the data unit with zeros or blanks depending on the type of HDU
C       from the end of the data to the end of the current FITS 2880 byte block

C       iunit   i  fortran unit number
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June, 1994

        integer iunit,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
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
        character*1 chbuff(2880),chfill,xdummy(2879)
        common/ftheap/chbuff,chfill,xdummy
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,filpos,nfill,i,tstat

        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       check if the data unit is null

        if (theap(ibuff) .eq. 0)return

        filpos=dtstrt(ibuff)+theap(ibuff)+scount(ibuff)
        nfill=(filpos+2879)/2880*2880-filpos

C       return if there are no fill bytes 
        if (nfill .eq. 0)return

C       set the correct fill value to be checked
        if (hdutyp(ibuff) .eq. 1)then
C              this is an ASCII table; should be filled with blanks
               chfill=char(32)
        else
               chfill=char(0)
        end if

C       move to the beginning of the fill bytes and read them
        tstat=status
        call ftmbyt(iunit,filpos,.true.,status)
        call ftgcbf(iunit,0,nfill,chbuff,status)

        if (status .gt. 0)then
C           fill bytes probably haven't been written yet so have to write them
            status=tstat
            go to 100
        end if

C       check if all the fill values are correct
        do 10 i=1,nfill
            if (chbuff(i) .ne. chfill)go to 100
10      continue             

C       fill bytes were correct, so just return
        return

100     continue

C       fill the buffer with the correct fill value
        do 20 i=1,nfill
               chbuff(i)=chfill
20      continue

C       move to the beginning of the fill bytes
        call ftmbyt(iunit,filpos,.true.,status)

C       write all the fill bytes
        call ftpcbf(iunit,0,nfill,chbuff,status)

        if (status .gt. 0)then
           call ftpmsg('Error writing Data Unit fill bytes (FTPDFL).')
        end if
        end
