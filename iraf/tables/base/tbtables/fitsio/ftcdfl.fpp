C----------------------------------------------------------------------
        subroutine ftcdfl(iunit,status)

C       Check Data Unit Fill values
C       Check that the data unit is correctly filled with zeros or blanks 
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

        integer ibuff,filpos,nfill,i

        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       check if the data unit is null
        if (theap(ibuff) .eq. 0)return

C       move to the beginning of the fill bytes
        filpos=dtstrt(ibuff)+theap(ibuff)+scount(ibuff)
        call ftmbyt(iunit,filpos,.true.,status)

C       get all the fill bytes
        nfill=(filpos+2879)/2880*2880-filpos
        if (nfill .eq. 0)return

        call ftgcbf(iunit,0,nfill,chbuff,status)
        if (status .gt. 0)then
           call ftpmsg('Error reading data unit fill bytes (FTCDFL).')
           return
        end if

C       set the correct fill value to be checked
        if (hdutyp(ibuff) .eq. 1)then
C              this is an ASCII table; should be filled with blanks
               chfill=char(32)
        else
               chfill=char(0)
        end if

C       check for all zeros or blanks
        do 10 i=1,nfill
            if (chbuff(i) .ne. chfill)then
                status=255
                if (hdutyp(ibuff) .eq. 1)then
                    call ftpmsg('Warning: remaining bytes following'//
     &              ' ASCII table data are not filled with blanks.')
                else
                    call ftpmsg('Warning: remaining bytes following'//
     &              ' data are not filled with zeros.')
                end if
                return
            end if
10      continue             
        end
