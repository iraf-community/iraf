C--------------------------------------------------------------------------
        subroutine ftdcol(iunit,colnum,status)

C       delete a column from a table

C       iunit   i  Fortran I/O unit number
C       colnum  i  number of of the column to be deleted
C       status  i  returned error status (0=ok)

        integer iunit,colnum,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 20)
        parameter (nf = 3000)
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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,typhdu,delbyt,fstbyt,sp,tflds,i
        integer naxis1,naxis2,size,freesp,nblock,tbc
        character comm*70,keynam*8

        if (status .gt. 0)return

C       define the number of the buffer used for this file
        ibuff=bufnum(iunit)

C       test that the CHDU is an ASCII table or BINTABLE
        typhdu=hdutyp(ibuff)
        if (typhdu .ne. 1 .and. typhdu .ne. 2)then
                status=235
                call ftpmsg('Can only delete column from TABLE '//
     &          'or BINTABLE extension (FTDCOL)')
                return
        end if

C       check if column number exists in the table
        tflds=tfield(ibuff)
        if (colnum .lt. 1 .or. colnum .gt. tflds)then
            status=302
            return
        end if

C       get the starting byte position of the column (=zero for first column)
        fstbyt=tbcol(colnum+tstart(ibuff))

C       find the width of the column
        if (typhdu .eq. 1)then
C           tnull is used to store the width of the ASCII column field
C           NOTE: ASCII columns may not be in physical order, or may overlap.

            delbyt=tnull(colnum+tstart(ibuff))

C           delete the space(s) between the columns, if there are any.
            if (colnum .lt. tflds)then
C               check for spaces between following column
                sp=tbcol(colnum+1+tstart(ibuff))-tbcol(colnum+
     &             tstart(ibuff))-delbyt
                if (sp .gt. 0)then
                    delbyt=delbyt+1
                end if
            else if (colnum .gt. 1)then
C               check for space between the last and next to last columns
                sp=tbcol(colnum+tstart(ibuff))-tbcol(colnum-1+
     &             tstart(ibuff))-tnull(colnum-1+tstart(ibuff))
                if (sp .gt. 0)then
                   delbyt=delbyt+1
                   fstbyt=fstbyt-1
                end if
            end if
        else
            if (colnum .lt. tflds)then
                delbyt=tbcol(colnum+1+tstart(ibuff))-
     &                 tbcol(colnum+tstart(ibuff))
            else
                delbyt=rowlen(ibuff)-tbcol(colnum+tstart(ibuff))
            end if
        end if

C       get current size of the table
        naxis1=rowlen(ibuff)
        call ftgkyj(iunit,'NAXIS2',naxis2,comm,status)

C       Calculate how many FITS blocks (2880 bytes) need to be deleted
        size=theap(ibuff)+scount(ibuff)
        freesp=(delbyt*naxis2) + ((size+2879)/2880)*2880 - size
        nblock=freesp/2880

C       shift each row up, deleting the desired column
        call ftcdel(iunit,naxis1,naxis2,delbyt,fstbyt,status)

C       shift the heap up and update pointer to start of heap
        size=delbyt*naxis2
        call fthpup(iunit,size,status)

C       delete the needed number of new FITS blocks at the end of the HDU
        if (nblock .gt. 0)call ftdblk(iunit,nblock,1,status)

        if (typhdu .eq. 1)then
C           adjust the TBCOL values of the remaining columns
            do 10 i=1,tflds
                call ftkeyn('TBCOL',i,keynam,status)
                call ftgkyj(iunit,keynam,tbc,comm,status)
                if (tbc .gt. fstbyt)then
                     tbc=tbc-delbyt
                     call ftmkyj(iunit,keynam,tbc,'&',status)
                end if
10          continue
        end if

C       update the mandatory keywords
        call ftmkyj(iunit,'TFIELDS',tflds-1,'&',status)        
        call ftmkyj(iunit,'NAXIS1',naxis1-delbyt,'&',status)

C       delete the index keywords starting with 'T' associated with the 
C       deleted column and subtract 1 from index of all higher keywords
        call ftkshf(iunit,colnum,tflds,-1,status)

C       parse the header to initialize the new table structure
        call ftrdef(iunit,status)
        end
