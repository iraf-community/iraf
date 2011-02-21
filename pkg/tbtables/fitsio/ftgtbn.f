C----------------------------------------------------------------------
        subroutine ftgtbn(iunit,ncols,nrows,pcount,nfield,status)

C       check that this is a valid binary table and get parameters
C
C       iunit   i  Fortran i/o unit number
C       ncols   i  width of each row of the table, in bytes
C       nrows   i  number of rows in the table
C       pcount  i  size of special data area following the table (usually = 0)
C       nfield  i  number of fields in the table
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,ncols,nrows,nfield,pcount,status
        character keynam*8,value*10,comm*8,rec*80

        if (status .gt. 0)return

C       check for correct type of extension
        call ftgrec(iunit,1,rec,status)
        if (status .gt. 0)go to 900

        keynam=rec(1:8)

        if (keynam .eq. 'XTENSION')then
                call ftpsvc(rec,value,comm,status)
                if (status .gt. 0)go to 900

                if (value(2:9) .ne. 'BINTABLE' .and. 
     &              value(2:9) .ne. 'A3DTABLE' .and.
     &              value(2:9) .ne. '3DTABLE ')then
C                       this is not a binary table extension
                        status=227
                        go to 900
                 end if
        else
                 status=225
                 go to 900
        end if

C       check that the second keyword is BITPIX = 8
        call fttkyn(iunit,2,'BITPIX','8',status)
        if (status .eq. 208)then
C               BITPIX keyword not found
                status=222
        else if (status .eq. 209)then
C               illegal value of BITPIX
                status=211
        end if
        if (status .gt. 0)go to 900

C       check that the third keyword is NAXIS = 2
        call fttkyn(iunit,3,'NAXIS','2',status)
        if (status .eq. 208)then
C               NAXIS keyword not found
                status=223
        else if (status .eq. 209)then
C               illegal NAXIS value
                status=212
        end if
        if (status .gt. 0)go to 900

C       check that the 4th keyword is NAXIS1 and get it's value
        call ftgtkn(iunit,4,'NAXIS1',ncols,status)
        if (status .eq. 208)then
C               NAXIS1 keyword not found
                status=224
        else if (status .eq. 209)then
C               illegal value of NAXISnnn
                status=213
        end if
        if (status .gt. 0)go to 900

C       check that the 5th keyword is NAXIS2 and get it's value
        call ftgtkn(iunit,5,'NAXIS2',nrows,status)
        if (status .eq. 208)then
C               NAXIS2 keyword not found
                status=224
        else if (status .eq. 209)then
C               illegal value of NAXISnnn
                status=213
        end if
        if (status .gt. 0)go to 900

C       check that the 6th keyword is PCOUNT and get it's value
        call ftgtkn(iunit,6,'PCOUNT',pcount,status)
        if (status .eq. 208)then
C               PCOUNT keyword not found
                status=228      
        else if (status .eq. 209)then
C               illegal PCOUNT value
                status=214
        end if
        if (status .gt. 0)go to 900

C       check that the 7th keyword is GCOUNT = 1
        call fttkyn(iunit,7,'GCOUNT','1',status)
        if (status .eq. 208)then
C               GCOUNT keyword not found
                status=229
        else if (status .eq. 209)then
C               illegal value of GCOUNT
                status=215
        end if
        if (status .gt. 0)go to 900

C       check that the 8th keyword is TFIELDS and get it's value
        call ftgtkn(iunit,8,'TFIELDS',nfield,status)
        if (status .eq. 208)then
C               TFIELDS keyword not found
                status=230
        else if (status .eq. 209)then
C               illegal value of TFIELDS
                status=216
        end if

900     continue
        if (status .gt. 0)then
            call ftpmsg('Failed to parse the required keywords in '//
     &       'the binary BINTABLE header (FTGTTB).')
        end if
        end
