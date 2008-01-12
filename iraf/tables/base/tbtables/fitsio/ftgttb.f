C----------------------------------------------------------------------
        subroutine ftgttb(iunit,ncols,nrows,nfield,status)

C       test that this is a legal ASCII table, and get some keywords
C
C       iunit   i  Fortran i/o unit number
C       OUTPUT PARAMETERS:
C       ncols   i  number of columns in the table
C       nrows   i  number of rows in the table
C       nfield  i  number of fields in the table
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,ncols,nrows,nfield,status
        character keynam*8,value*10,comm*8,keybuf*80

        if (status .gt. 0)return

C       check for correct type of extension
        call ftgrec(iunit,1,keybuf,status)

        keynam=keybuf(1:8)
C       parse the value and comment fields from the record
        call ftpsvc(keybuf,value,comm,status)

        if (status .gt. 0)go to 900

        if (keynam .eq. 'XTENSION')then
                if (value(2:9) .ne. 'TABLE   ')then
C                       this is not a ASCII table extension
                        status=226
        call ftpmsg('Was expecting an ASCII table; instead got '//
     &  'XTENSION= '//value)
                        call ftpmsg(keybuf)
                        go to 900
                 end if
        else
                 status=225
        call ftpmsg('First keyword of extension was not XTENSION:'//
     &           keynam)
                 call ftpmsg(keybuf)
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
C               illegal value of NAXIS
                status=212
        end if
        if (status .gt. 0)go to 900

C       check that the 4th keyword is NAXIS1 and get it's value
        call ftgtkn(iunit,4,'NAXIS1',ncols,status)
        if (status .eq. 208)then
C               NAXIS1 keyword not found
                status=224
        else if (status .eq. 209)then
C               illegal NAXIS1 value
                status=213
        end if
        if (status .gt. 0)go to 900

C       check that the 5th keyword is NAXIS2 and get it's value
        call ftgtkn(iunit,5,'NAXIS2',nrows,status)
        if (status .eq. 208)then
C               NAXIS2 keyword not found
                status=224
        else if (status .eq. 209)then
C               illegal NAXIS2 value
                status=213
        end if
        if (status .gt. 0)go to 900

C       check that the 6th keyword is PCOUNT = 0
        call fttkyn(iunit,6,'PCOUNT','0',status)
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

C       check that the 8th keyword is TFIELDS
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
     &       'the ASCII TABLE header (FTGTTB).')
        end if
        end
