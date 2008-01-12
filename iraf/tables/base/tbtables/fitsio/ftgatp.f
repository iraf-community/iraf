C--------------------------------------------------------------------------
        subroutine ftgatp(ibuff,keynam,value,status)

C       Get ASCII Table Parameter
C       test if the keyword is one of the table column definition keywords
C       of an ASCII table. If so, decode it and update the value in the common 
C       block

C       ibuff   i sequence number of the data buffer
C       keynam  c name of the keyword
C       value   c value of the keyword
C       status  i returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ibuff,status
        character keynam*8,value*70

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
C       nb = number of file buffers = max. number of FITS file opened at once
C       nf = maximum number of fields allowed in a table
        integer nf,nb
        parameter (nb = 20)
        parameter (nf = 3000)

C       tfield = number of fields in the table
C       tbcol = byte offset in the row of the beginning of the column
C       rowlen = length of one row of the table, in bytes
C       tdtype = integer code representing the datatype of the column
C       trept = the repeat count = number of data values/element in the column
C       tnull = the value used to represent an undefined value in the column
C       tscale = the scale factor for the column
C       tzero = the scaling zero point for the column
C       scount = the total size of the binary table heap (+ gap if any)
C       theap = the starting byte offset for the binary table heap, relative
C               to the start of the binary table data
C       nxheap = the next empty heap location
        integer tfield,tstart,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tstart(nb),tbcol(nf),rowlen(nb),
     &  tdtype(nf),trept(nf),tscale(nf),tzero(nf),tnull(nf),scount(nb)
     &  ,theap(nb),nxheap(nb)

C       cnull = character string representing nulls in character columns
C       cform = the Fortran format of the column
        character cnull*16, cform*8
        common/ft0003/cnull(nf),cform(nf)
C-------END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer nfield,i,c2,bcol,tstat
        character tform*16

        if (status .gt. 0)return
        tstat=status

        if (keynam(1:5) .eq. 'TFORM')then
C               get the field number
                call ftc2ii(keynam(6:8),nfield,status)
                if (status .gt. 0)then
C                   this must not have been a TFORMn keyword
                    status=tstat
                else
C                   get the TFORM character string, without quotes
                    call ftc2s(value,tform,status)
                    if (status .gt. 0)return
                    if  (tform(1:1) .ne. 'A' .and. tform(1:1) .ne. 'I'
     &              .and. tform(1:1) .ne. 'F' .and. tform(1:1) .ne. 'E'
     &              .and. tform(1:1) .ne. 'D')then
                        status=311
                     call ftpmsg('Illegal '//keynam//' format code: '
     &                           //tform)
                        return
                    end if

                    cform(nfield+tstart(ibuff))=tform
C                   set numeric data type code to indicate an ASCII table field
                    tdtype(nfield+tstart(ibuff))=16
C                   set the repeat count to 1
                    trept(nfield+tstart(ibuff))=1
C                   set the TNULL parameter to the width of the field:
                    c2=0
                    do 10 i=2,8
                        if (tform(i:i) .ge. '0' .and. tform(i:i)
     &                     .le. '9')then
                                c2=i
                        else
                                go to 20
                        end if
10                  continue
20                  continue

                    if (status .gt. 0)return
                    if (c2 .eq. 0)then
C                       no explicit field width, so assume width=1 character
                        tnull(nfield+tstart(ibuff))=1
                    else
                        call ftc2ii(tform(2:c2),tnull(nfield+
     &                              tstart(ibuff)),status)
                        if (status .gt. 0)then
C                               error parsing the TFORM value string
                                status=261
           call ftpmsg('Error parsing '//keynam//' field width: '
     &                  //tform)
                        end if
                    end if
                end if
        else if (keynam(1:5) .eq. 'TBCOL')then
C               get the field number
                call ftc2ii(keynam(6:8),nfield,status)
                if (status .gt. 0)then
C                   this must not have been a TBCOLn keyword
                    status=tstat
                else
C                   get the beginning column number
                    call ftc2ii(value,bcol,status)
                     if (status .gt. 0)then
                        call ftpmsg('Error reading value of '//keynam        
     &                  //' as an integer: '//value)
                     else
                        tbcol(nfield+tstart(ibuff))=bcol-1
                     end if
                end if
        else if (keynam(1:5) .eq. 'TSCAL')then
C               get the field number
                call ftc2ii(keynam(6:8),nfield,status)
                if (status .gt. 0)then
C                   this must not have been a TSCALn keyword
                    status=tstat
                else
C                   get the scale factor
                    call ftc2dd(value,tscale(nfield+tstart(ibuff)),
     &                          status)
                    if (status .gt. 0)then
                         call ftpmsg('Error reading value of'//keynam
     &                //' as a Double: '//value)
                    end if
                end if
        else if (keynam(1:5) .eq. 'TZERO')then
C               get the field number
                call ftc2ii(keynam(6:8),nfield,status)
                if (status .gt. 0)then
C                   this must not have been a TZEROn keyword
                    status=tstat
                else
C                   get the scaling zero point
                    call ftc2dd(value,tzero(nfield+tstart(ibuff)),
     &                          status)
                    if (status .gt. 0)then
                         call ftpmsg('Error reading value of'//keynam
     &                //' as a Double: '//value)
                    end if
                end if
        else if (keynam(1:5) .eq. 'TNULL')then
C               get the field number
                call ftc2ii(keynam(6:8),nfield,status)
                if (status .gt. 0)then
C                   this must not have been a TNULLn keyword
                    status=tstat
                else
C                   get the Null value flag (character)
                    call ftc2s(value,cnull(nfield+tstart(ibuff)),status)
                    if (status .gt. 0)then
                         call ftpmsg('Error reading value of'//keynam
     &                //' as a character string: '//value)
                    end if
                end if
        end if
        end
