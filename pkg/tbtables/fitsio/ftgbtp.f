C--------------------------------------------------------------------------
        subroutine ftgbtp(ibuff,keynam,value,status)

C       Get Binary Table Parameter
C       test if the keyword is one of the table column definition keywords
C       of a binary table. If so, decode it and update the values in the common
C       block

C       ibuff   i sequence number of the data buffer
C       OUTPUT PARAMETERS:
C       keynam  c name of the keyword
C       value   c value of the keyword
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ibuff,status,width
        character keynam*8,value*70

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
C       nb = number of file buffers = max. number of FITS file opened at once
C       nf = maximum number of fields allowed in a table
        integer nf,nb
        parameter (nb = 20)
        parameter (nf = 3000)
        integer tfield,tstart,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tstart(nb),tbcol(nf),rowlen(nb),
     &  tdtype(nf),trept(nf),tscale(nf),tzero(nf),tnull(nf),scount(nb)
     &  ,theap(nb),nxheap(nb)
C-------END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer nfield,tstat
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
C                   get the datatype code and repeat count
                    call ftbnfm(tform,tdtype(nfield+tstart(ibuff)),
     &                 trept(nfield+tstart(ibuff)),width,status)
                    if (tdtype(nfield+tstart(ibuff)) .eq. 1)then
C                       treat Bit datatype as if it were a Byte datatype
                        tdtype(nfield+tstart(ibuff))=11
                        trept(nfield+tstart(ibuff))=(trept(nfield+
     &                  tstart(ibuff))+7)/8
                    else if (tdtype(nfield+tstart(ibuff)) .eq. 16)then
C                      store the width of the ASCII field in the TNULL parameter
                        tnull(nfield+tstart(ibuff))=width
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
C                   make sure this is not an ASCII column (the tnull
C                   variable is use to store the ASCII column width)
                    if (tdtype(nfield+tstart(ibuff)) .ne. 16)then
C                       get the Null value flag (Integer)
                        call ftc2ii(value,tnull(nfield+tstart(ibuff)),
     &                              status)
                        if (status .gt. 0)then
                            call ftpmsg('Error reading value of '//
     &                      keynam//' as an integer: '//value)
                        end if
                    end if
                end if
        else if (keynam(1:8) .eq. 'THEAP   ')then
C               get the heap offset value
                call ftc2ii(value,theap(ibuff),status)
                if (status .gt. 0)then
                        call ftpmsg('Error reading value of '//keynam
     &                  //' as an integer: '//value)
                end if
        end if
        end
