C--------------------------------------------------------------------------
        subroutine ftrhdu(iunit,xtend,status)

C       read the CHDU structure by reading the header keywords which define
C       the size and structure of the header and data units.

C       iunit   i  Fortran I/O unit number
C       OUTPUT PARAMETERS:
C       xtend   i  returned type of extension:   0 = the primary HDU
C                                                1 = an ASCII table
C                                                2 = a binary table
C                                               -1 = unknown
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991
        
        integer iunit,xtend,status,i,ic,tstat
        character keynam*8,exttyp*10,comm*30,keybuf*80
        logical endof

        if (status .gt. 0)return

C       read first keyword to determine the type of the CHDU
        call ftgrec(iunit,1,keybuf,status)

        if (status .gt. 0)then
          call ftpmsg('Cannot read first keyword in header (FTRHDU)')
                return
        end if

C       release any current column descriptors for this unit
        call ftfrcl(iunit,status)

        keynam=keybuf(1:8)
C       parse the value and comment fields from the record
        call ftpsvc(keybuf,exttyp,comm,status)

        if (status .gt. 0)then
C               unknown type of FITS record; can't read it
          call ftpmsg('Cannot parse value of first keyword; unknown '
     &      //'type of FITS record (FTRHDU):')

        else if (keynam .eq. 'SIMPLE')then
C               initialize the parameters describing the primay HDU
                call ftpini(iunit,status)
                xtend=0
        else if (keynam.eq.'XTENSION')then
                if (exttyp(1:1) .ne. '''')then
C                       value of XTENSION is not a quoted character string!
                        if (keybuf(9:10) .ne. '= ')then
                            call ftpmsg('XTENSION keyword does not '
     &                     //'have "= " in cols 9-10.')
                        else
                        call ftpmsg('Unknown type of extension; value'
     &               //' of XTENSION keyword is not a quoted string:')
                        end if
                        status=251
                        call ftpmsg(keybuf)
                else if (exttyp(2:9) .eq. 'TABLE   ')then
C                       initialize the parameters for the ASCII table extension
                        call ftaini(iunit,status)
                        xtend=1
                else if (exttyp(2:9) .eq. 'BINTABLE' .or. exttyp(2:9)  
     &            .eq. 'A3DTABLE' .or. exttyp(2:9) .eq. '3DTABLE ')then
C                       initialize the parameters for the binary table extension
                        call ftbini(iunit,status)
                        xtend=2
                else 
C                       try to initialize the parameters describing extension
                        tstat=status
                        call ftpini(iunit,status)
                        xtend=0
                        if (status .eq. 251)then
C                           unknown type of extension
                            xtend=-1
                            status=tstat
                        end if
                end if
        else
C               unknown record 
C               If file is created on a VAX with 512-byte records, then
C               the FITS file may have fill bytes (ASCII NULs) at the end.
C               Also, if file has been editted on a SUN, an extra ASCII 10 
C               character may appear at the end of the file.  Finally, if
C               file is not a multiple of the record length long, then
C               the last truncated record may be filled with ASCII blanks.
C               So, if the record only contains NULS, LF, and blanks, then
C               assume we found the end of file.  Otherwise report an error.

                endof=.true.
                do 10 i=1,80
                    ic=ichar(keybuf(i:i))
                    if (ic .ne. 0 .and .ic .ne. 10 .and. ic .ne. 32)
     &                 endof=.false.
10              continue
                if (endof)then
                     status=107
                     call ftpmsg('ASCII 0s, 10s, or 32s at start of '
     &             //'extension are treated as EOF (FTRHDU):')
                else
                     status=252
                     call ftpmsg('Extension does not start with SIMPLE'
     &               //' or XTENSION keyword (FTRHDU):')
                end if
                xtend=-1
                call ftpmsg(keybuf)
        end if        
        end
