C--------------------------------------------------------------------------
        subroutine ftopen(funit,fname,rwmode,block,status)

C       open an existing FITS file with readonly or read/write access
C
C       funit   i  Fortran I/O unit number
C       fname   c  name of file to be opened
C       rwmode  i  file access mode: 0 = readonly; else = read and write
C       block   i  returned record length blocking factor
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer funit,rwmode,block,status,strlen,i,xtend
        character*(*) fname

        if (status .gt. 0)return

C       ignore any leading blanks in the file name
        strlen=len(fname)
        do 10 i=1,strlen
            if (fname(i:i) .ne. ' ')then

C               call the machine dependent routine which opens the file
                call ftopnx(funit,fname(i:),0,rwmode,block,status)
                if (status .gt. 0)then
                     call ftpmsg('FTOPEN failed to Find and/or Open'//
     &                         ' the following file:')
                     call ftpmsg(fname)
                     return
                end if

C               set column descriptors as undefined
                call ftfrcl(funit,-999)

C               determine the structure and size of the primary HDU
                call ftrhdu(funit,xtend,status)
                if (status .gt. 0)then
                  call ftpmsg('FTOPEN could not interpret primary '
     &              //'array header keywords of file:')
                  call ftpmsg(fname)
                  if (status .eq. 252)then
                      call ftpmsg('Is this a FITS file??')
                  end if
                end if

C               set current column name buffer as undefined
                call ftrsnm
                return
            end if
10      continue

C       if we got here, then the input filename was all blanks
        status=104
        call ftpmsg('FTOPEN: Name of file to open is blank.')
        return

        end
