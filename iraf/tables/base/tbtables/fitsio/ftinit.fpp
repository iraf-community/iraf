C--------------------------------------------------------------------------
        subroutine ftinit(funit,fname,block,status)

C       open a new FITS file with write access
C
C       funit   i  Fortran I/O unit number
C       fname   c  name of file to be opened
C       block   i  input record length blocking factor
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer funit,status,block,strlen,i
        character*(*) fname

        if (status .gt. 0)return

C       ignore any leading blanks in the file name
        strlen=len(fname)
        do 10 i=1,strlen
            if (fname(i:i) .ne. ' ')then

C               call the machine dependent routine which creates the file
                call ftopnx(funit,fname(i:),1,1,block,status)
                if (status .gt. 0)then
         call ftpmsg('FTINIT failed to create the following new file:')
         call ftpmsg(fname)
                    return
                end if

C               set column descriptors as undefined
                call ftfrcl(funit,-999)

C               set current column name buffer as undefined
                call ftrsnm
                return
            end if
10      continue

C       if we got here, then the input filename was all blanks
        status=105
        call ftpmsg('FTINIT: Name of file to create is blank.')
        end
