C--------------------------------------------------------------------------
        subroutine ftclos(iunit,status)

C       close a FITS file that was previously opened with ftopen or ftinit
C
C       iunit   i  Fortran I/O unit number
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,status

        logical keep

C       close the current HDU and pad the header with blanks
        call ftchdu(iunit,status)

C       close the file
        keep=.true.
        call ftclsx(iunit,keep,status)
        end
