C----------------------------------------------------------------------
        subroutine ftgcve(iunit,colnum,frow,felem,nelem,nulval,array,
     &          anynul,status)

C       read an array of R*4 values from a specified column of the table.
C       Any undefined pixels will be set equal to the value of NULVAL,
C       unless NULVAL=0, in which case no checks for undefined pixels
C       will be made.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       nulval  r  value that undefined pixels will be set to
C       array   r  returned array of data values that was read from FITS file
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical flgval,anynul
        real array(*),nulval

        call ftgcle(iunit,colnum,frow,felem,nelem,1,1,nulval,
     &      array,flgval,anynul,status)
        end        
