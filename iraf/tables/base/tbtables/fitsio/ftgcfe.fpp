C----------------------------------------------------------------------
        subroutine ftgcfe(iunit,colnum,frow,felem,nelem,array,
     &          flgval,anynul,status)

C       read an array of R*4 values from a specified column of the table.
C       Any undefined pixels will be have the corresponding value of FLGVAL
C       set equal to .true., and ANYNUL will be set equal to .true. if
C       any pixels are undefined.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       array   r  returned array of data values that was read from FITS file
C       flgval  l  set .true. if corresponding element undefined
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical flgval(*),anynul
        real array(*),dummy
        integer i

        do 10 i=1,nelem
                flgval(i)=.false.
10      continue

        call ftgcle(iunit,colnum,frow,felem,nelem,1,2,dummy,
     &      array,flgval,anynul,status)
        end        
