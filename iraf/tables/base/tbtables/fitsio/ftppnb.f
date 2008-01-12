C----------------------------------------------------------------------
        subroutine ftppnb(ounit,group,felem,nelem,array,nulval,status)

C       Write an array of c*1 (byte) values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same as the
C       array being written).  Any input pixels equal to the value of NULVAL
C       will be replaced by the appropriate null value in the output FITS file.

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be written (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be written
C       array   c*1  the array of values to be written
C       nulval  c*1  pixel value used to represent an undefine pixel
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1994

        integer ounit,group,felem,nelem,status,row
        character*1 array(*),nulval

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(group,1)
        call ftpcnb(ounit,2,row,felem,nelem,array,nulval,status)
        end
