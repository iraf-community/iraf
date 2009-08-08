C----------------------------------------------------------------------
        subroutine ftpprj(ounit,group,felem,nelem,array,status)

C       Write an array of i*4 values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be written (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be written
C       array   i  the array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,felem,nelem,status,row
        integer array(*)

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(group,1)
        call ftpclj(ounit,2,row,felem,nelem,array,status)
        end
