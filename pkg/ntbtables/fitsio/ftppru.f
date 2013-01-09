C----------------------------------------------------------------------
        subroutine ftppru(ounit,group,felem,nelem,status)

C       set elements of the primary array equal to the undefined value

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be written (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be set to undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,felem,nelem,status,row

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(group,1)
        call ftpclu(ounit,2,row,felem,nelem,status)
        end
