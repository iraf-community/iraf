C----------------------------------------------------------------------
        subroutine ftgpve(iunit,group,felem,nelem,nulval,
     &                    array,anynul,status)

C       Read an array of r*4 values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).
C       Undefined elements will be set equal to NULVAL, unless NULVAL=0
C       in which case no checking for undefined values will be performed.
C       ANYNUL is return with a value of .true. if any pixels were undefined.

C       iunit   i  Fortran unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be read (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be read
C       nulval  r  the value to be assigned to undefined pixels
C       array   r  returned array of values that were read
C       anynul  l  set to .true. if any returned elements were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,group,felem,nelem,status,row 
        real nulval,array(*)
        logical anynul,flgval

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(1,group)
        call ftgcle(iunit,2,row,felem,nelem,1,1,nulval,
     &      array,flgval,anynul,status)
        end
