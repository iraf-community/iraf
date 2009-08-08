C----------------------------------------------------------------------
        subroutine ftgpfb(iunit,group,felem,nelem,
     &                    array,flgval,anynul,status)

C       Read an array of byte values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).
C       Undefined elements will have the corresponding element of
C       FLGVAL set equal to .true.
C       ANYNUL is return with a value of .true. if any pixels were undefined.

C       iunit   i  Fortran unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be read (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be read
C       array   b  returned array of values that were read
C       flgval  l  set to .true. if the corresponding element is undefined
C       anynul  l  set to .true. if any returned elements are undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,group,felem,nelem,status,row 
        character*1 nulval,array(*)
        logical anynul,flgval(*)
        integer i

        do 10 i=1,nelem
                flgval(i)=.false.
10      continue

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(1,group)
        call ftgclb(iunit,2,row,felem,nelem,1,2,nulval,
     &      array,flgval,anynul,status)
        end
