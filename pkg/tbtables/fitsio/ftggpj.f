C----------------------------------------------------------------------
        subroutine ftggpj(iunit,group,fparm,nparm,array,status)

C       Read an array of group parameter values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       iunit   i  Fortran unit number
C       group   i  number of the data group, if any
C       fparm   i  the first group parameter be read (starting with 1)
C       nparm   i  number of group parameters to be read
C       array   i  returned array of values that were read
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,group,fparm,nparm,status,row 
        integer nulval,array(*)
        logical anynul,flgval

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
C       set nulval to blank to inhibit checking for undefined values
        nulval=0
        row=max(1,group)
        call ftgclj(iunit,1,row,fparm,nparm,1,1,nulval,
     &      array,flgval,anynul,status)
        end
