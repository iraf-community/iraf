C----------------------------------------------------------------------
        subroutine ftpgpe(ounit,group,fparm,nparm,array,status)

C       Write an array of group parmeters into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       fparm   i  the first group parameter to be written (starting with 1)
C       nparm   i  number of group parameters to be written
C       array   r  the array of group parameters to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,fparm,nparm,status,row
        real array(*)

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(group,1)
        call ftpcle(ounit,1,row,fparm,nparm,array,status)
        end
