C--------------------------------------------------------------------------
        subroutine ftgcno(iunit,casesn,templt,colnum,status)

C       determine the column number corresponding to an input column name.
C       This supports the * and ? wild cards in the input template.

C       iunit   i  Fortran i/o unit number
C       casesn  l  true if an exact case match of the names is required
C       templt  c  name of column as specified in a TTYPE keyword
C       colnum  i  number of the column (first column = 1)
C                       (a value of 0 is returned if the column is not found)
C       status  i  returned error status

C       modified by Wm Pence, HEASARC/GSFC, December 1994

        integer iunit,colnum,status
        character*(*) templt
        logical casesn
        character*8 dummy

        call ftgcnn(iunit,casesn,templt,dummy,colnum,status)
        end
