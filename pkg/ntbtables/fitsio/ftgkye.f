C--------------------------------------------------------------------------
        subroutine ftgkye(iunit,keywrd,rval,comm,status)

C       read a real*4 value and the comment string from a header record 
C
C       iunit   i  fortran input unit number
C       keywrd  c  keyword name   
C       OUTPUT PARAMETERS:
C       rval    r  output keyword value 
C       comm    c  output keyword comment
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        integer iunit,status
        character value*35
        real rval

C       find the keyword and return value and comment as character strings
        call ftgkey(iunit,keywrd,value,comm,status)

C       convert character string to real
C       datatype conversion will be performed if necessary and if possible
        call ftc2r(value,rval,status)
        end
