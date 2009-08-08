C--------------------------------------------------------------------------
        subroutine ftgkyl(iunit,keywrd,logval,comm,status)

C       read a logical value and the comment string from a header record 
C
C       iunit   i  fortran input unit number
C       keywrd  c  keyword name   
C       OUTPUT PARAMETERS:
C       logval  l  output keyword value 
C       comm    c  output keyword comment
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        integer iunit,status
        character value*20
        logical logval

C       find the keyword and return value and comment as character strings
        call ftgkey(iunit,keywrd,value,comm,status)

C       convert character string to logical
        call ftc2l(value,logval,status)
        end
