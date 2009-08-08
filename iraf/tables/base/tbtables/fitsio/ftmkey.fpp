C--------------------------------------------------------------------------
        subroutine ftmkey(ounit,keywrd,value,comm,status)

C       modify an existing simple FITS keyword record with format:
C            "KEYWORD = VALUE / COMMENT"
C               VALUE is assumed to be 20 characters long
C               COMMENT is assumed to be 47 characters long
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       value   c  keyword value   (20 characters, cols. 11-30)
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,value,comm
        integer ounit,status
        character key*8, val*20, com*47

        key=keywrd
        val=value
        com=comm

C       overwrite the preceeding 80 characters to the output buffer:
        call ftmodr(ounit,key//'= '//val//' / '//com,status)
        end
