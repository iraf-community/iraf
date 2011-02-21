C--------------------------------------------------------------------------
        subroutine ftmcrd(ounit,keywrd,card,status)

C       modify (overwrite) a given header record specified by keyword name.
C       This can be used to overwrite the name of the keyword as well as
C       the value and comment fields.
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       card    c  new 80-character card image to be written
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Feb 1992

        character*(*) keywrd,card
        integer ounit,status
        character value*80
        
        if (status .gt. 0)return

C       find the old keyword string
        call ftgcrd(ounit,keywrd,value,status)
        
        value=card

C       make sure new keyword name is in upper case
        call ftupch(value(1:8))

C       test that keyword name contains only legal characters
        call fttkey(value(1:8),status)

C       write the new keyword record
        call ftmodr(ounit,value,status)
        end
