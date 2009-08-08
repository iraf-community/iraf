C--------------------------------------------------------------------------
        subroutine ftmnam(ounit,oldkey,newkey,status)

C       modify (overwrite) the name of an existing keyword, preserving
C       the current value and comment fields
C
C       ounit   i  fortran output unit number
C       oldkey  c  old keyword name    ( 8 characters, cols.  1- 8)
C       newkey  c  new keyword name to be written
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Feb 1992

        character*(*) oldkey,newkey
        integer ounit,status
        character card*80
        
        if (status .gt. 0)return

C       find the old keyword string
        call ftgcrd(ounit,oldkey,card,status)
        
        card(1:8)=newkey

C       make sure new keyword name is in upper case
        call ftupch(card(1:8))

C       test that keyword name contains only legal characters
        call fttkey(card(1:8),status)

C       write the new keyword record
        call ftmodr(ounit,card,status)
        end
