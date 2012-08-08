C----------------------------------------------------------------------
        subroutine ftc2ll(cval,lval,status)
C       convert a character string to a logical value 
C       (assumes that the input string is left justified)
        integer status
        logical lval
        character*(*) cval

        if (status .gt. 0)return

C       convert character string to logical
        if (cval(1:1) .eq.'T')then
                lval=.true.
        else
C               any other character is considered false
                lval=.false.
        end if
        end
