C----------------------------------------------------------------------
        subroutine ftl2c(lval,cval,status)
C       convert a logical value to a C*20 right justified character string 
        integer status
        logical lval
        character*20 cval

        if (status .gt. 0)return

        if (lval)then
                cval='                   T'
        else
                cval='                   F'
        end if
        end
