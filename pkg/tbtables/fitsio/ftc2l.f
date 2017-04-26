C----------------------------------------------------------------------
        subroutine ftc2l(cval,lval,status)

C       convert a character string to a logical value
C       perform datatype conversion, if required

        logical lval
        integer ival,status
        character*(*) cval
        character*1 dtype
        character sval*16
        double precision dval


C       convert string to its intrinsic data type
        call ftc2x(cval,dtype,ival,lval,sval,dval,status)
        if (status .gt. 0)return

        if (dtype .ne. 'L')then
C              this is not a logical keyword, so return error
               status=404
               sval=cval
               call ftpmsg('Error in FTC2L evaluating this string '//
     &          'as a logical value: '//sval)
        end if
        end
