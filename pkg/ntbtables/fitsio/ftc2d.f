C----------------------------------------------------------------------
        subroutine ftc2d(cval,dval,status)
C       convert a character string to a double precision value
C       perform datatype conversion, if required

        character*(*) cval
        integer ival,status
        character*1 dtype
        logical lval
        character*16 sval
        double precision dval


C       convert string to its intrinsic data type
        call ftc2x(cval,dtype,ival,lval,sval,dval,status)
        if (status .gt. 0)return

        if (dtype .eq. 'F')then
C               no datatype conversion required, so just return
        else if (dtype .eq. 'I')then
C               convert from integer to double precision
                dval=ival
        else if (dtype .eq. 'L')then
C               need to convert from logical to double precision
                if (lval)then
                        dval=1.
                else
                        dval=0.
                end if
        else if (dtype .eq. 'C')then
C               can't convert a string to double precision, so return error
                dval=0
                status=406
                sval=cval
                call ftpmsg('Error in FTC2D evaluating this string '//
     &          'as a double value: '//sval)
        end if
        end
