C----------------------------------------------------------------------
        subroutine ftc2r(cval,rval,status)
C       convert a character string to a real value
C       perform datatype conversion, if required

        character*(*) cval
        real rval
        integer ival,status
        character*1 dtype
        logical lval
        character*16 sval
        double precision dval


C       convert string to its intrinsic data type
        call ftc2x(cval,dtype,ival,lval,sval,dval,status)
        if (status .gt. 0)return

        if (dtype .eq. 'F')then
C               convert from double to single precision
                rval=dval
        else if (dtype .eq. 'I')then
C               convert from integer to real
                rval=ival
        else if (dtype .eq. 'L')then
C               need to convert from logical to real
                if (lval)then
                        rval=1.
                else
                        rval=0.
                end if
        else if (dtype .eq. 'C')then
C               can't convert a string to a real, so return error
                rval=0
                status=405
                sval=cval
                call ftpmsg('Error in FTC2R evaluating this string '//
     &          'as a real value: '//sval)
        end if
        end
