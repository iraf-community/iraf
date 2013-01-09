C----------------------------------------------------------------------
        subroutine ftc2i(cval,ival,status)
C       convert a character string to an integer
C       perform datatype conversion, if required

        integer ival,status
        character*(*) cval
        character*1 dtype
        logical lval
        character sval*16
        double precision dval

C       convert string to its intrinsic data type
        call ftc2x(cval,dtype,ival,lval,sval,dval,status)
        if (status .gt. 0)return

        if (dtype .eq. 'I')then
C               no datatype conversion required, so just return
        else if (dtype .eq. 'F')then
C               need to convert from floating point to integer
                ival=dval
        else if (dtype .eq. 'L')then
C               need to convert from logical to integer
                if (lval)then
                        ival=1
                else
                        ival=0
                end if
        else if (dtype .eq. 'C')then
C               can't convert a string to an integer, so return error
                ival=0
                status=403
                sval=cval
        call ftpmsg('Error in FTC2I evaluating this string as an '
     &  //'integer: '//sval)
        end if
        end
