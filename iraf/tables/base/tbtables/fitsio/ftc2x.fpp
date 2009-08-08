C----------------------------------------------------------------------
        subroutine ftc2x(cval,dtype,ival,lval,sval,dval,status)

C       convert a character string into it intrinsic data type

C       cval  c  input character string to be converted
C       dtype c  returned intrinsic datatype of the string (I,L,C,F)
C
C       one of  the following values is returned, corresponding to the
C       value of dtype:
C               ival i integer value
C               lval l logical value
C               sval c string value
C               dval d double precision value
C       statue i returned error status

        character*(*) cval
        character*1 dtype
        integer ival,status
        logical lval
        character*(*) sval
        double precision dval

C       determine intrinsic datatype
        call ftdtyp(cval,dtype,status)

C       convert string into its intrinsic datatype
        if (dtype .eq. 'I')then
                call ftc2ii(cval,ival,status)
        else if (dtype .eq. 'F')then
                call ftc2dd(cval,dval,status)
        else if (dtype .eq. 'L')then
                call ftc2ll(cval,lval,status)
        else if (dtype .eq. 'C')then
                call ftc2s(cval,sval,status)
        end if
        end
