C----------------------------------------------------------------------
        subroutine ftc2rr(cval,val,status)

C       convert a character string to a real value
C       (assumes that the input string is left justified)
C       cval    c  input character string to be converted 
C       val     r  output value 
C       status  i  output error status (0 = OK)

        character*(*) cval
        real val
        integer status,nleng
        character iform*8,sval*16

        if (status .gt. 0)return

        if (cval .eq. ' ')go to 900

C       find length of the input real character string
        nleng=index(cval,' ')-1
        if (nleng .eq. -1)nleng=len(cval)

C       construct the format statement to read the character string
        if (nleng .le. 9)then
                write(iform,1000)nleng
1000            format('(F',I1,'.0)')
        else
                write(iform,1001)nleng
1001            format('(F',I2,'.0)')
        end if

        read(cval,iform,err=900)val
        return

900     status=408
        sval=cval
        call ftpmsg('Error in FTC2RR evaluating this string '//
     &       'as a real: '//sval)
        end
