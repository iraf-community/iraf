C----------------------------------------------------------------------
        subroutine ftc2ii(cval,ival,status)
C       convert a character string to an integer
C       (assumes that the input string is left justified)

        integer ival,status,nleng
        character*(*) cval
        character*8 iform

        if (status .gt. 0)return

        if (cval .eq. ' ')go to 900

C       find length of the input integer character string
        nleng=index(cval,' ')-1
        if (nleng .eq. -1)nleng=len(cval)

C       construct the format statement to read the character string
        if (nleng .le. 9)then
                write(iform,1000)nleng
1000            format('(I',I1,')')
        else
                write(iform,1001)nleng
1001            format('(I',I2,')')
        end if

        read(cval,iform,err=900)ival
        return

900     continue
C       work around for bug in the DEC Alpha VMS compiler
        if (cval(1:nleng) .eq. '-2147483648')then
                 ival=-2147483647 - 1
        else
                 status=407
        end if
        end
