C----------------------------------------------------------------------
        subroutine fti2c(ival,cval,status)
C       convert an integer value to a C*20 character string, right justified
        integer ival,status
        character*20 cval

        if (status .gt. 0)return

        write(cval,1000,err=900)ival
1000    format(i20)
        if (cval(1:1) .eq. '*')go to 900
        return
900     status=401
        call ftpmsg('Error in FTI2C converting integer to C*20 string.')        
        end
