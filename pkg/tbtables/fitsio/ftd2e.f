C----------------------------------------------------------------------
        subroutine ftd2e(val,dec,cval,vlen,status)

C       convert a double precision value to an E format character string
C       If it will fit, the value field will be 20 characters wide;
C       otherwise it will be expanded to up to 35 characters, left
C       justified.
C
C       val     d  input value to be converted 
C       dec     i  number of decimal places to display in output string
C       cval    c  output character string
C       vlen    i  length of output string
C       status  i  output error status (0 = OK)

        double precision val
        integer dec,vlen,status
        character*35 cval,form*10

        if (status .gt. 0)return

        if (dec .ge. 1 .and. dec .le. 9)then
                vlen=20
                write(form,2000)dec
2000            format('(1pe20.',i1,')')
        else if (dec .ge. 10 .and. dec .le. 28)then
                vlen=max(20,dec+7)
                write(form,2001)vlen,dec
2001            format('(1pe',i2,'.',i2,')')
        else
C               illegal number of decimal places were specified
                status=411
                call ftpmsg('Error in FTR2E: number of decimal places '
     &                      //'is less than 1 or greater than 28.')
                return
        endif

        write(cval,form,err=900)val
        if (cval(1:1) .eq. '*')go to 900
        return

900     status=402
        call ftpmsg('Error in FTD2E converting double to En.m string.')
        end
