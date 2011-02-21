C----------------------------------------------------------------------
        subroutine ftr2e(val,dec,cval,status)

C       convert real value to E20.* format character string
C       val     r  input value to be converted 
C       dec     i  number of decimal places to display in output string
C       cval    c  output character string
C       status  i  output error status (0 = OK)

        real val
        integer dec,status
        character*20 cval,form*10

        if (status .gt. 0)return

        if (dec .ge. 1 .and. dec .le. 9)then
                write(form,2000)dec
2000            format('(1pe20.',i1,')')
        else if (dec .ge. 10 .and. dec .le. 13)then
                write(form,2001)dec
2001            format('(1pe20.',i2,')')
        else
C               illegal number of decimal places were specified
                status=411
                call ftpmsg('Error in FTR2E: number of decimal places '
     &                      //'is less than 1 or greater than 13.')
                return
        endif

        write(cval,form,err=900)val
        if (cval(1:1) .eq. '*')go to 900
        return

900     status=402
        call ftpmsg('Error in FTR2E converting real to E20. string.')
        end
