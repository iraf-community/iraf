C----------------------------------------------------------------------
        subroutine ftd2f(val,dec,cval,status)

C       convert double precision value to F20.* format character string
C       NOTE: some precision may be lost
C       val     d  input value to be converted 
C       dec     i  number of decimal places to display in output string
C       cval    c  output character string
C       status  i  output error status (0 = OK)

        double precision val
        integer dec,status
        character*20 cval,form*8

        if (status .gt. 0)return

        if (dec .ge. 0 .and. dec .le. 9)then
                write(form,2000)dec
2000            format('(f20.',i1,')')
        else if (dec .ge. 10 .and. dec .lt.18)then
                write(form,2001)dec
2001            format('(f20.',i2,')')
        else
C               illegal number of decimal places were specified
                status=411
                call ftpmsg('Error in FTD2F: number of decimal places '
     &                      //'is less than 0 or greater than 18.')
                return
        endif

        write(cval,form,err=900)val
        if (cval(1:1) .eq. '*')go to 900
        return
900     status=402
        call ftpmsg('Error in FTD2F converting double to F20. string.')
        end
