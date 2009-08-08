C----------------------------------------------------------------------
        subroutine fttbit(bitpix,status)

C       test that bitpix has a legal value

        integer bitpix,status
        character value*20

        if (status .gt. 0)return

        if (bitpix .ne. 8 .and. bitpix .ne. 16 .and. bitpix .ne. 32
     &      .and. bitpix .ne. -32 .and. bitpix .ne. -64)then
                status=211
                write(value,1000)bitpix
1000            format(i20)
                call ftpmsg('Illegal BITPIX value: '//value)
        end if
        end
