C------------------------------------------------------------------------------
        subroutine ftgiou(iounit,status)

C       get an unallocated logical unit number

        integer iounit,status

        if (status .gt. 0)return
        iounit=0
        call ftxiou(iounit,status)
        end
