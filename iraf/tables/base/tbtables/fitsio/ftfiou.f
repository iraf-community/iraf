C------------------------------------------------------------------------------
        subroutine ftfiou(iounit,status)

C       free specified logical unit number; if iounit=-1, then free all units

        integer iounit,status

        if (status .gt. 0)return

        call ftxiou(iounit,status)
        end
