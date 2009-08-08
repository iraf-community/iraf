C------------------------------------------------------------------------------
        subroutine ftxiou(iounit,status)

C       generic routine to manage logical unit numbers in the range 50-99

        integer iounit,status,i
        integer*2 array(50)
        save array
        data array/50*0/

        if (iounit .eq. 0)then
C           get an unused logical unit number
            do 10 i=50,1,-1
                 if (array(i) .eq. 0)then
                     array(i)=1
                     iounit=i+49
                     return
                 end if
10          continue
C           error: all units are allocated
            iounit=-1
            status=114
            call ftpmsg('FTGIOU has no more available unit numbers.')

        else if (iounit .eq. -1)then
C           deallocate all the unit numbers
            do 20 i=1,50
                 array(i)=0
20          continue

        else
C            deallocat a specific unit number
             if (iounit .ge. 50 .and. iounit .le. 99)then
                 array(iounit-49)=0
             end if
        endif
        end
