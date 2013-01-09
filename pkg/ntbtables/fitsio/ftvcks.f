C----------------------------------------------------------------------
        subroutine ftvcks(iunit,dataok,hduok,status)

C       Verify the HDU by comparing the value of the computed checksums against
C       the values of the DATASUM and CHECKSUM keywords if they are present.

C       iunit   i  fortran unit number
C       dataok  i  output verification code for the data unit alone
C       hduok   i  output verification code for the entire HDU
C                  the code values = 1  verification is correct
C                                  = 0  checksum keyword is not present
C                                  = -1 verification not correct
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Dec, 1994

        integer iunit,dataok,hduok,status,tstat,i
        double precision datsum,chksum,dsum
        character keyval*20,comm*8
        logical cexist,dexist

        if (status .gt. 0)return

C       check if the CHECKSUM keyword exists
        tstat=status
        call ftgkys(iunit,'CHECKSUM',keyval,comm,status)
        if (status .le. 0)then
            cexist=.true.
        else
            hduok=0
            cexist=.false.
            status=tstat
        end if

C       check if the DATASUM keyword exists and get its value
        call ftgkys(iunit,'DATASUM',keyval,comm,status)
        if (status .le. 0)then
            dexist=.true.
        else
            dataok=0
            dexist=.false.
            status=tstat
        end if

C       return if neither keyword exists
        if (.not. cexist .and. .not. dexist)return
            
C       calculate the data checksum and the HDU checksum
        call ftgcks(iunit,datsum,chksum,status)
        if (status .gt. 0)return

        if (dexist)then

C           decode the datasum string into a double precision variable
            do 10 i=1,20
                if (keyval(i:i) .ne. ' ')then
                    call ftc2dd(keyval(i:20),dsum,status)
                    if (status .eq. 409)then
C                       couldn't read the keyword; assume it is out of date
                        status=tstat
                        dsum=-1.
                    end if
                    go to 15
                end if
10          continue
            dsum=0.
15          continue

            if (dsum .eq. datsum)then
                dataok=1
            else
                dataok=-1
            end if
        end if

        if (cexist)then
            if (chksum .eq. 0 .or. chksum .eq. 4.294967295D+09)then
                hduok=1
            else
                hduok=-1
            end if
        end if
        end
