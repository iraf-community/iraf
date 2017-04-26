C----------------------------------------------------------------------
        subroutine ftgbnh(iunit,nrows,nfield,ttype,tform,tunit,
     &                    extnam,pcount,status)

C       OBSOLETE routine: should call ftghbn instead

        integer iunit,nrows,nfield,pcount,status
        character*(*) ttype(*),tform(*),tunit(*),extnam

        call ftghbn(iunit,-1,nrows,nfield,ttype,tform,
     &                    tunit,extnam,pcount,status)
        end
