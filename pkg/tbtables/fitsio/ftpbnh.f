C----------------------------------------------------------------------
        subroutine ftpbnh(ounit,nrows,nfield,ttype,tform,tunit,
     &                    extnam,pcount,status)

C       OBSOLETE routine: should call ftphbn instead

        integer ounit,nrows,nfield,pcount,status
        character*(*) ttype(*),tform(*),tunit(*),extnam

        call ftphbn(ounit,nrows,nfield,ttype,tform,tunit,
     &                    extnam,pcount,status)
        end
