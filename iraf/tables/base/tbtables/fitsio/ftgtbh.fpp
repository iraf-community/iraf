C----------------------------------------------------------------------
        subroutine ftgtbh(iunit,ncols,nrows,nfield,ttype,tbcol,
     &                    tform,tunit,extnam,status)

C       OBSOLETE routine: should call ftghtb instead

        integer iunit,ncols,nrows,nfield,status,tbcol(*)
        character*(*) ttype(*),tform(*),tunit(*),extnam

        call ftghtb(iunit,0,ncols,nrows,nfield,ttype,
     &                    tbcol,tform,tunit,extnam,status)
        end
