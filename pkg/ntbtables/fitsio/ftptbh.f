C----------------------------------------------------------------------
        subroutine ftptbh(ounit,ncols,nrows,nfield,ttype,tbcol,
     &  tform,tunit,extnam,status)

C       OBSOLETE routine: should call ftphtb instead

        integer ounit,ncols,nrows,nfield,tbcol(*),status
        character*(*) ttype(*),tform(*),tunit(*),extnam

        call ftphtb(ounit,ncols,nrows,nfield,ttype,tbcol,
     &  tform,tunit,extnam,status)
        end
