C----------------------------------------------------------------------
        subroutine ftgprh(iunit,simple,bitpix,naxis,naxes,
     &                    pcount,gcount,extend,status)

C       OBSOLETE routine: should call ftghpr instead

        integer iunit,bitpix,naxis,naxes(*),pcount,gcount,blank,status
        integer nblank
        logical simple,extend
        double precision fill
        
        call ftgphx(iunit,0,simple,bitpix,naxis,naxes,
     &        pcount,gcount,extend,fill,fill,blank,nblank,status)
        end
