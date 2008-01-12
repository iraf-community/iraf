C----------------------------------------------------------------------
        subroutine ftpprh(ounit,simple,bitpix,naxis,naxes,
     &                    pcount,gcount,extend,status)

C       OBSOLETE routine: should call ftphpr instead

        integer ounit,bitpix,naxis,naxes(*),pcount,gcount,status
        logical simple,extend

        call ftphpr(ounit,simple,bitpix,naxis,naxes,
     &                    pcount,gcount,extend,status)
        end
