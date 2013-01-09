C----------------------------------------------------------------------
        subroutine ftghpr(iunit,maxdim,simple,bitpix,naxis,naxes,
     &                    pcount,gcount,extend,status)

C       get the required primary header or image extension keywords
C
C       iunit   i  fortran unit number to use for reading
C       maxdim  i  maximum no. of dimensions to read; dimension of naxes
C       OUTPUT PARAMETERS:
C       simple  l  does file conform to FITS standard?
C       bitpix  i  number of bits per data value
C       naxis   i  number of axes in the data array
C       naxes   i  array giving the length of each data axis
C       pcount  i  number of group parameters (usually 0)
C       gcount  i  number of random groups (usually 1 or 0)
C       extend  l  may extensions be present in the FITS file?
C       status  i  output error status (0=OK)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,bitpix,naxis,naxes(*),pcount,gcount,blank,status
        integer maxdim,nblank
        logical simple,extend
        double precision fill
        
        call ftgphx(iunit,maxdim,simple,bitpix,naxis,naxes,
     &        pcount,gcount,extend,fill,fill,blank,nblank,status)
        end
