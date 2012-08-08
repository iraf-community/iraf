include <gset.h>

# Default viewport edges.
define  EDGE1  0.1
define  EDGE2  0.9
define  EDGE3  0.12
define  EDGE4  0.85

#---------------------------------------------------------------------------
.help sp_map_viewport Sep92 source
.ih
NAME
sp_map_viewport -- set device viewport for contour plots.
.endhelp
#---------------------------------------------------------------------------

procedure sp_map_viewport (gp, ncols, nlines, ux1, ux2, uy1, uy2, pre, perim)

pointer gp                      # I:  pointer to graphics descriptor
real    ncols, nlines           # I:  size of image area, after block reduction
real    ux1, ux2, uy1, uy2      # I:  NDC coordinates of requested viewort
bool    pre                     # I:  Preserve aspect ratio.
bool    perim                   # I:  draw perimeter

real  xcen, ycen, x, y
real  aspect_ratio
real  x1, x2, y1, y2, ext, xdis, ydis
data    ext /0.0625/
real  ggetr()

begin
        # Determine the standard window sizes.
        if (!pre && !perim) {
            x1 = 0.0;  x2 = 1.0
            y1 = 0.0;  y2 = 1.0
        } else {
            x1 = EDGE1;  x2 = EDGE2
            y1 = EDGE3;  y2 = EDGE4
            
        }
        
        # If any values were specified, then replace them here.
        if( !IS_INDEFR( ux1 ) )
            x1 = ux1
        if( !IS_INDEFR( ux2 ) )
            x2 = ux2
        if( !IS_INDEFR( uy1 ) )
            y1 = uy1
        if( !IS_INDEFR( uy2 ) )
            y2 = uy2
        
        xdis = x2 - x1
        ydis = y2 - y1
        xcen = ( x2 + x1 ) / 2.
        ycen = ( y2 + y1 ) / 2.
        
        # So far, the viewport has been calculated so that equal numbers of
        # image pixels map to equal distances in NDC space, regardless of 
        # the aspect ratio of the device.  If preserving aspect ratio,
        # modify viewport to correctly display the contour aspect.
        if (pre) {
            aspect_ratio = ggetr (gp, "ar")
            if (aspect_ratio == 0.0) {
                x = ggetr (gp, "xr")
                y = ggetr (gp, "yr")
                if ( x != 0.0 && y != 0.0)
                    aspect_ratio = y / x
                else
                    aspect_ratio = 1.0
            }
            aspect_ratio = nlines / ncols / aspect_ratio
            x = ydis / aspect_ratio
            y = ydis
            if ( x > xdis) {
                y = aspect_ratio * xdis
                x = xdis
            }
            xdis = x
            ydis = y
        }

        # All set.
        ux1 = xcen - (xdis / 2.0)
        ux2 = xcen + (xdis / 2.0)
        uy1 = ycen - (ydis / 2.0)
        uy2 = ycen + (ydis / 2.0)
        
        call gsview (gp, ux1, ux2, uy1, uy2)
        call gswind (gp, 1.0, ncols, 1.0, nlines)
        
end
#---------------------------------------------------------------------------
# End of sp_map_viewport
#---------------------------------------------------------------------------
