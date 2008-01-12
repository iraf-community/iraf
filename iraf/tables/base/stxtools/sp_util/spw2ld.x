# sp_w2ld - Transform world coordinates to logical coordinates (double).
#
# History
#   24Jun91 - Created by Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

procedure sp_w2ld( wlct, flip, wx, wy, lx, ly, npts )

pointer wlct                # I:  The MWCS coordinate trans. descriptor.
bool    flip                # I:  True if the axes are transposed.
double  wx[npts], wy[npts]  # I: The world coordinates.
double  lx[npts], ly[npts]  # O: The logical coordinates.
int     npts                # I: The number of points to translate.

begin

  if( flip )
    call mw_v2trand( wlct, wx, wy, ly, lx, npts )
  else
    call mw_v2trand( wlct, wx, wy, lx, ly, npts )

end
#---------------------------------------------------------------------------
# End of sp_w2ld
#---------------------------------------------------------------------------
# sp_l2wd - Transform logical coordinates to world coordinates (double).
#
# History
#   24Jun91 - Created by Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

procedure sp_l2wd( lwct, flip, lx, ly, wx, wy, npts )

pointer lwct                # I:  The MWCS coordinate trans. descriptor.
bool    flip                # I:  True if the axes are transposed.
double  lx[npts], ly[npts]  # I:  The logical coordinates.
double  wx[npts], wy[npts]  # O:  The world coordinates.
int     npts                # I:  The number of points to translate.

begin

  if( flip )
    call mw_v2trand( lwct, ly, lx, wx, wy, npts )
  else
    call mw_v2trand( lwct, lx, ly, wx, wy, npts )

end
#---------------------------------------------------------------------------
# End of sp_l2wd
#---------------------------------------------------------------------------
