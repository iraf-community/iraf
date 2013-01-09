include "fitsio.h"

procedure fsgtcs(iunit,xcol,ycol,xrval,yrval,xrpix,yrpix,xinc,yinc,
           rot,coord,status)

int     iunit,xcol,ycol
double  xrval,yrval,xrpix,yrpix,xinc,yinc,rot
char    coord[4]
%       character fcoord*4
int     status          # o error status

begin

call ftgtcs(iunit,xcol,ycol,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,
            fcoord,status)
call f77upk(fcoord,coord,4)

end
