include "fitsio.h"

procedure fsxypx(xpos,ypos,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,coord,
                xpix,ypix,status)

double  xpix,ypix,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,xpos,ypos
char    coord[4]
%       character fcoord*4
int     status          # o error status

begin

call f77pak(coord,fcoord,4)
call ftxypx(xpos,ypos,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,fcoord,
           xpix,ypix,status)

end
