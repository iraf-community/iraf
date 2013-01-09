include "fitsio.h"

procedure fswldp(xpix,ypix,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,coord,
                 xpos,ypos,status) 

double  xpix,ypix,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,xpos,ypos
char    coord[4]
%       character fcoord*4
int     status          # o error status

begin

call f77pak(coord,fcoord,4)
call ftwldp(xpix,ypix,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,fcoord,
             xpos,ypos,status)

end
