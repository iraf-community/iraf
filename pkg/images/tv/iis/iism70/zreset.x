# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include <gki.h>
include "../lib/ids.h"
include "iis.h"

# cfactor is conversion from integer to NDC coordinates (max 32767) for cursor
# see iiscursor.x
# The "hardness" notion is now somewhat obsolete...a range of reset values
# would be better, especially if better named.

define	CFACTOR	528

# ZRESET -- reset IIS 

procedure zreset (hardness)

short	hardness			# soft, medium, hard

short	data[LEN_IFM]
short	frames[IDS_MAXIMPL+1]
short	colors[IDS_MAXGCOLOR+1]
short	quad[5]
int	i,j

include	"iis.com"

begin
	if ( hardness == IDS_R_SNAPDONE ) {
	    call zsnap_done
	    return
	}

	# mark all frames
	do i = 1,IDS_MAXIMPL
	    frames[i] = i
	frames[IDS_MAXIMPL+1] = IDS_EOD
	# mark all colors
	do i = 1, IDS_MAXGCOLOR
	    colors[i] = i
	colors[IDS_MAXGCOLOR+1] = IDS_EOD
	# all quadrants
	do i = 1,4
	    quad[i] = i
	quad[5] = IDS_EOD

	if ( hardness == IDS_R_SOFT) {
	    # all coordinates are NDC ( 0 - 32767 )
	    # Reseting the "soft" parameters: scroll, constant offsets,
	    # split point, alu, zoom; turn cursor and tball on.
	    	
	    # constants
	    call aclrs (data,3)
	    call iisoffset(short(IDS_WRITE), colors, short(3), data)

	    # range
	    data[1] = 1
	    call iisrange (short(IDS_WRITE), colors, short(1), data)

	    # split point
	    call aclrs ( data, 2)
	    call iissplit(short(IDS_WRITE), short(2), data)

	    # alu
	    data[1] = 0
	    call iishdr(IWRITE, 1, ALU+COMMAND, 0, 0, 0, 0)
	    call iisio (data, 1 * SZB_CHAR)

	    # graphics status register
	    data[1] = 0
	    call iishdr(IWRITE, 1, GRAPHICS+COMMAND, 0, 0, 0, 0)
	    call iisio (data, 1 * SZB_CHAR)

	    # zoom
	    data[1] = 1
	    data[2] = IIS_XCEN * MCXSCALE		# gki mid point
	    data[3] = IIS_YCEN * MCYSCALE
	    data[4] = IDS_EOD
	    call iiszoom(short(IDS_WRITE), frames, short(4), data)

	    # scroll -- screen center to be centered
	    # zoom does affect scroll if zoom not power==1
	    # so to be safe, do scroll after zoom.
	    data[1] = IIS_XCEN * MCXSCALE
	    data[2] = IIS_YCEN * MCYSCALE
	    data[3] = IDS_EOD
	    call iisscroll(short(IDS_WRITE), frames, short(3), data)

	    # cursor and tball; no blink for cursor
	    data[1] = IDS_ON
	    call iiscursor(short(IDS_WRITE), short(1), short(1), data)
	    call iistball (short(IDS_WRITE), data)
	    data[1] = IDS_CBLINK
	    data[2] = IDS_CSTEADY
	    call iiscursor(short(IDS_WRITE), short(1), short(1), data)
    
	    # standard cursor shape
	    data[1] = IDS_CSHAPE
	    j = 2
	    # don't use last line/column so have a real center
	    for ( i = 0 ; i <= 62 ; i = i + 1 ) {
	        # make the puka in the middle
	        if ( (i == 30) || (i == 31) || (i == 32) )
		    next
	        # fill in the lines
	        data[j] = 31 * CFACTOR
	        data[j+1] = i * CFACTOR 
	        j = j + 2
	        data[j] = i * CFACTOR
	        data[j+1] = 31 * CFACTOR
	        j = j + 2
	    }
	    data[j] = IDS_EOD
	    call iiscursor ( short(IDS_WRITE), short(1), short(j), data)

	    return
	}
    
	if ( hardness == IDS_R_MEDIUM) {
	    # reset all tables to linear--ofm, luts, ifm
	    # ofm  (0,0) to (0.25,1.0) to (1.0,1.0)
	    data[1] = 0
	    data[2] = 0
	    data[3] = 0.25 * GKI_MAXNDC
	    data[4] = GKI_MAXNDC
	    data[5] = GKI_MAXNDC
	    data[6] = GKI_MAXNDC
	    call iisofm(short(IDS_WRITE), colors, short(1), short(6), data)

	    # luts
	    data[1] = 0
	    data[2] = 0
	    data[3] = GKI_MAXNDC
	    data[4] = GKI_MAXNDC
	    call iislut(short(IDS_WRITE), frames, colors, short(1),
	             short(4), data)

	    # ifm (0,0) to (1/32, 1.0) to (1.,1.)
	    # ifm is length 8192, but output is only 255.  So map linearly for
	    # first 256, then flat.  Other possibility is ifm[i] = i-1 ( for
	    # i = 1,8192) which relies on hardware dropping high bits.

	    data[1] = 0
	    data[2] = 0
	    data[3] = (1./32.) * GKI_MAXNDC
	    data[4] = GKI_MAXNDC
	    data[5] = GKI_MAXNDC
	    data[6] = GKI_MAXNDC
	    call iisifm(short(IDS_WRITE), short(1), short(6), data)

	    return
	}

	if (hardness == IDS_R_HARD) {
	    # clear all image/graph planes, and set channel selects to
	    # mono
	    call zclear(frames, frames, true)
	    call zclear(frames, frames, false)
	    # reset all to no display 
	    call zdisplay_i(short(IDS_OFF), frames, colors, quad)
	    call zdisplay_g(short(IDS_OFF), frames, colors, quad)
	}
end
