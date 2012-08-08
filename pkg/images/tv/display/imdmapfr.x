# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imset.h>
include <imhdr.h>
include <mach.h>
include <fset.h>
include	"display.h"
include	"iis.h"

# IMD_MAPFRAME -- Open the given frame of the stdimage display device on an
# IMIO image descriptor.

pointer procedure imd_mapframe (frame, mode, select_frame)

int	frame			#I frame to be opened [1:N]
int	mode			#I access mode
int	select_frame		#I make frame the display frame

pointer	ds
int	chan[MAXCHAN]
char	device[SZ_FNAME]

pointer	imdmap()
extern	imdopen()
int	imstati(), fstati(), envgets()
errchk	imdmap, imseti
include	"iis.com"

begin
	if (envgets ("stdimage", device, SZ_FNAME) == 0)
	    call error (1, "variable `stdimage' not defined in environment")

	# Pass frame number into IIS code.
	call iis_setframe (frame)

	# Map the frame onto an image descriptor.
	ds = imdmap (device, mode, imdopen)
	# call imseti (ds, IM_CLOSEFD, YES)
	chan[1] = fstati (imstati (ds, IM_PIXFD), F_CHANNEL)
	chan[2] = MONO

	# Pick up the frame size.
	iis_xdim   = IM_LEN(ds,1)
	iis_ydim   = IM_LEN(ds,2)
	iis_config = IM_LEN(ds,3)

	# Optimize for sequential i/o.
	call imseti (ds, IM_ADVICE, SEQUENTIAL)

	# Display frame being loaded?
	if (select_frame == YES)
	    call zfrmim (chan)

	return (ds)
end

# IMD_MAPFRAME1 -- Open the given frame of the stdimage display device on an
# IMIO image descriptor.
# This differs from imd_mapframe only in the addition of the erase option.

pointer procedure imd_mapframe1 (frame, mode, select_frame, erase)

int	frame			#I frame to be opened [1:N]
int	mode			#I access mode
int	select_frame		#I make frame the display frame
int	erase			#I erase frame

pointer	ds
int	chan[MAXCHAN]
char	device[SZ_FNAME]

pointer	imdmap()
extern	imdopen()
int	imstati(), fstati(), envgets()
errchk	imdmap, imseti
include	"iis.com"

begin
	if (envgets ("stdimage", device, SZ_FNAME) == 0)
	    call error (1, "variable `stdimage' not defined in environment")

	# Pass frame number into IIS code.
	call iis_setframe (frame)

	# Map the frame onto an image descriptor.
	ds = imdmap (device, mode, imdopen)
	# call imseti (ds, IM_CLOSEFD, YES)
	chan[1] = fstati (imstati (ds, IM_PIXFD), F_CHANNEL)
	chan[2] = MONO

	# Pick up the frame size.
	iis_xdim   = IM_LEN(ds,1)
	iis_ydim   = IM_LEN(ds,2)
	iis_config = IM_LEN(ds,3)

	# Optimize for sequential i/o.
	call imseti (ds, IM_ADVICE, SEQUENTIAL)

	# Display frame being loaded?
	if (select_frame == YES)
	    call zfrmim (chan)

	# Erase frame being loaded?
	if (erase == YES)
	    call zersim (chan)

	return (ds)
end
