# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "../lib/ids.h"
include "iis.h"

# ZCONTROL -- call the device dependent control routines

procedure zcontrol(device, rw, frame, color, offset, n, data)

short	device			# which device/register to control
short	rw			# write/read/wait,read
short	frame[ARB]		# array of image frames
short	color[ARB]		# array of color
short	offset			# generalized offset or datum
short	n			# count of items in data array
short	data[ARB]		# data array

begin
	switch(device) {
	    case IDS_FRAME_LUT:
		call iislut(rw, frame, color, offset, n, data)

	    case IDS_GR_MAP:
		# for now, nothing

	    case IDS_INPUT_LUT:
		call iisifm(rw, offset, n, data)

	    case IDS_OUTPUT_LUT:
		call iisofm(rw, color, offset, n, data)

	    case IDS_SPLIT:
		call iissplit(rw, n, data)

	    case IDS_SCROLL:
		call iisscroll(rw, frame, n, data)

	    case IDS_ZOOM:
		call iiszoom(rw, frame, n, data)

	    case IDS_OUT_OFFSET:
		call iisoffset(rw, color, n, data)

	    case IDS_MIN:
		call iismin(rw, color, n, data)

	    case IDS_MAX:
		call iismax(rw, color, n, data)

	    case IDS_RANGE:
		call iisrange(rw, color, n, data)

	    case IDS_HISTOGRAM:
		call iishisto(rw, color, offset, n, data)

	    case IDS_ALU_FCN:
		# for now, nothing

	    case IDS_FEEDBACK:
		# for now, nothing

	    case IDS_SLAVE:
		# for now, nothing

	    case IDS_CURSOR:
		call iiscursor(rw, offset, n, data)

	    case IDS_TBALL:
		call iistball(rw, data)

	    case IDS_DIGITIZER:
		# for now, nothing

	    case IDS_BLINK:
		# for now, nothing

	    case IDS_SNAP:
		call zsnap_init(data[1])

	    case IDS_MATCH:
		call iismatch (rw, frame, color, n, data)
	}
end


# MAPCOLOR - modify the color array to map rgb for iis

int procedure mapcolor(color)

short	color[ARB]			# input data

int	i
int	val, result
int	or()

begin
	result = 0
	for ( i = 1; color[i] != IDS_EOD ; i = i + 1 ) {
	    val = color[i]
	    switch (val) {
		case IDS_RED:
		    val = RED

		case IDS_GREEN:
		    val = GREEN

		case IDS_BLUE:
		    val = BLUE

		default:
		    val = 2**(val-1)
	    }
	    result = or (result, val)
	}
	return (result)
end
