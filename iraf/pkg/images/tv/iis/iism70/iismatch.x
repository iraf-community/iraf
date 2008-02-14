# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include "iis.h"
include "../lib/ids.h"

# IISMATCH -- copy (match) a set of look up tables to a given table;
# frames/color specify the given table, data gives frame/color for
# set to be changed.

procedure iismatch (code, frames, color, n, data)

short	code			# which table type
short	frames[ARB]		# reference frame
short	color[ARB]		# reference color
short	n			# count of data items
short	data[ARB]		# frame/color to be changed.

pointer	sp, ldata
int	len, x,y,z,t
int	unit, i
int	mapcolor(), ids_dcopy()
short	temp[IDS_MAXIMPL+1]
short	iispack()

include	"../lib/ids.com"

begin
	switch (code) {
	    case IDS_FRAME_LUT:
		len = LEN_LUT
		x = ADVXONTC
		y = 0
		z = mapcolor (color)
		t = iispack (frames)
		if (t == GRCHAN)
		    return
		unit = LUT

	    case IDS_OUTPUT_LUT:
		len = LEN_OFM
		x = ADVXONTC
		y = ADVYONXOV
		z = mapcolor (color)
		t = 0

	    default:
		return
	}

	call smark (sp)
	call salloc (ldata, len, TY_SHORT)

	call iishdr (IREAD+VRETRACE, len, unit, x, y, z, t)
	call iisio (Mems[ldata], len * SZB_CHAR)

	i = ids_dcopy (data, temp)
	switch (code) {
	    case IDS_FRAME_LUT:
		call ids_expand (temp, i_maxframes, true)
		t = iispack (temp)
		i = ids_dcopy (data[i+1], temp)
		call ids_expand (temp, 3, false)	# 3...max colors
		z = mapcolor (temp)

	    case IDS_OUTPUT_LUT:
		i = ids_dcopy (data[i+1], temp)
		call ids_expand (temp, 3, false)
		z = mapcolor (temp)
	}

	call iishdr (IWRITE+VRETRACE, len, unit, x, y, z, t)
	call iisio (Mems[ldata], len * SZB_CHAR)

	call sfree (sp)
end
