# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gtools.h"

# GT_GCUR1 -- Interface to clgcur to confirm EOF, map key 'q' to EOF.
# Transposes X and Y if needed.

int procedure gt_gcur1 (gt, cur, wx, wy, wcs, key, cmd, sz_cmd)

pointer	gt			# GTOOLS pointer
char	cur[ARB]		# Cursor parameter
real	wx, wy			# Cursor position
int	wcs, key		# WCS and cursor key
char	cmd[sz_cmd]		# Command string
int	sz_cmd			# Size of command string

int	curval, clgcur()
real	temp

begin
	curval = clgcur (cur, wx, wy, wcs, key, cmd, sz_cmd)

	if (curval == EOF) {
	    curval = clgcur (cur, wx, wy, wcs, key, cmd, sz_cmd)
	    if (curval != EOF) {
		if (key == 'q')
		    curval = EOF
	    }
	} else if (key == 'q')
	    curval = EOF

	if (GT_TRANSPOSE(gt) == YES) {
	    temp = wx
	    wx = wy
	    wy = temp
	}
	return (curval)
end
