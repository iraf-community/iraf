# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GT_GCUR -- Interface to clgcur to confirm EOF, map key 'q' to EOF.

int procedure gt_gcur (cur, wx, wy, wcs, key, cmd, sz_cmd)

char	cur[ARB]		# Cursor parameter
real	wx, wy			# Cursor position
int	wcs, key		# WCS and cursor key
char	cmd[sz_cmd]		# Command string
int	sz_cmd			# Size of command string

int	curval, clgcur()

begin
	curval = clgcur (cur, wx, wy, wcs, key, cmd, sz_cmd)
	if (key == 'q')
	    curval = EOF

	return (curval)
end
