# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<gset.h>
include	<gio.h>

# GPAGEFILE -- File pager which works in or out of cursor mode.  If in graphics
# mode, the workstation is deactivated, the file paged, and graphics mode later
# restored.

procedure gpagefile (gp, fname, prompt)

pointer	gp			# graphics descriptor
char	fname[ARB]		# name of file to be paged
char	prompt[ARB]		# user prompt string

bool	wsactive
int	and()

begin
	wsactive = (and (GP_GFLAGS(gp), GF_WSACTIVE) != 0)

	if (wsactive)
	    call gdeactivate (gp, 0)
	iferr (call pagefile (fname, prompt))
	    call erract (EA_WARN)
	if (wsactive)
	    call greactivate (gp, AW_PAUSE)
end
