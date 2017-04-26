# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<gio.h>
include "icfit.h"

define	CMDS "|open|close|params|graph|wcs|refit|help|"

define	OPEN		1	# Open GUI and send initial parameters
define	CLOSE		2	# Close GUI and send final parameters
define	PARAMS		3	# Send new parameters
define	GRAPH		4	# Send graph type parameters
define	WCS		5	# Send graph wcs parameters
define	REFIT		6	# Send refit flag
define	HELP		7	# Send help

# IC_GUI -- GUI interaction.
#
# Note there is currently an interface violation to determine if the graphics
# stream is connected to a GUI.

procedure ic_gui (ic, cmd)

pointer	ic		#I ICFIT pointer
char	cmd[ARB]	#I Command

int	ncmd, strdic()
real	vx1, vx2, vy1, vy2, wx1, wx2, wy1, wy2
pointer	sp, str, msg
bool	streq()
errchk	ic_help

begin
	if (IC_GP(ic) == NULL)
	    return

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Scan the command and switch on the first word.
	call sscan (cmd)
	call gargwrd (Memc[str], SZ_LINE)
	ncmd = strdic (Memc[str], Memc[str], SZ_LINE, CMDS)
	switch (ncmd) {
	case OPEN, CLOSE, PARAMS:
	    call salloc (msg, SZ_LINE+IC_SZSAMPLE, TY_CHAR)
	    call ic_gstr (ic, "function", Memc[str], SZ_LINE)
	    call sprintf (Memc[msg], SZ_LINE+IC_SZSAMPLE,
		"%s %s %d \"%s\" %d %d %g %g %g %b")
		call pargstr (cmd)
		call pargstr (Memc[str])
		call pargi (IC_ORDER(ic))
		call pargstr (Memc[IC_SAMPLE(ic)])
		call pargi (IC_NAVERAGE(ic))
		call pargi (IC_NITERATE(ic))
		call pargr (IC_LOW(ic))
		call pargr (IC_HIGH(ic))
		call pargr (IC_GROW(ic))
		call pargi (IC_MARKREJ(ic))
	    if (GP_UIFNAME(IC_GP(ic)) != EOS)
		call gmsg (IC_GP(ic), "icfit", Memc[msg])

	    if (GP_UIFNAME(IC_GP(ic)) != EOS) {
		if (streq (Memc[IC_HELP(ic)], IC_DEFHELP))
		    call strcpy (IC_DEFHTML, Memc[IC_HELP(ic)], SZ_LINE)
	    }

	case GRAPH:
	    call sprintf (Memc[str], SZ_LINE, "graph %c %c %c")
		call pargi ('h'+IC_GKEY(ic)-1)
		call pargi (IC_AXES(ic,IC_GKEY(ic),1))
		call pargi (IC_AXES(ic,IC_GKEY(ic),2))
	    if (GP_UIFNAME(IC_GP(ic)) != EOS)
		call gmsg (IC_GP(ic), "icfit", Memc[str])

	case WCS:
	    call ggview (IC_GP(ic), vx1, vx2, vy1, vy2)
	    call ggwind (IC_GP(ic), wx1, wx2, wy1, wy2)
	    call sprintf (Memc[str], SZ_LINE, "wcs %g %g %g %g %g %g %g %g")
		call pargr (vx1)
		call pargr (vx2)
		call pargr (vy1)
		call pargr (vy2)
		call pargr (wx1)
		call pargr (wx2)
		call pargr (wy1)
		call pargr (wy2)
	    if (GP_UIFNAME(IC_GP(ic)) != EOS)
		call gmsg (IC_GP(ic), "icfit", Memc[str])

	case REFIT:
	    if (GP_UIFNAME(IC_GP(ic)) != EOS)
		call gmsg (IC_GP(ic), "icrefit", cmd)

	case HELP:
	    if (GP_UIFNAME(IC_GP(ic)) != EOS)
		call ic_help (ic)
	    else
		call gpagefile (IC_GP(ic), Memc[IC_HELP(ic)], IC_PROMPT)
	}

	call sfree (sp)
end


# IC_HELP - Send help to GUI

procedure ic_help (ic)

pointer	ic		#I ICFIT pointer

int	i, fd, len_str, open(), getline()
pointer	line, help
errchk	open()

begin
	len_str = 10 * SZ_LINE
	call calloc (help, len_str, TY_CHAR)
	line = help

	fd = open (Memc[IC_HELP(ic)], READ_ONLY, TEXT_FILE)
	while (getline (fd, Memc[line]) != EOF) {
	    for (; Memc[line]!=EOS; line=line+1)
		;
	    i = line - help
	    if (i + SZ_LINE > len_str) {
		len_str = len_str + 10 * SZ_LINE
		call realloc (help, len_str, TY_CHAR)
		line = help + i
	    }
	}
	call close (fd)

	# Send results to GUI.
	call gmsg (IC_GP(ic), "ichelp", Memc[help])

	call mfree (help, TY_CHAR)
end
