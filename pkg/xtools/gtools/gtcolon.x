# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gtools.h"

define	KEY	"lib$scr/gtools.key"
define	PROMPT	"graph format options"

# Defined colon commands for the GTOOLS package
define	COMMANDS "|/xwindow|/ywindow|/xtransform|/ytransform|/title|/xlabel|\
	|/ylabel|/xunits|/yunits|/comments|/help|/redraw|/subtitle|/xsize|\
	|/ysize|/parameters|/type|/mark|/line|/transpose|/sysid|/color|\
	|/xflip|/yflip|/drawtitle|/drawxlabels|/drawylabels|"

define	XWINDOW		1	# Set X window limits
define	YWINDOW		2	# Set Y window limits
define	XTRANSFORM	3	# Set X transformation function
define	YTRANSFORM	4	# Set Y transformation function
define	TITLE		5	# Set title
define	XLABEL		6	# Set X label
#	newline		7
define	YLABEL		8	# Set Y label
define	XUNITS		9	# Set X unit label
define	YUNITS		10	# Set Y unit label
define	COMMENTS	11	# Set comment string
define	HELP		12	# Get help
define	REDRAW		13	# Redraw graph
define	SUBTITLE	14	# Set subtitle string
define	XSIZE		15	# Set X symbol size
#	newline		16
define	YSIZE		17	# Set Y symbol size
define	PARAMETERS	18	# Set parameters string
define	TYPE		19	# Set graph type
define	MARK		20	# Set symbol mark type
define	LINE		21	# Set line type
define	TRANSPOSE	22	# Transpose graph
define	SYSID		23	# Draw SYSID?
define	COLOR		24	# Set color
#	newline		25
define	XFLIP		26	# Toggle x flip
define	YFLIP		27	# Toggle y flip
define	DRAWTITLE	28	# Draw title?
define	DRAWXLABELS	29	# Draw X label?
define	DRAWYLABELS	30	# Draw Y label?


# GT_COLON -- Process standard gtools colon commands.

procedure gt_colon (cmdstr, gp, gt, newgraph)

char	cmdstr[ARB]			# Command string
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
int	newgraph			# Update graph?

char	cmd[SZ_LINE]
int	ncmd, ival
bool	bval
real	rval[2]

int	nscan(), strdic(), gt_geti()
bool	btoi()
real	gt_getr()

begin
	# Parse the command string matched against a dictionary..
	call sscan (cmdstr)
	call gargwrd (cmd, SZ_LINE)
	ncmd = strdic (cmd, cmd, SZ_LINE, COMMANDS)

	# Switch on the command and parse the arguments.
	switch (ncmd) {
	case XWINDOW: # /xwindow:  List or set x window.
	    call gargr (rval[1])
	    call gargr (rval[2])
	    if (nscan() == 3) {
		call gt_setr (gt, GTXMIN, rval[1])
		call gt_setr (gt, GTXMAX, rval[2])
	    } else {
		call printf ("xwindow = %g %g\n")
		    call pargr (gt_getr (gt, GTXMIN))
		    call pargr (gt_getr (gt, GTXMAX))
	    }

	case YWINDOW: # /ywindow:  List or set y window.
	    call gargr (rval[1])
	    call gargr (rval[2])
	    if (nscan() == 3) {
		call gt_setr (gt, GTYMIN, rval[1])
		call gt_setr (gt, GTYMAX, rval[2])
	    } else {
		call printf ("ywindow = %g %g\n")
		    call pargr (gt_getr (gt, GTYMIN))
		    call pargr (gt_getr (gt, GTYMAX))
	    }

	case XTRANSFORM: # /xtransform: List or set ytransform.
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 2)
		call gt_sets (gt, GTXTRAN, cmd)
	    else {
		call gt_gets (gt, GTXTRAN, cmd, SZ_LINE)
	        call printf ("xtransform = %s\n")
		    call pargstr (cmd)
	    }

	case YTRANSFORM: # /ytransform: List or set ytransform.
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 2)
		call gt_sets (gt, GTYTRAN, cmd)
	    else {
		call gt_gets (gt, GTYTRAN, cmd, SZ_LINE)
	        call printf ("ytransform = %s\n")
		    call pargstr (cmd)
	    }

	case TITLE: # /title: Set graph title
	    call gargstr (cmd, SZ_LINE)
	    call gt_sets (gt, GTTITLE, cmd)

	case XLABEL: # /xlabel: Set graph x label
	    call gargstr (cmd, SZ_LINE)
	    call gt_sets (gt, GTXLABEL, cmd)

	case YLABEL: # /ylabel: Set graph y label
	    call gargstr (cmd, SZ_LINE)
	    call gt_sets (gt, GTYLABEL, cmd)

	case XUNITS: # /xunits: Set graph x units
	    call gargstr (cmd, SZ_LINE)
	    call gt_sets (gt, GTXUNITS, cmd)

	case YUNITS: # /yunits: Set graph y units
	    call gargstr (cmd, SZ_LINE)
	    call gt_sets (gt, GTYUNITS, cmd)

	case COMMENTS: # /comments: Set graph comments
	    call gargstr (cmd, SZ_LINE)
	    call gt_sets (gt, GTCOMMENTS, cmd)

	case HELP: # /help: Print help
	    call gpagefile (gp, KEY, PROMPT)
	
	case REDRAW: # /redraw: Redraw the graph
	    newgraph = YES

	case SUBTITLE: # /subtitle: Set subtitle string
	    call gargstr (cmd, SZ_LINE)
	    call gt_sets (gt, GTSUBTITLE, cmd)

	case XSIZE: # /xsize:  List or set x mark size.
	    call gargr (rval[1])
	    if (nscan() == 2) {
		call gt_setr (gt, GTXSIZE, rval[1])
	    } else {
		call printf ("xsize = %g\n")
		    call pargr (gt_getr (gt, GTXSIZE))
	    }


	case YSIZE: # /ysize:  List or set y mark size.
	    call gargr (rval[1])
	    if (nscan() == 2) {
		call gt_setr (gt, GTYSIZE, rval[1])
	    } else {
		call printf ("ysize = %g\n")
		    call pargr (gt_getr (gt, GTYSIZE))
	    }

	case PARAMETERS: # /parameters: Set parameters string
	    call gargstr (cmd, SZ_LINE)
####	    call gt_sets (gt, GTPARAMETERS, cmd)
	    call gt_sets (gt, GTPARAMS, cmd)

	case TYPE: # /type: Graph type
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 2)
		call gt_sets (gt, GTTYPE, cmd)
	    else {
		call gt_gets (gt, GTTYPE, cmd, SZ_LINE)
	        call printf ("type = %s\n")
		    call pargstr (cmd)
	    }

	case MARK: # /mark: Mark type
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 2)
		call gt_sets (gt, GTMARK, cmd)
	    else {
		call gt_gets (gt, GTMARK, cmd, SZ_LINE)
	        call printf ("mark = %s\n")
		    call pargstr (cmd)
	    }

	case LINE: # /line: Line type
	    call gargi (ival)
	    if (nscan() == 2)
		call gt_seti (gt, GTLINE, ival)
	    else {
	        call printf ("line = %s\n")
		    call pargi (gt_geti (gt, GTLINE))
	    }

	case XFLIP: # /xflip: Toggle x flip flag
	    call gargb (bval)
	    if (nscan() == 2)
		call gt_seti (gt, GTXFLIP, btoi (bval))
	    else {
		call printf ("xflip = %b\n")
		    call pargi (gt_geti (gt, GTXFLIP))
	    }

	case YFLIP: # /yflip: Toggle y flip flag
	    call gargb (bval)
	    if (nscan() == 2)
		call gt_seti (gt, GTYFLIP, btoi (bval))
	    else {
		call printf ("yflip = %b\n")
		    call pargi (gt_geti (gt, GTYFLIP))
	    }

	case TRANSPOSE: # /transpose: Toggle transpose flag
	    if (gt_geti (gt, GTTRANSPOSE) == NO)
		call gt_seti (gt, GTTRANSPOSE, YES)
	    else
		call gt_seti (gt, GTTRANSPOSE, NO)

	case SYSID: # /sysid: Write SYSID string?
	    call gargb (bval)
	    if (nscan() == 2)
		call gt_seti (gt, GTSYSID, btoi (bval))
	    else {
		call printf ("sysid = %b\n")
		    call pargi (gt_geti (gt, GTSYSID))
	    }

	case DRAWTITLE: # /drawtitle: Draw title?
	    call gargb (bval)
	    if (nscan() == 2)
		call gt_seti (gt, GTDRAWTITLE, btoi (bval))
	    else {
		call printf ("drawtitle = %b\n")
		    call pargi (gt_geti (gt, GTDRAWTITLE))
	    }

	case DRAWXLABELS: # /drawxlabels: draw x labels?
	    call gargb (bval)
	    if (nscan() == 2)
		call gt_seti (gt, GTDRAWXLABELS, btoi (bval))
	    else {
		call printf ("drawxlabels = %b\n")
		    call pargi (gt_geti (gt, GTDRAWXLABELS))
	    }

	case DRAWYLABELS: # /drawylabels: draw y labels?
	    call gargb (bval)
	    if (nscan() == 2)
		call gt_seti (gt, GTDRAWYLABELS, btoi (bval))
	    else {
		call printf ("drawylabels = %b\n")
		    call pargi (gt_geti (gt, GTDRAWYLABELS))
	    }

	case COLOR: # /color: line/mark color
	    call gargi (ival)
	    if (nscan() == 2)
		call gt_seti (gt, GTCOLOR, ival)
	    else {
	        call printf ("color = %s\n")
		    call pargi (gt_geti (gt, GTCOLOR))
	    }
	}
end
