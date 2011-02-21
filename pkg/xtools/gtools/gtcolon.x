# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<gset.h>
include	"gtools.h"

define	KEY	"lib$scr/gtools.key"
define	PROMPT	"graph format options"

# Defined colon commands for the GTOOLS package
define	CMDS "|help|xview|yview|xwindow|ywindow|sysid|parameters|title|subtitle\
	|comments|xlabel|ylabel|xunits|yunits|drawtitle|drawxlabels|drawylabels\
	|type|mark|line|xsize|ysize|color|xtransform|ytransform|xflip|yflip\
	|transpose|xformat|yformat|xbuf|ybuf|clip|redraw|expand|shift|expand\
	|uivalues|"

define	HELP		1	# Get help
define	XVIEW		2	# Set X viewport
define	YVIEW		3	# Set Y viewport
define	XWINDOW		4	# Set X window
define	YWINDOW		5	# Set Y window
define	SYSID		6	# Draw SYSID?
define	PARAMETERS	7	# Set parameters string
define	TITLE		8	# Set title
define	SUBTITLE	9	# Set subtitle string
define	COMMENTS	10	# Set comment string
define	XLABEL		11	# Set X label
define	YLABEL		12	# Set Y label
define	XUNITS		13	# Set X unit label
define	YUNITS		14	# Set Y unit label
define	DRAWTITLE	15	# Draw title block?
define	DRAWXLABELS	16	# Draw X label block?
define	DRAWYLABELS	17	# Draw Y label block?
define	TYPE		18	# Set graph type
define	MARK		19	# Set symbol mark type
define	LINE		20	# Set line type
define	XSIZE		21	# Set X symbol size
define	YSIZE		22	# Set Y symbol size
define	COLOR		23	# Set color
define	XTRANSFORM	24	# Set X transformation function
define	YTRANSFORM	25	# Set Y transformation function
define	XFLIP		26	# X flip
define	YFLIP		27	# Y flip
define	TRANSPOSE	28	# Transpose graph
define	XFORMAT		29	# X format
define	YFORMAT		30	# Y format
define	XBUF		31	# X buffer distance
define	YBUF		32	# X buffer distance
define	CLIP		33	# Clipping factors
define	REDRAW		34	# Redraw graph
define	EXPAND		35	# Expand world coordinates
define	SHIFT		36	# Shift world coordinates
define	WINDOW		37	# Window command
define	UIVALUES	38	# Send UI values


# GT_COLON -- Process standard gtools colon commands.

procedure gt_colon (cmdstr, gp, gt, newgraph)

char	cmdstr[ARB]			# Command string
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
int	newgraph			# Update graph?

char	cmd[SZ_LINE]
int	ip, ncmd, ival
real	x, y, rval[4]
bool	bval

int	nscan(), strdic(), gt_geti(), btoi()
real	gt_getr()

begin
	# All GTOOLS commands start with '/'.
	if (cmdstr[1] != '/')
	    return

	# Parse the command string matched against a dictionary.
	call sscan (cmdstr[2])
	call gargwrd (cmd, SZ_LINE)
	ncmd = strdic (cmd, cmd, SZ_LINE, CMDS)

	# Switch on the command and parse the arguments.
	switch (ncmd) {
	case HELP: # help: Print help
	    call gpagefile (gp, KEY, PROMPT)

	case XVIEW: # xview:  List or set x viewport.
	    call gargr (rval[1])
	    call gargr (rval[2])
	    if (nscan() == 3) {
		call gt_setr (gt, GTVXMIN, rval[1])
		call gt_setr (gt, GTVXMAX, rval[2])
	    } else {
		call printf ("xview = %g %g\n")
		    call pargr (gt_getr (gt, GTVXMIN))
		    call pargr (gt_getr (gt, GTVXMAX))
	    }

	case YVIEW: # yview:  List or set y viewport.
	    call gargr (rval[1])
	    call gargr (rval[2])
	    if (nscan() == 3) {
		call gt_setr (gt, GTVYMIN, rval[1])
		call gt_setr (gt, GTVYMAX, rval[2])
	    } else {
		call printf ("yview = %g %g\n")
		    call pargr (gt_getr (gt, GTVYMIN))
		    call pargr (gt_getr (gt, GTVYMAX))
	    }

	case XWINDOW: # xwindow:  List or set x window.
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

	case YWINDOW: # ywindow:  List or set y window.
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

	case SYSID: # sysid: Write SYSID string?
	    call gargb (bval)
	    if (nscan() == 2)
		call gt_seti (gt, GTSYSID, btoi (bval))
	    else {
		call printf ("sysid = %b\n")
		    call pargi (gt_geti (gt, GTSYSID))
	    }

	case PARAMETERS: # parameters: Set parameters string
	    call gargstr (cmd, SZ_LINE)
	    for (ip=1; IS_WHITE(cmd[ip]); ip=ip+1)
		;
	    call gt_sets (gt, GTPARAMS, cmd[ip])

	case TITLE: # title: Set graph title
	    call gargstr (cmd, SZ_LINE)
	    for (ip=1; IS_WHITE(cmd[ip]); ip=ip+1)
		;
	    call gt_sets (gt, GTTITLE, cmd[ip])

	case SUBTITLE: # subtitle: Set subtitle string
	    call gargstr (cmd, SZ_LINE)
	    for (ip=1; IS_WHITE(cmd[ip]); ip=ip+1)
		;
	    call gt_sets (gt, GTSUBTITLE, cmd[ip])

	case COMMENTS: # comments: Set graph comments
	    call gargstr (cmd, SZ_LINE)
	    for (ip=1; IS_WHITE(cmd[ip]); ip=ip+1)
		;
	    call gt_sets (gt, GTCOMMENTS, cmd[ip])

	case XLABEL: # xlabel: Set graph x label
	    call gargstr (cmd, SZ_LINE)
	    for (ip=1; IS_WHITE(cmd[ip]); ip=ip+1)
		;
	    call gt_sets (gt, GTXLABEL, cmd[ip])

	case YLABEL: # ylabel: Set graph y label
	    call gargstr (cmd, SZ_LINE)
	    for (ip=1; IS_WHITE(cmd[ip]); ip=ip+1)
		;
	    call gt_sets (gt, GTYLABEL, cmd[ip])

	case XUNITS: # xunits: Set graph x units
	    call gargstr (cmd, SZ_LINE)
	    for (ip=1; IS_WHITE(cmd[ip]); ip=ip+1)
		;
	    call gt_sets (gt, GTXUNITS, cmd[ip])

	case YUNITS: # yunits: Set graph y units
	    call gargstr (cmd, SZ_LINE)
	    for (ip=1; IS_WHITE(cmd[ip]); ip=ip+1)
		;
	    call gt_sets (gt, GTYUNITS, cmd[ip])

	case DRAWTITLE: # drawtitle: Draw title block?
	    call gargb (bval)
	    if (nscan() == 2)
		call gt_seti (gt, GTDRAWTITLE, btoi (bval))
	    else {
		call printf ("drawtitle = %b\n")
		    call pargi (gt_geti (gt, GTDRAWTITLE))
	    }

	case DRAWXLABELS: # drawxlabels: Draw x label block?
	    call gargb (bval)
	    if (nscan() == 2)
		call gt_seti (gt, GTDRAWXLABELS, btoi (bval))
	    else {
		call printf ("drawxlabel = %b\n")
		    call pargi (gt_geti (gt, GTDRAWXLABELS))
	    }

	case DRAWYLABELS: # drawylabels: Draw y label block?
	    call gargb (bval)
	    if (nscan() == 2)
		call gt_seti (gt, GTDRAWYLABELS, btoi (bval))
	    else {
		call printf ("drawylabel = %b\n")
		    call pargi (gt_geti (gt, GTDRAWYLABELS))
	    }

	case TYPE: # type: Graph type
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 2)
		call gt_sets (gt, GTTYPE, cmd)
	    else {
		call gt_gets (gt, GTTYPE, cmd, SZ_LINE)
	        call printf ("type = %s\n")
		    call pargstr (cmd)
	    }

	case MARK: # mark: Mark type
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 2)
		call gt_sets (gt, GTMARK, cmd)
	    else {
		call gt_gets (gt, GTMARK, cmd, SZ_LINE)
	        call printf ("mark = %s\n")
		    call pargstr (cmd)
	    }

	case LINE: # line: Line type
	    call gargi (ival)
	    if (nscan() == 2)
		call gt_seti (gt, GTLINE, ival)
	    else {
	        call printf ("line = %s\n")
		    call pargi (gt_geti (gt, GTLINE))
	    }

	case XSIZE: # xsize:  List or set x mark size.
	    call gargr (rval[1])
	    if (nscan() == 2) {
		call gt_setr (gt, GTXSIZE, rval[1])
	    } else {
		call printf ("xsize = %g\n")
		    call pargr (gt_getr (gt, GTXSIZE))
	    }


	case YSIZE: # ysize:  List or set y mark size.
	    call gargr (rval[1])
	    if (nscan() == 2) {
		call gt_setr (gt, GTYSIZE, rval[1])
	    } else {
		call printf ("ysize = %g\n")
		    call pargr (gt_getr (gt, GTYSIZE))
	    }

	case COLOR: # color: line/mark color
	    call gargi (ival)
	    if (nscan() == 2)
		call gt_seti (gt, GTCOLOR, ival)
	    else {
	        call printf ("color = %s\n")
		    call pargi (gt_geti (gt, GTCOLOR))
	    }

	case XTRANSFORM: # xtransform: List or set ytransform.
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 2)
		call gt_sets (gt, GTXTRAN, cmd)
	    else {
		call gt_gets (gt, GTXTRAN, cmd, SZ_LINE)
	        call printf ("xtransform = %s\n")
		    call pargstr (cmd)
	    }

	case YTRANSFORM: # ytransform: List or set ytransform.
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 2)
		call gt_sets (gt, GTYTRAN, cmd)
	    else {
		call gt_gets (gt, GTYTRAN, cmd, SZ_LINE)
	        call printf ("ytransform = %s\n")
		    call pargstr (cmd)
	    }
	
	case XFLIP: # xflip: Toggle x flip flag
	    call gargb (bval)
	    if (nscan() == 2)
		call gt_seti (gt, GTXFLIP, btoi (bval))
	    else {
		call printf ("xflip = %b\n")
		    call pargi (gt_geti (gt, GTXFLIP))
	    }

	case YFLIP: # yflip: Toggle y flip flag
	    call gargb (bval)
	    if (nscan() == 2)
		call gt_seti (gt, GTYFLIP, btoi (bval))
	    else {
		call printf ("yflip = %b\n")
		    call pargi (gt_geti (gt, GTYFLIP))
	    }

	case TRANSPOSE: # transpose: Toggle transpose flag
	    if (gt_geti (gt, GTTRANSPOSE) == NO)
		call gt_seti (gt, GTTRANSPOSE, YES)
	    else
		call gt_seti (gt, GTTRANSPOSE, NO)

	case XFORMAT: # xformat: Set graph x format
	    call gargstr (cmd, SZ_LINE)
	    for (ip=1; IS_WHITE(cmd[ip]); ip=ip+1)
		;
	    call gt_sets (gt, GTXFORMAT, cmd[ip])

	case YFORMAT: # yformat: Set graph y format
	    call gargstr (cmd, SZ_LINE)
	    for (ip=1; IS_WHITE(cmd[ip]); ip=ip+1)
		;
	    call gt_sets (gt, GTYFORMAT, cmd[ip])

	case XBUF: # xbuf: List or set x buffer.
	    call gargr (rval[1])
	    if (nscan() == 2)
		call gt_setr (gt, GTXBUF, rval[1])
	    else {
		call printf ("xbuf = %g\n")
		   call pargr (gt_getr (gt, GTXBUF))
	    }

	case YBUF: # ybuf: List or set y buffer.
	    call gargr (rval[1])
	    if (nscan() == 2)
		call gt_setr (gt, GTYBUF, rval[1])
	    else {
		call printf ("ybuf = %g\n")
		   call pargr (gt_getr (gt, GTYBUF))
	    }

	case CLIP: # clip: autoscaling clipping
	    call gargr (rval[1])
	    call gargr (rval[2])
	    if (nscan() == 3) {
		call gt_setr (gt, GTLCLIP, rval[1])
		call gt_setr (gt, GTHCLIP, rval[2])
	    } else {
		call printf ("clip = %g %g\n")
		    call pargr (gt_getr (gt, GTLCLIP))
		    call pargr (gt_getr (gt, GTHCLIP))
	    }

	case REDRAW: # redraw: Redraw the graph
	    newgraph = 1

	case EXPAND: # :expand x1 x2 y1 y2
	    call gargr (rval[1])
	    call gargr (rval[2])
	    call gargr (rval[3])
	    call gargr (rval[4])
	    if (nscan() == 5) {
		if (rval[1] !=  gt_getr (gt, GTXMIN)) {
		    call gt_setr (gt, GTXMIN, rval[1])
		    newgraph = 1
		}
		if (rval[2] !=  gt_getr (gt, GTXMAX)) {
		    call gt_setr (gt, GTXMAX, rval[2])
		    newgraph = 1
		}
		if (rval[3] !=  gt_getr (gt, GTYMIN)) {
		    call gt_setr (gt, GTYMIN, rval[3])
		    newgraph = 1
		}
		if (rval[4] !=  gt_getr (gt, GTYMAX)) {
		    call gt_setr (gt, GTYMAX, rval[4])
		    newgraph = 1
		}
	    }

	case SHIFT: # :shift x y
	    call gargr (x)
	    call gargr (y)
	    rval[1] = gt_getr (gt, GTXMIN)
	    rval[2] = gt_getr (gt, GTXMAX)
	    if (IS_INDEFR(x)) {
		if (!IS_INDEFR(rval[1]) || !IS_INDEFR(rval[2])) {
		    call gt_setr (gt, GTXMIN, INDEFR)
		    call gt_setr (gt, GTXMAX, INDEFR)
		    newgraph = 1
		}
	    } else {
		if (!IS_INDEFR(rval[1]) && !IS_INDEFR(rval[2])) {
		    rval[3] = rval[2] - rval[1]
		    rval[4] = x - (rval[1] + rval[2]) / 2
		    if (abs (rval[4] / rval[3]) > 0.001) {
			call gt_setr (gt, GTXMIN, rval[1] + rval[4])
			call gt_setr (gt, GTXMAX, rval[2] + rval[4])
		    }
		    newgraph = 1
		}
	    }

	    rval[1] = gt_getr (gt, GTYMIN)
	    rval[2] = gt_getr (gt, GTYMAX)
	    if (IS_INDEFR(y)) {
		if (!IS_INDEFR(rval[1]) || !IS_INDEFR(rval[2])) {
		    call gt_setr (gt, GTYMIN, INDEFR)
		    call gt_setr (gt, GTYMAX, INDEFR)
		    newgraph = 1
		}
	    } else {
		if (!IS_INDEFR(rval[1]) && !IS_INDEFR(rval[2])) {
		    rval[3] = rval[2] - rval[1]
		    rval[4] = y - (rval[1] + rval[2]) / 2
		    if (abs (rval[4] / rval[3]) > 0.001) {
			call gt_setr (gt, GTYMIN, rval[1] + rval[4])
			call gt_setr (gt, GTYMAX, rval[2] + rval[4])
		    }
		    newgraph = 1
		}
	    }

	case WINDOW: # window: window x y wcs key cmd
	    call gargr (x)
	    call gargr (y)
	    call gargi (ip)
	    call gargwrd (cmd, SZ_LINE)
	    ival = cmd[1]
	    if (nscan() < 5)
		return
	    if (ival == ':')
		call gargwrd (cmd, SZ_LINE)
	    call gt_window1 (gt, gp, x, y, ip, ival, cmd, newgraph)

	case UIVALUES: # uivalues: send values to UI
	    call gt_uivalues (gp, gt)

	default: # Check for more colon command
	    call gt_colon1 (cmdstr, gp, gt, newgraph)
	}
end


# Defined colon commands
define	CMDS1 "|txup|txsize|txpath|txspacing|txhjustify|txvjustify|txfont\
	|txquality|txcolor|drawtitle|titlesize|titlejust|ntitlelines|aspect\
	|charsize|titlecolor|framecolor|drawaxes|setaxispos|axispos1|axispos2\
	|drawgrid|round|labelaxis|axislabelsize|drawticks|labelticks|nmajor\
	|nminor|majorlength|minorlength|majorwidth|minorwidth|axiswidth\
	|ticklabelsize|gridcolor|axislabelcolor|axiscolor|ticklabelcolor\
	|tickcolor|axes|ticks|colors|"

define	TXUP			1	# Text parameters
define	TXSIZE			2
define	TXPATH			3
define	TXSPACING		4
define	TXHJUSTIFY		5
define	TXVJUSTIFY		6
define	TXFONT			7
define	TXQUALITY		8
define	TXCOLOR			9

define	DRAWTITLE		10	# GLABAX, general parameters
define	TITLESIZE		11
define	TITLEJUST		12
define	NTITLELINES		13
define	ASPECT			14
define	CHARSIZE		15
define	TITLECOLOR		16
define	FRAMECOLOR		17

define	DRAWAXES		18	# GLABAX, x/y axis parameters
define	SETAXISPOS		19
define	AXISPOS1		20
define	AXISPOS2		21
define	DRAWGRID		22
define	ROUND			23
define	LABELAXIS		24
define	AXISLABELSIZE		25
define	DRAWTICKS		26
define	LABELTICKS		27
define	NMAJOR			28
define	NMINOR			29
define	MAJORLENGTH		30
define	MINORLENGTH		31
define	MAJORWIDTH		32
define	MINORWIDTH		33
define	AXISWIDTH		34
define	TICKLABELSIZE		35
define	GRIDCOLOR		36
define	AXISLABELCOLOR		37
define	AXISCOLOR		38
define	TICKLABELCOLOR		39
define	TICKCOLOR		40

define	AXES			41	# Grouped parameters
define	TICKS			42
define	COLORS			43


# GT_COLON1 -- Interpret colon commands.

procedure gt_colon1 (cmdstr, gp, gt, newgraph)

char	cmdstr[ARB]			# Command string
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
int	newgraph			# Update graph?

bool	bval
real	rval[12]
pointer	sp, cmd
int	ncmd, btoi(), nscan(), strdic()

begin
	# All GTOOLS commands start with '/'.
	if (cmdstr[1] != '/')
	    return

	# Parse the command string matched against a dictionary.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	call sscan (cmdstr[2])
	call gargwrd (Memc[cmd], SZ_LINE)
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, CMDS1)

	# Get arguments and return if there are insufficient arguments.
	if (ncmd < DRAWAXES) {
	    call gargr (rval[1])
	    if (nscan() != 2) {
		call sfree (sp)
		return
	    }
	} else if (ncmd < AXES) {
	    switch (ncmd) {
	    case DRAWAXES:
		call gargwrd (Memc[cmd], SZ_LINE)
		rval[1] = strdic (Memc[cmd], Memc[cmd], SZ_LINE, GT_XAXES)
		call gargwrd (Memc[cmd], SZ_LINE)
		rval[2] = strdic (Memc[cmd], Memc[cmd], SZ_LINE, GT_YAXES)
	    case DRAWGRID, ROUND, LABELAXIS, DRAWTICKS, LABELTICKS:
		call gargb (bval)
		rval[1] = btoi (bval)
		call gargb (bval)
		rval[2] = btoi (bval)
	    default:
		call gargr (rval[1])
		call gargr (rval[2])
	    }
	    if (nscan() != 3) {
		call sfree (sp)
		return
	    }
	}

	# Switch on the command and parse the arguments.
	switch (ncmd) {
	case TXUP:
	    Memi[gt+GT_TXUP] = nint (rval[1])
	case TXSIZE:
	    Memr[P2R(gt+GT_TXSIZE)] = rval[1]
	case TXPATH:
	    Memi[gt+GT_TXPATH] = nint (rval[1])
	case TXSPACING:
	    Memr[P2R(gt+GT_TXSPACING)] = rval[1]
	case TXHJUSTIFY:
	    Memi[gt+GT_TXHJUSTIFY] = nint (rval[1])
	case TXVJUSTIFY:
	    Memi[gt+GT_TXVJUSTIFY] = nint (rval[1])
	case TXFONT:
	    Memi[gt+GT_TXFONT] = nint (rval[1])
	case TXQUALITY:
	    Memi[gt+GT_TXQUALITY] = nint (rval[1])
	case TXCOLOR:
	    Memi[gt+GT_TXCOLOR] = nint (rval[1])

	case DRAWTITLE:
	    Memi[gt+GT_DRAWTITLE] = nint (rval[1])
	case TITLESIZE:
	    Memr[P2R(gt+GT_TITLESIZE)] = rval[1]
	case TITLEJUST:
	    Memi[gt+GT_TITLEJUST] = nint (rval[1])
	case NTITLELINES:
	    Memi[gt+GT_NTITLELINES] = nint (rval[1])
	case ASPECT:
	    Memr[P2R(gt+GT_ASPECT)] = rval[1]
	case CHARSIZE:
	    Memr[P2R(gt+GT_CHARSIZE)] = rval[1]
	case TITLECOLOR:
	    Memi[gt+GT_TITLECOLOR] = nint (rval[1])
	case FRAMECOLOR:
	    Memi[gt+GT_FRAMECOLOR] = nint (rval[1])

	case DRAWAXES:
	    if (rval[1] > 0)
		Memi[gt+GT_XDRAWAXES] = nint (rval[1]) - 1
	    if (rval[2] > 0)
		Memi[gt+GT_YDRAWAXES] = nint (rval[2]) - 1
	case SETAXISPOS:
	    Memi[gt+GT_XSETAXISPOS] = nint (rval[1])
	    Memi[gt+GT_YSETAXISPOS] = nint (rval[2])
	case AXISPOS1:
	    Memr[P2R(gt+GT_XAXISPOS1)] = rval[1]
	    Memr[P2R(gt+GT_YAXISPOS1)] = rval[2]
	case AXISPOS2:
	    Memr[P2R(gt+GT_XAXISPOS2)] = rval[1]
	    Memr[P2R(gt+GT_YAXISPOS2)] = rval[2]
	case DRAWGRID:
	    Memi[gt+GT_XDRAWGRID] = nint (rval[1])
	    Memi[gt+GT_YDRAWGRID] = nint (rval[2])
	case ROUND:
	    Memi[gt+GT_XROUND] = nint (rval[1])
	    Memi[gt+GT_YROUND] = nint (rval[2])
	case LABELAXIS:
	    Memi[gt+GT_XLABELAXIS] = nint (rval[1])
	    Memi[gt+GT_YLABELAXIS] = nint (rval[2])
	case AXISLABELSIZE:
	    Memr[P2R(gt+GT_XAXISLABELSIZE)] = rval[1]
	    Memr[P2R(gt+GT_YAXISLABELSIZE)] = rval[2]
	case DRAWTICKS:
	    Memi[gt+GT_XDRAWTICKS] = nint (rval[1])
	    Memi[gt+GT_YDRAWTICKS] = nint (rval[2])
	case LABELTICKS:
	    Memi[gt+GT_XLABELTICKS] = nint (rval[1])
	    Memi[gt+GT_YLABELTICKS] = nint (rval[2])
	case NMAJOR:
	    Memi[gt+GT_XNMAJOR] = nint (rval[1])
	    Memi[gt+GT_YNMAJOR] = nint (rval[2])
	case NMINOR:
	    Memi[gt+GT_XNMINOR] = nint (rval[1])
	    Memi[gt+GT_YNMINOR] = nint (rval[2])
	case MAJORLENGTH:
	    Memr[P2R(gt+GT_XMAJORLENGTH)] = rval[1]
	    Memr[P2R(gt+GT_YMAJORLENGTH)] = rval[2]
	case MINORLENGTH:
	    Memr[P2R(gt+GT_XMINORLENGTH)] = rval[1]
	    Memr[P2R(gt+GT_YMINORLENGTH)] = rval[2]
	case MAJORWIDTH:
	    Memr[P2R(gt+GT_XMAJORWIDTH)] = rval[1]
	    Memr[P2R(gt+GT_YMAJORWIDTH)] = rval[2]
	case MINORWIDTH:
	    Memr[P2R(gt+GT_XMINORWIDTH)] = rval[1]
	    Memr[P2R(gt+GT_YMINORWIDTH)] = rval[2]
	case AXISWIDTH:
	    Memr[P2R(gt+GT_XAXISWIDTH)] = rval[1]
	    Memr[P2R(gt+GT_YAXISWIDTH)] = rval[2]
	case TICKLABELSIZE:
	    Memr[P2R(gt+GT_XTICKLABELSIZE)] = rval[1]
	    Memr[P2R(gt+GT_YTICKLABELSIZE)] = rval[2]
	case GRIDCOLOR:
	    Memi[gt+GT_XGRIDCOLOR] = nint (rval[1])
	    Memi[gt+GT_YGRIDCOLOR] = nint (rval[2])
	case AXISLABELCOLOR:
	    Memi[gt+GT_XAXISLABELCOLOR] = nint (rval[1])
	    Memi[gt+GT_YAXISLABELCOLOR] = nint (rval[2])
	case AXISCOLOR:
	    Memi[gt+GT_XAXISCOLOR] = nint (rval[1])
	    Memi[gt+GT_YAXISCOLOR] = nint (rval[2])
	case TICKLABELCOLOR:
	    Memi[gt+GT_XTICKLABELCOLOR] = nint (rval[1])
	    Memi[gt+GT_YTICKLABELCOLOR] = nint (rval[2])
	case TICKCOLOR:
	    Memi[gt+GT_XTICKCOLOR] = nint (rval[1])
	    Memi[gt+GT_YTICKCOLOR] = nint (rval[2])

	case AXES:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    rval[1] = strdic (Memc[cmd], Memc[cmd], SZ_LINE, GT_XAXES)
	    call gargr (rval[2])
	    call gargb (bval)
	    rval[3] = btoi (bval)
	    call gargwrd (Memc[cmd], SZ_LINE)
	    rval[4] = strdic (Memc[cmd], Memc[cmd], SZ_LINE, GT_XAXES)
	    call gargr (rval[5])
	    call gargb (bval)
	    rval[6] = btoi (bval)
	    if (nscan() == 7) {
		Memi[gt+GT_XDRAWAXES] = nint (rval[1])
		Memr[P2R(gt+GT_XAXISWIDTH)] = rval[2]
		Memr[P2R(gt+GT_XMAJORWIDTH)] = rval[2]
		Memr[P2R(gt+GT_XMINORWIDTH)] = rval[2]
		Memi[gt+GT_XDRAWGRID] = nint (rval[3])
		Memi[gt+GT_YDRAWAXES] = nint (rval[4])
		Memr[P2R(gt+GT_YAXISWIDTH)] = rval[5]
		Memr[P2R(gt+GT_YMAJORWIDTH)] = rval[5]
		Memr[P2R(gt+GT_YMINORWIDTH)] = rval[5]
		Memi[gt+GT_YDRAWGRID] = nint (rval[6])
	    }
	case TICKS:
	    call gargb (bval)
	    rval[1] = btoi (bval)
	    call gargb (bval)
	    rval[2] = btoi (bval)
	    call gargr (rval[3])
	    call gargr (rval[4])
	    call gargb (bval)
	    rval[5] = btoi (bval)
	    call gargb (bval)
	    rval[6] = btoi (bval)
	    call gargr (rval[7])
	    call gargr (rval[8])
	    if (nscan() == 9) {
		Memi[gt+GT_XDRAWTICKS] = nint (rval[1])
		Memi[gt+GT_XLABELTICKS] = nint (rval[2])
		Memi[gt+GT_XNMAJOR] = nint (rval[3])
		Memi[gt+GT_XNMINOR] = nint (rval[4])
		Memi[gt+GT_YDRAWTICKS] = nint (rval[5])
		Memi[gt+GT_YLABELTICKS] = nint (rval[6])
		Memi[gt+GT_YNMAJOR] = nint (rval[7])
		Memi[gt+GT_YNMINOR] = nint (rval[8])
	    }
	case COLORS:
	    call gargr (rval[1])
	    call gargr (rval[2])
	    call gargr (rval[3])
	    call gargr (rval[4])
	    call gargr (rval[5])
	    call gargr (rval[6])
	    call gargr (rval[7])
	    call gargr (rval[8])
	    call gargr (rval[9])
	    call gargr (rval[10])
	    call gargr (rval[11])
	    call gargr (rval[12])
	    if (nscan() == 13) {
		Memi[gt+GT_FRAMECOLOR] = nint (rval[1])
		Memi[gt+GT_TITLECOLOR] = nint (rval[2])
		Memi[gt+GT_XGRIDCOLOR] = nint (rval[3])
		Memi[gt+GT_XAXISLABELCOLOR] = nint (rval[4])
		Memi[gt+GT_XAXISCOLOR] = nint (rval[5])
		Memi[gt+GT_XTICKLABELCOLOR] = nint (rval[6])
		Memi[gt+GT_XTICKCOLOR] = nint (rval[7])
		Memi[gt+GT_YGRIDCOLOR] = nint (rval[8])
		Memi[gt+GT_YAXISLABELCOLOR] = nint (rval[9])
		Memi[gt+GT_YAXISCOLOR] = nint (rval[10])
		Memi[gt+GT_YTICKLABELCOLOR] = nint (rval[11])
		Memi[gt+GT_YTICKCOLOR] = nint (rval[12])
	    }
	}

	call sfree (sp)
end
