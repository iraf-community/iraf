# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<error.h>
include	"imexam.h"
 
# List of boundary types, marker types, and colon commands.

define	BTYPES	"|constant|nearest|reflect|wrap|project|"
define	MTYPES	"|point|box|plus|cross|circle|hebar|vebar|hline|vline|diamond|"
define	CMDS	"|angh|angv|background|banner|boundary|box|buffer|ceiling|\
		|center|constant|dashpat|defkey|eparam|fill|floor|interval|\
		|label|logfile|logx|logy|magzero|majrx|majry|marker|minrx|\
		|minry|naverage|ncolumns|ncontours|ncstat|nhi|nlines|nlstat|\
		|pointmode|radius|round|rplot|select|szmarker|ticklabels|\
		|title|width|x|xlabel|xorder|y|ylabel|yorder|zero|unlearn|\
		|autoredraw|nbins|z1|z2|autoscale|top_closed|allframes|wcs|\
		|xformat|yformat|fitplot|sigma|axes|fittype|beta|iterations|\
		|output|ncoutput|nloutput|"
 
define	ANGH		 1
define	ANGV		 2
define	BACKGROUND	 3
define	BANNER		 4
define	BOUNDARY	 5
define	BOX		 6
define	BUFFER		 7
define	CEILING		 8

define	CENTER		10
define	CONSTANT	11
define	DASHPAT		12
define	DEFKEY		13
define	EPARAM		14
define	FILL		15
define	FLOOR		16
define	INTERVAL	17

define	LABEL		19
define	LOGFILE		20
define	LOGX		21
define	LOGY		22
define	MAGZERO		23
define	MAJRX		24
define	MAJRY		25
define	MARKER		26
define	MINRX		27

define	MINRY		29
define	NAVERAGE	30
define	NCOLUMNS	31
define	NCONTOURS	32
define	NCSTAT		33
define	NHI		34
define	NLINES		35
define	NLSTAT		36

define	POINTMODE	38
define	RADIUS		39
define	ROUND		40
define	RPLOT		41
define	SELECT		42
define	SZMARKER	43
define	TICKLABELS	44

define	TITLE		46
define	WIDTH		47
define	X		48
define	XLABEL		49
define	XORDER		50
define	Y		51
define	YLABEL		52
define	YORDER		53
define	ZERO		54
define	UNLEARN		55

define	AUTOREDRAW	57
define	NBINS		58
define	Z1		59
define	Z2		60
define	AUTOSCALE	61
define	TOP_CLOSED	62
define	ALLFRAMES	63
define	WCS		64

define	XFORMAT		66
define	YFORMAT		67
define	FITPLOT		68
define	SIGMA		69
define	AXES		70
define	FITTYPE		71
define	BETA		72
define	ITERATIONS	73

define	OUTPUT		75
define	NCOUTPUT	76
define	NLOUTPUT	77

 
# IE_COLON -- Respond to colon commands.
 
procedure ie_colon (ie, cmdstr, gp, redraw)
 
pointer	ie			# IMEXAM data structure
char	cmdstr[ARB]		# Colon command
pointer	gp			# GIO pointer
int	redraw			# Redraw graph?
 
char	gtype
bool	bval
real	rval1
int	ival, ncmd
pointer	sp, cmd, pp
 
bool	clgetb(), clgpsetb()
char	clgetc()
real	clgetr(), clgpsetr()
int	nscan(), strdic(), clgeti()
pointer	clopset()
errchk	clopset, clppsetb, clppsetr, clputb, clputi, clputr

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
 
	# Scan the command string and get the first word.
	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, CMDS)
	if (ncmd == 0) {
	    call printf ("Unrecognized or ambiguous command\007")
	    call sfree (sp)
	    return
	}

	gtype = IE_GTYPE(ie)
	pp = IE_PP(ie)

	# Special optimization for the a key.
	switch (ncmd) {
	case BACKGROUND, CENTER, NAVERAGE, RPLOT, XORDER, WIDTH:
	    if (IE_LASTKEY(ie) == 'a') {
	       gtype = 'r'
	       pp = clopset ("rimexam")
	    }
	    if (IE_LASTKEY(ie) == ',') {
	       gtype = '.'
	       pp = clopset ("rimexam")
	    }
	}

	# Switch on the command and possibly read further arguments.
	switch (ncmd) {
	case ANGH:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("angh %g\n")
		    call pargr (clgetr ("simexam.angh"))
	    } else {
		call clputr ("simexam.angh", rval1)
		if (gtype == 's')
		    redraw = YES
	    }
	case ANGV:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("angv %g\n")
		    call pargr (clgetr ("simexam.angv"))
	    } else {
		call clputr ("simexam.angv", rval1)
		if (gtype == 's')
		    redraw = YES
	    }
	case BACKGROUND:
	    switch (gtype) {
	    case 'j', 'k', 'r', '.':
		call gargb (bval)
		if (nscan() == 1) {
		    call printf ("background %b\n")
			call pargb (clgpsetb (pp, "background"))
		} else {
		    call clppsetb (pp, "background", bval)
		    if (pp == IE_PP(ie))
			redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case BANNER:
	    switch (gtype) {
	    case 'c', 'u', 'j', 'k', 'l', 'r', 'v', 'e', 'h', '.':
	        call gargb (bval)
	        if (nscan() == 2) {
		    call clppsetb (pp, "banner", bval)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case BOUNDARY:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, BTYPES)
	    if (ncmd == 0) {
		call printf ("Boundary types are %s\n")
		    call pargstr (BTYPES)
	    } else
		call clpstr ("vimexam.boundary", Memc[cmd])
	case BOX:
	    switch (gtype) {
	    case 'c', 'u', 'j', 'k', 'l', 'r', 'v', 'e', 'h', '.':
	        call gargb (bval)
	        if (nscan() == 2) {
		    call clppsetb (pp, "box", bval)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case BUFFER:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("buffer %g\n")
		    call pargr (clgetr ("rimexam.buffer"))
	    } else {
		call clputr ("rimexam.buffer", rval1)
		if (gtype == 'r' || gtype == '.')
		    redraw = YES
	    }
	case CEILING:
	    switch (gtype) {
	    case 's', 'e':
	        call gargr (rval1)
	        if (nscan() == 1) {
		    call printf ("ceiling %g\n")
		        call pargr (clgpsetr (pp, "ceiling"))
	        } else {
		    call clppsetr (pp, "ceiling", rval1)
		    redraw = YES
	        }
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case CENTER:
	    switch (gtype) {
	    case 'j', 'k', 'r', '.':
		call gargb (bval)
		if (nscan() == 1) {
		    call printf ("center %b\n")
			call pargb (clgpsetb (pp, "center"))
		} else {
		    call clppsetb (pp, "center", bval)
		    if (pp == IE_PP(ie))
			redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case CONSTANT:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("constant %g\n")
		    call pargr (clgetr ("vimexam.constant"))
	    } else
		call clputr ("vimexam.constant", rval1)
	case DASHPAT:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("dashpat %g\n")
		    call pargi (clgeti ("eimexam.dashpat"))
	    } else {
		call clputi ("eimexam.dashpat", ival)
		if (gtype == 'e')
		    redraw = YES
	    }
	case DEFKEY:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan() == 1) {
		call printf ("defkey %c\n")
		    call pargc (clgetc ("defkey"))
	    } else
		call clputc ("defkey", Memc[cmd])
	case EPARAM:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan() == 1)
		Memc[cmd] = gtype

	    switch (Memc[cmd]) {
	    case 'c', 'u', 'j', 'k', 'l', 'r', 'v', 'e', 's', 'h', '.':
		call gdeactivate (gp, 0)
	        switch (Memc[cmd]) {
	        case 'c':
		    call clcmdw ("eparam cimexam")
	        case 'j':
		    call clcmdw ("eparam jimexam")
	        case 'k':
		    call clcmdw ("eparam kimexam")
	        case 'l':
		    call clcmdw ("eparam limexam")
	        case 'r', '.':
		    call clcmdw ("eparam rimexam")
	        case 's':
		    call clcmdw ("eparam simexam")
	        case 'u', 'v':
		    call clcmdw ("eparam vimexam")
	        case 'e':
		    call clcmdw ("eparam eimexam")
	        case 'h':
		    call clcmdw ("eparam himexam")
		}
		if (Memc[cmd] == gtype)
		    redraw = YES
	    }
	case FILL:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("fill %b\n")
		    call pargb (clgetb ("eimexam.fill"))
	    } else {
		call clputb ("eimexam.fill", bval)
		if (gtype == 'e')
		    redraw = YES
	    }
	case FLOOR:
	    switch (gtype) {
	    case 's', 'e':
	        call gargr (rval1)
	        if (nscan() == 1) {
		    call printf ("floor %g\n")
		        call pargr (clgpsetr (pp, "floor"))
	        } else {
		    call clppsetr (pp, "floor", rval1)
		    redraw = YES
	        }
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case INTERVAL:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("interval %g\n")
		    call pargr (clgetr ("eimexam.interval"))
	    } else {
		call clputr ("eimexam.interval", rval1)
		if (gtype == 'e')
		    redraw = YES
	    }
	case LABEL:
	    call gargb (bval)
	    if (nscan() == 2) {
		call clputb ("eimexam.label", bval)
		if (gtype == 'e')
		    redraw = YES
	    }

	case LOGFILE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan() == 1) {
		call strcpy (IE_LOGFILE(ie), Memc[cmd], SZ_LINE)
		if (IE_LOGFD(ie) == NULL) {
		    call printf ("logfile %s [closed]\n")
			call pargstr (Memc[cmd])
		} else {
		    call printf ("logfile %s [open]\n")
			call pargstr (Memc[cmd])
		}
	    } else {
		call clpstr ("logfile", Memc[cmd])
		if (IE_LOGFD(ie) != NULL) {
		    call close (IE_LOGFD(ie))
		    IE_LOGFD(ie) = NULL
		}

		call clgstr ("logfile", IE_LOGFILE(ie), SZ_LINE)
		if (clgetb ("keeplog"))
		    iferr (call ie_openlog (ie))
			call erract (EA_WARN)
	    }

	case LOGX:
	    switch (gtype) {
	    case 'c', 'u', 'j', 'k', 'l', 'r', 'v', 'h', '.':
	        call gargb (bval)
	        if (nscan() == 2) {
		    call clppsetb (pp, "logx", bval)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case LOGY:
	    switch (gtype) {
	    case 'c', 'u', 'j', 'k', 'l', 'r', 'v', 'h', '.':
	        call gargb (bval)
	        if (nscan() == 2) {
		    call clppsetb (pp, "logy", bval)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case MAGZERO:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("magzero %g\n")
		    call pargr (clgetr ("rimexam.magzero"))
	    } else {
		call clputr ("rimexam.magzero", rval1)
		if (gtype == 'r' || gtype == '.')
		    redraw = YES
	    }
	case AUTOREDRAW:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("autoredraw %b\n")
		    call pargb (clgetb ("autoredraw"))
	    } else
		call clputb ("autoredraw", bval)
	default:
	    call ie_colon1 (ie, ncmd, gp, pp, gtype, redraw)
	}
 
	if (pp != IE_PP(ie))
	    call clcpset (pp)
	if (redraw == YES && !clgetb ("autoredraw"))
	    redraw = NO
	call sfree (sp)
end


# IE_COLON1 -- Subprocedure to get around too many strings error in xc.

procedure ie_colon1 (ie, ncmd, gp, pp, gtype, redraw)
 
pointer	ie			# IMEXAM data structure
int	ncmd			# Command number
pointer	gp			# GIO pointer
pointer	pp			# Pset pointer
char	gtype			# Graph type
int	redraw			# Redraw graph?
 
int	ival
real	rval1, rval2
bool	bval
pointer	sp, cmd, im
 
real	clgetr(), clgpsetr()
pointer	ie_gimage()
int	nscan(), strdic(), clgeti(), clgpseti()
errchk	ie_gimage, clppseti

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	switch (ncmd) {
	case MAJRX:
	    switch (gtype) {
	    case 'c', 'u', 'j', 'k', 'l', 'r', 'v', 'e', 'h', '.':
	        call gargi (ival)
		if (nscan() == 1) {
		    call printf ("majrx %d\n")
			call pargi (clgpseti (pp, "majrx"))
		} else {
		    call clppseti (pp, "majrx", ival)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case MAJRY:
	    switch (gtype) {
	    case 'c', 'u', 'j', 'k', 'l', 'r', 'v', 'e', 'h', '.':
	        call gargi (ival)
		if (nscan() == 1) {
		    call printf ("majry %d\n")
			call pargi (clgpseti (pp, "majry"))
		} else {
		    call clppseti (pp, "majry", ival)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case MARKER:
	    switch (gtype) {
	    case 'c', 'u', 'j', 'k', 'l', 'r', 'v', 'h', '.':
	        call gargwrd (Memc[cmd], SZ_LINE)
	        ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, MTYPES)
	        if (ncmd == 0) {
		    call printf ("Marker types are %s\n")
		        call pargstr (MTYPES)
	        } else {
		    call clppset (pp, "marker", Memc[cmd])
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case MINRX:
	    switch (gtype) {
	    case 'c', 'u', 'j', 'k', 'l', 'r', 'v', 'e', 'h', '.':
	        call gargi (ival)
		if (nscan() == 1) {
		    call printf ("minrx %d\n")
			call pargi (clgpseti (pp, "minrx"))
		} else {
		    call clppseti (pp, "minrx", ival)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case MINRY:
	    switch (gtype) {
	    case 'c', 'u', 'j', 'k', 'l', 'r', 'v', 'e', 'h', '.':
	        call gargi (ival)
		if (nscan() == 1) {
		    call printf ("minry %d\n")
			call pargi (clgpseti (pp, "minry"))
		} else {
		    call clppseti (pp, "minry", ival)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case NAVERAGE:
	    switch (gtype) {
	    case 'c', 'u', 'j', 'k', 'l', 'v':
	        call gargi (ival)
		if (nscan() == 1) {
		    call printf ("naverage %d\n")
			call pargi (clgpseti (pp, "naverage"))
		} else {
		    call clppseti (pp, "naverage", ival)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case NCOLUMNS:
	    switch (gtype) {
	    case 's', 'e', 'h':
	        call gargi (ival)
		if (nscan() == 1) {
		    call printf ("ncolumns %d\n")
			call pargi (clgpseti (pp, "ncolumns"))
		} else {
		    call clppseti (pp, "ncolumns", ival)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case NCONTOURS:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("ncontours %g\n")
		    call pargi (clgeti ("eimexam.ncontours"))
	    } else {
		call clputi ("eimexam.ncontours", ival)
		if (gtype == 'e')
		    redraw = YES
	    }
	case NCSTAT:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("ncstat %g\n")
		    call pargi (clgeti ("ncstat"))
	    } else
		call clputi ("ncstat", ival)
	case NHI:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("nhi %g\n")
		    call pargi (clgeti ("eimexam.nhi"))
	    } else {
		call clputi ("eimexam.nhi", ival)
		if (gtype == 'e')
		    redraw = YES
	    }
	case NLINES:
	    switch (gtype) {
	    case 's', 'e', 'h':
	        call gargi (ival)
		if (nscan() == 1) {
		    call printf ("nlines %d\n")
			call pargi (clgpseti (pp, "nlines"))
		} else {
		    call clppseti (pp, "nlines", ival)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case NLSTAT:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("nlstat %g\n")
		    call pargi (clgeti ("nlstat"))
	    } else
		call clputi ("nlstat", ival)
	case POINTMODE:
	    switch (gtype) {
	    case 'c', 'u', 'j', 'k', 'l', 'r', 'v', 'h', '.':
	        call gargb (bval)
	        if (nscan() == 2) {
		    call clppsetb (pp, "pointmode", bval)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case RADIUS:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("radius %g\n")
		    call pargr (clgetr ("rimexam.radius"))
	    } else {
		call clputr ("rimexam.radius", rval1)
		if (gtype == 'r' || gtype == '.')
		    redraw = YES
	    }
	case ROUND:
	    switch (gtype) {
	    case 'c', 'u', 'j', 'k', 'l', 'r', 'v', 'e', 'h', '.':
	        call gargb (bval)
	        if (nscan() == 2) {
		    call clppsetb (pp, "round", bval)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case RPLOT:
	    switch (gtype) {
	    case 'j', 'k', 'r', '.':
		call gargr (rval1)
		if (nscan() == 1) {
		    call printf ("rplot %g\n")
			call pargr (clgpsetr (pp, "rplot"))
		} else {
		    call clppsetr (pp, "rplot", rval1)
		    if (pp == IE_PP(ie))
			redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case SELECT:
	    call gargi (ival)
	    if (nscan () > 1) {
		if (IE_LIST(ie) != NULL)
		    IE_INDEX(ie) = ival
		else
		    IE_NEWFRAME(ie) = ival
		IE_MAPFRAME(ie) = 0
		iferr (im = ie_gimage (ie, YES))
		    call erract (EA_WARN)
	    }
	case SZMARKER:
	    switch (gtype) {
	    case 'c', 'u', 'j', 'k', 'l', 'r', 'v', 'h', '.':
	        call gargi (ival)
		if (nscan() == 1) {
		    call printf ("szmarker %d\n")
			call pargi (clgpseti (pp, "szmarker"))
		} else {
		    call clppseti (pp, "szmarker", ival)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case TICKLABELS:
	    switch (gtype) {
	    case 'c', 'u', 'j', 'k', 'l', 'r', 'v', 'e', 'h', '.':
	        call gargb (bval)
	        if (nscan() == 2) {
		    call clppsetb (pp, "ticklabels", bval)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case TITLE:
	    switch (gtype) {
	    case 'c', 'u', 'j', 'k', 'l', 'r', 's', 'v', 'e', 'h', '.':
		Memc[cmd] = EOS
		call gargstr (Memc[cmd], SZ_LINE)
		call clppset (pp, "title", Memc[cmd])
		redraw = YES
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case WIDTH:
	    switch (gtype) {
	    case 'j', 'k', 'r', '.':
		call gargr (rval1)
		if (nscan() == 1) {
		    call printf ("width %g\n")
			call pargr (clgpsetr (pp, "width"))
		} else {
		    call clppsetr (pp, "width", rval1)
		    if (pp == IE_PP(ie))
			redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case X:
	    switch (gtype) {
	    case 'c', 'j', 'k', 'l', 'r', 'v', 'h', '.':
	        call gargr (rval1)
	        call gargr (rval2)
	        if (nscan() < 3) {
		    call clppsetr (pp, "x1", INDEF)
		    call clppsetr (pp, "x2", INDEF)
		} else {
		    call clppsetr (pp, "x1", rval1)
		    call clppsetr (pp, "x2", rval2)
		}
		redraw = YES
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case XLABEL:
	    switch (gtype) {
	    case 'c', 'u', 'j', 'k', 'l', 'r', 'v', 'e', 'h', '.':
		Memc[cmd] = EOS
		call gargstr (Memc[cmd], SZ_LINE)
		call clppset (pp, "xlabel", Memc[cmd])
		redraw = YES
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case XORDER:
	    switch (gtype) {
	    case 'j', 'k', 'r', '.':
		call gargi (ival)
		if (nscan() == 1) {
		    call printf ("xorder %d\n")
			call pargi (clgpseti (pp, "xorder"))
		} else {
		    call clppseti (pp, "xorder", ival)
		    if (pp == IE_PP(ie))
			redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case Y:
	    switch (gtype) {
	    case 'c', 'j', 'k', 'l', 'r', 'v', 'h', '.':
	        call gargr (rval1)
	        call gargr (rval2)
	        if (nscan() < 3) {
		    call clppsetr (pp, "y1", INDEF)
		    call clppsetr (pp, "y2", INDEF)
		} else {
		    call clppsetr (pp, "y1", rval1)
		    call clppsetr (pp, "y2", rval2)
		}
		redraw = YES
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	default:
	    call ie_colon2 (ie, ncmd, gp, pp, gtype, redraw)
	}

	call sfree (sp)
end


# IE_COLON2 -- Subprocedure to get around too many strings error in xc.

procedure ie_colon2 (ie, ncmd, gp, pp, gtype, redraw)
 
pointer	ie			# IMEXAM data structure
int	ncmd			# Command number
pointer	gp			# GIO pointer
pointer	pp			# Pset pointer
char	gtype			# Graph type
int	redraw			# Redraw graph?
 
int	ival
real	rval1
bool	bval
pointer	sp, cmd
 
real	clgetr()
bool	clgetb()
int	nscan(), clgeti(), btoi(), strdic()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	switch (ncmd) {
	case YLABEL:
	    switch (gtype) {
	    case 'c', 'u', 'j', 'k', 'l', 'r', 'v', 'e', 'h', '.':
		Memc[cmd] = EOS
		call gargstr (Memc[cmd], SZ_LINE)
		call clppset (pp, "ylabel", Memc[cmd])
		redraw = YES
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case YORDER:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("yorder %d\n")
		    call pargi (clgeti ("rimexam.yorder"))
	    } else {
		call clputi ("rimexam.yorder", ival)
		if (gtype == 'r' || gtype == '.')
		    redraw = YES
	    }
	case ZERO:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("zero %g\n")
		    call pargr (clgetr ("eimexam.zero"))
	    } else {
		call clputr ("eimexam.zero", rval1)
		if (gtype == 'e')
		    redraw = YES
	    }
	case UNLEARN:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan() == 1)
		Memc[cmd] = gtype

	    switch (Memc[cmd]) {
	    case 'c', 'u', 'j', 'k', 'l', 'r', 'v', 'e', 's', 'h', '.':
	        switch (Memc[cmd]) {
	        case 'c':
		    call clcmdw ("unlearn cimexam")
	        case 'j':
		    call clcmdw ("unlearn jimexam")
	        case 'k':
		    call clcmdw ("unlearn jimexam")
	        case 'l':
		    call clcmdw ("unlearn limexam")
	        case 'r', '.':
		    call clcmdw ("unlearn rimexam")
	        case 's':
		    call clcmdw ("unlearn simexam")
	        case 'u', 'v':
		    call clcmdw ("unlearn vimexam")
	        case 'e':
		    call clcmdw ("unlearn eimexam")
	        case 'h':
		    call clcmdw ("unlearn himexam")
		}
		if (Memc[cmd] == gtype)
		    redraw = YES
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case NBINS:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("nbins %d\n")
		    call pargi (clgeti ("himexam.nbins"))
	    } else {
		call clputi ("himexam.nbins", ival)
		if (gtype == 'h')
		    redraw = YES
	    }
	case Z1:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("z1 %g\n")
		    call pargr (clgetr ("himexam.z1"))
	    } else {
		call clputr ("himexam.z1", rval1)
		if (gtype == 'h')
		    redraw = YES
	    }
	case Z2:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("z2 %g\n")
		    call pargr (clgetr ("himexam.z2"))
	    } else {
		call clputr ("himexam.z2", rval1)
		if (gtype == 'h')
		    redraw = YES
	    }
	case AUTOSCALE:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("autoscale %b\n")
		    call pargb (clgetb ("himexam.autoscale"))
	    } else {
		call clputb ("himexam.autoscale", bval)
		if (gtype == 'h')
		    redraw = YES
	    }
	case TOP_CLOSED:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("top_closed %b\n")
		    call pargb (clgetb ("himexam.top_closed"))
	    } else {
		call clputb ("himexam.top_closed", bval)
		if (gtype == 'h')
		    redraw = YES
	    }
	case ALLFRAMES:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("allframes %b\n")
		    call pargb (clgetb ("allframes"))
	    } else {
		call clputb ("allframes", bval)
		IE_ALLFRAMES(ie) = btoi (bval)
	    }
	case WCS:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan() == 1) {
		call printf ("wcs %s\n")
		    call pargstr (IE_WCSNAME(ie))
	    } else {
		call strcpy (Memc[cmd], IE_WCSNAME(ie), SZ_FNAME)
		call ie_mwinit (ie)
		redraw = YES
	    }
	case XFORMAT:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan() == 1)
		call clpstr ("xformat", "")
	    else
		call clpstr ("xformat", Memc[cmd])
	case YFORMAT:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan() == 1)
		call clpstr ("yformat", "")
	    else
		call clpstr ("yformat", Memc[cmd])
	case FITPLOT:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("fitplot %b\n")
		    call pargb (clgetb ("rimexam.fitplot"))
	    } else {
		call clputb ("rimexam.fitplot", bval)
		if (gtype == 'r')
		    redraw = YES
	    }
	case SIGMA:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("sigma %g\n")
		    call pargr (clgetr ("jimexam.sigma"))
	    } else {
		call clputr ("jimexam.sigma", rval1)
		if (gtype == 'j' || gtype == 'k')
		    redraw = YES
	    }
	case AXES:
	    call gargb (bval)
	    if (nscan() == 2) {
		call clputb ("simexam.axes", bval)
		if (gtype == 's')
		    redraw = YES
	    }
	case FITTYPE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan() == 1) {
		call clgstr ("rimexam.fittype", Memc[cmd], SZ_LINE)
		call printf ("fittype %s\n")
		    call pargstr (Memc[cmd])
	    } else {
		ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE,
		    "|gaussian|moffat|")
		if (ncmd == 0) {
		    call printf ("Profile fit types are %s\n")
			call pargstr ("|gaussian|moffat|")
		} else {
		    call clpstr ("rimexam.fittype", Memc[cmd])
		    if (gtype == 'r' || gtype == '.')
			redraw = YES
		}
	    }
	case BETA:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("beta %g\n")
		    call pargr (clgetr ("rimexam.beta"))
	    } else {
		call clputr ("rimexam.beta", rval1)
		if (gtype == 'r' || gtype == '.')
		    redraw = YES
	    }
	case ITERATIONS:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("iterations %d\n")
		    call pargi (clgeti ("rimexam.iterations"))
	    } else {
		call clputi ("rimexam.iterations", ival)
		if (gtype == 'r')
		    redraw = YES
	    }

	case OUTPUT:
	    call gargwrd (Memc[cmd], SZ_FNAME)
	    if (nscan() == 1) {
		call clgstr ("output", Memc[cmd], SZ_FNAME)
		call printf ("output `%s'\n")
		    call pargstr (Memc[cmd])
	    } else
		call clpstr ("output", Memc[cmd])
	case NCOUTPUT:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("ncoutput %g\n")
		    call pargi (clgeti ("ncoutput"))
	    } else
		call clputi ("ncoutput", ival)
	case NLOUTPUT:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("nloutput %g\n")
		    call pargi (clgeti ("nloutput"))
	    } else
		call clputi ("nloutput", ival)

	default:
	    call printf ("Ambiguous or unrecognized command\007\n")
	}

	call sfree (sp)
end
