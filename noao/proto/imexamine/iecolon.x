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
		|autoredraw|nbins|z1|z2|autoscale|top_closed|allframes|"
 
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

 
# IE_COLON -- Respond to colon commands.
 
procedure ie_colon (ie, cmdstr, gp, redraw)
 
pointer	ie			# IMEXAM data structure
char	cmdstr[ARB]		# Colon command
pointer	gp			# GIO pointer
int	redraw			# Redraw graph?
 
bool	bval
real	rval1
int	ival, ncmd
pointer	sp, cmd
 
bool	clgetb()
char	clgetc()
real	clgetr(), clgpsetr()
int	nscan(), strdic(), clgeti()
errchk	clppsetb, clppsetr, clputb, clputi, clputr

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
 
	# Scan the command string and get the first word.
	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, CMDS)
 
	# Switch on the command and possibly read further arguments.
	switch (ncmd) {
	case ANGH:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("angh %g\n")
		    call pargr (clgetr ("simexam.angh"))
	    } else {
		call clputr ("simexam.angh", rval1)
		if (IE_GTYPE(ie) == 's')
		    redraw = YES
	    }
	case ANGV:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("angv %g\n")
		    call pargr (clgetr ("simexam.angv"))
	    } else {
		call clputr ("simexam.angv", rval1)
		if (IE_GTYPE(ie) == 's')
		    redraw = YES
	    }
	case BACKGROUND:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("background %b\n")
		    call pargb (clgetb ("rimexam.background"))
	    } else {
		call clputb ("rimexam.background", bval)
		if (IE_GTYPE(ie) == 'r')
		    redraw = YES
	    }
	case BANNER:
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'u', 'l', 'r', 'v', 'e', 'h':
	        call gargb (bval)
	        if (nscan() == 2) {
		    call clppsetb (IE_PP(ie), "banner", bval)
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
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'u', 'l', 'r', 'v', 'e', 'h':
	        call gargb (bval)
	        if (nscan() == 2) {
		    call clppsetb (IE_PP(ie), "box", bval)
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
		if (IE_GTYPE(ie) == 'r')
		    redraw = YES
	    }
	case CEILING:
	    switch (IE_GTYPE(ie)) {
	    case 's', 'e':
	        call gargr (rval1)
	        if (nscan() == 1) {
		    call printf ("ceiling %g\n")
		        call pargr (clgpsetr (IE_PP(ie), "ceiling"))
	        } else {
		    call clppsetr (IE_PP(ie), "ceiling", rval1)
		    redraw = YES
	        }
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case CENTER:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("center %b\n")
		    call pargb (clgetb ("rimexam.center"))
	    } else {
		call clputb ("rimexam.center", bval)
		if (IE_GTYPE(ie) == 'r')
		    redraw = YES
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
		if (IE_GTYPE(ie) == 'e')
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
		Memc[cmd] = IE_GTYPE(ie)

	    switch (Memc[cmd]) {
	    case 'c', 'u', 'l', 'r', 'v', 'e', 's', 'h':
		call gdeactivate (gp, 0)
	        switch (Memc[cmd]) {
	        case 'c':
		    call clcmdw ("eparam cimexam")
	        case 'l':
		    call clcmdw ("eparam limexam")
	        case 'r':
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
		if (Memc[cmd] == IE_GTYPE(ie))
		    redraw = YES
	    }
	case FILL:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("fill %b\n")
		    call pargb (clgetb ("eimexam.fill"))
	    } else {
		call clputb ("eimexam.fill", bval)
		if (IE_GTYPE(ie) == 'e')
		    redraw = YES
	    }
	case FLOOR:
	    switch (IE_GTYPE(ie)) {
	    case 's', 'e':
	        call gargr (rval1)
	        if (nscan() == 1) {
		    call printf ("floor %g\n")
		        call pargr (clgpsetr (IE_PP(ie), "floor"))
	        } else {
		    call clppsetr (IE_PP(ie), "floor", rval1)
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
		if (IE_GTYPE(ie) == 'e')
		    redraw = YES
	    }
	case LABEL:
	    call gargb (bval)
	    if (nscan() == 2) {
		call clputb ("simexam.label", bval)
		if (IE_GTYPE(ie) == 's')
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
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'u', 'l', 'r', 'v', 'h':
	        call gargb (bval)
	        if (nscan() == 2) {
		    call clppsetb (IE_PP(ie), "logx", bval)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case LOGY:
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'u', 'l', 'r', 'v', 'h':
	        call gargb (bval)
	        if (nscan() == 2) {
		    call clppsetb (IE_PP(ie), "logy", bval)
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
		if (IE_GTYPE(ie) == 'r')
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
	    call ie_colon1 (ie, ncmd, gp, redraw)
	}
 
	if (redraw == YES && !clgetb ("autoredraw"))
	    redraw = NO
	call sfree (sp)
end


# IE_COLON1 -- Subprocedure to get around too many strings error in xc.

procedure ie_colon1 (ie, ncmd, gp, redraw)
 
pointer	ie			# IMEXAM data structure
int	ncmd			# Command number
pointer	gp			# GIO pointer
int	redraw			# Redraw graph?
 
int	ival
real	rval1, rval2
bool	bval
pointer	sp, cmd, im
 
real	clgetr()
pointer	ie_gimage()
int	nscan(), strdic(), clgeti(), clgpseti()
errchk	ie_gimage, clppseti

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	switch (ncmd) {
	case MAJRX:
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'u', 'l', 'r', 'v', 'e', 'h':
	        call gargi (ival)
		if (nscan() == 1) {
		    call printf ("majrx %d\n")
			call pargi (clgpseti (IE_PP(ie), "majrx"))
		} else {
		    call clppseti (IE_PP(ie), "majrx", ival)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case MAJRY:
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'u', 'l', 'r', 'v', 'e', 'h':
	        call gargi (ival)
		if (nscan() == 1) {
		    call printf ("majry %d\n")
			call pargi (clgpseti (IE_PP(ie), "majry"))
		} else {
		    call clppseti (IE_PP(ie), "majry", ival)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case MARKER:
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'u', 'l', 'r', 'v', 'h':
	        call gargwrd (Memc[cmd], SZ_LINE)
	        ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, MTYPES)
	        if (ncmd == 0) {
		    call printf ("Marker types are %s\n")
		        call pargstr (MTYPES)
	        } else {
		    call clppset (IE_PP(ie), "marker", Memc[cmd])
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case MINRX:
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'u', 'l', 'r', 'v', 'e', 'h':
	        call gargi (ival)
		if (nscan() == 1) {
		    call printf ("minrx %d\n")
			call pargi (clgpseti (IE_PP(ie), "minrx"))
		} else {
		    call clppseti (IE_PP(ie), "minrx", ival)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case MINRY:
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'u', 'l', 'r', 'v', 'e', 'h':
	        call gargi (ival)
		if (nscan() == 1) {
		    call printf ("minry %d\n")
			call pargi (clgpseti (IE_PP(ie), "minry"))
		} else {
		    call clppseti (IE_PP(ie), "minry", ival)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case NAVERAGE:
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'u', 'l', 'v':
	        call gargi (ival)
		if (nscan() == 1) {
		    call printf ("naverage %d\n")
			call pargi (clgpseti (IE_PP(ie), "naverage"))
		} else {
		    call clppseti (IE_PP(ie), "naverage", ival)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case NCOLUMNS:
	    switch (IE_GTYPE(ie)) {
	    case 's', 'e', 'h':
	        call gargi (ival)
		if (nscan() == 1) {
		    call printf ("ncolumns %d\n")
			call pargi (clgpseti (IE_PP(ie), "ncolumns"))
		} else {
		    call clppseti (IE_PP(ie), "ncolumns", ival)
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
		if (IE_GTYPE(ie) == 'e')
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
		if (IE_GTYPE(ie) == 'e')
		    redraw = YES
	    }
	case NLINES:
	    switch (IE_GTYPE(ie)) {
	    case 's', 'e', 'h':
	        call gargi (ival)
		if (nscan() == 1) {
		    call printf ("nlines %d\n")
			call pargi (clgpseti (IE_PP(ie), "nlines"))
		} else {
		    call clppseti (IE_PP(ie), "nlines", ival)
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
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'u', 'l', 'r', 'v', 'h':
	        call gargb (bval)
	        if (nscan() == 2) {
		    call clppsetb (IE_PP(ie), "pointmode", bval)
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
		if (IE_GTYPE(ie) == 'r')
		    redraw = YES
	    }
	case ROUND:
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'u', 'l', 'r', 'v', 'e', 'h':
	        call gargb (bval)
	        if (nscan() == 2) {
		    call clppsetb (IE_PP(ie), "round", bval)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case RPLOT:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("rplot %g\n")
		    call pargr (clgetr ("rimexam.rplot"))
	    } else {
		call clputr ("rimexam.rplot", rval1)
		if (IE_GTYPE(ie) == 'r')
		    redraw = YES
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
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'u', 'l', 'r', 'v', 'h':
	        call gargi (ival)
		if (nscan() == 1) {
		    call printf ("szmarker %d\n")
			call pargi (clgpseti (IE_PP(ie), "szmarker"))
		} else {
		    call clppseti (IE_PP(ie), "szmarker", ival)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case TICKLABELS:
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'u', 'l', 'r', 'v', 'e', 'h':
	        call gargb (bval)
	        if (nscan() == 2) {
		    call clppsetb (IE_PP(ie), "ticklabels", bval)
		    redraw = YES
		}
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case TITLE:
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'u', 'l', 'r', 's', 'v', 'e', 'h':
		Memc[cmd] = EOS
		call gargstr (Memc[cmd], SZ_LINE)
		call clppset (IE_PP(ie), "title", Memc[cmd])
		redraw = YES
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case WIDTH:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("width %g\n")
		    call pargr (clgetr ("rimexam.width"))
	    } else {
		call clputr ("rimexam.width", rval1)
		if (IE_GTYPE(ie) == 'r')
		    redraw = YES
	    }
	case X:
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'l', 'r', 'v', 'h':
	        call gargr (rval1)
	        call gargr (rval2)
	        if (nscan() < 3) {
		    call clppsetr (IE_PP(ie), "x1", INDEF)
		    call clppsetr (IE_PP(ie), "x2", INDEF)
		} else {
		    call clppsetr (IE_PP(ie), "x1", rval1)
		    call clppsetr (IE_PP(ie), "x2", rval2)
		}
		redraw = YES
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case XLABEL:
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'u', 'l', 'r', 'v', 'e', 'h':
		Memc[cmd] = EOS
		call gargstr (Memc[cmd], SZ_LINE)
		call clppset (IE_PP(ie), "xlabel", Memc[cmd])
		redraw = YES
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case XORDER:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("xorder %d\n")
		    call pargi (clgeti ("rimexam.xorder"))
	    } else {
		call clputi ("rimexam.xorder", ival)
		if (IE_GTYPE(ie) == 'r')
		    redraw = YES
	    }
	case Y:
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'l', 'r', 'v', 'h':
	        call gargr (rval1)
	        call gargr (rval2)
	        if (nscan() < 3) {
		    call clppsetr (IE_PP(ie), "y1", INDEF)
		    call clppsetr (IE_PP(ie), "y2", INDEF)
		} else {
		    call clppsetr (IE_PP(ie), "y1", rval1)
		    call clppsetr (IE_PP(ie), "y2", rval2)
		}
		redraw = YES
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	case YLABEL:
	    switch (IE_GTYPE(ie)) {
	    case 'c', 'u', 'l', 'r', 'v', 'e', 'h':
		Memc[cmd] = EOS
		call gargstr (Memc[cmd], SZ_LINE)
		call clppset (IE_PP(ie), "ylabel", Memc[cmd])
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
		if (IE_GTYPE(ie) == 'r')
		    redraw = YES
	    }
	case ZERO:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("zero %g\n")
		    call pargr (clgetr ("eimexam.zero"))
	    } else {
		call clputr ("eimexam.zero", rval1)
		if (IE_GTYPE(ie) == 'e')
		    redraw = YES
	    }
	case UNLEARN:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan() == 1)
		Memc[cmd] = IE_GTYPE(ie)

	    switch (Memc[cmd]) {
	    case 'c', 'u', 'l', 'r', 'v', 'e', 's', 'h':
	        switch (Memc[cmd]) {
	        case 'c':
		    call clcmdw ("unlearn cimexam")
	        case 'l':
		    call clcmdw ("unlearn limexam")
	        case 'r':
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
		if (Memc[cmd] == IE_GTYPE(ie))
		    redraw = YES
	    default:
		call printf ("Parameter does not apply to current graph\007\n")
	    }
	default:
	    call ie_colon2 (ie, ncmd, gp, redraw)
	}

	call sfree (sp)
end


# IE_COLON2 -- Subprocedure to get around too many strings error in xc.

procedure ie_colon2 (ie, ncmd, gp, redraw)
 
pointer	ie			# IMEXAM data structure
int	ncmd			# Command number
pointer	gp			# GIO pointer
int	redraw			# Redraw graph?
 
int	ival
real	rval1
bool	bval
pointer	sp, cmd
 
real	clgetr()
bool	clgetb()
int	nscan(), clgeti(), btoi()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	switch (ncmd) {
	case NBINS:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("nbins %d\n")
		    call pargi (clgeti ("himexam.nbins"))
	    } else {
		call clputi ("himexam.nbins", ival)
		if (IE_GTYPE(ie) == 'h')
		    redraw = YES
	    }
	case Z1:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("z1 %g\n")
		    call pargr (clgetr ("himexam.z1"))
	    } else {
		call clputr ("himexam.z1", rval1)
		if (IE_GTYPE(ie) == 'h')
		    redraw = YES
	    }
	case Z2:
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("z2 %g\n")
		    call pargr (clgetr ("himexam.z2"))
	    } else {
		call clputr ("himexam.z2", rval1)
		if (IE_GTYPE(ie) == 'h')
		    redraw = YES
	    }
	case AUTOSCALE:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("autoscale %b\n")
		    call pargb (clgetb ("himexam.autoscale"))
	    } else {
		call clputb ("himexam.autoscale", bval)
		if (IE_GTYPE(ie) == 'h')
		    redraw = YES
	    }
	case TOP_CLOSED:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("top_closed %b\n")
		    call pargb (clgetb ("himexam.top_closed"))
	    } else {
		call clputb ("himexam.top_closed", bval)
		if (IE_GTYPE(ie) == 'r')
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
	    
	default:
	    call printf ("Ambiguous or unrecognized command\007\n")
	}

	call sfree (sp)
end
