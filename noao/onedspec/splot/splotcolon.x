include	<error.h>
include	<pkg/gtools.h>
include	"../shdr.h"
include	"../units.h"

# List of colon commands.
define	CMDS		 "|show|nolog|log|dispaxis|nsum|#|units|auto|zero\
			  |xydraw|histogram|nosysid|wreset|"
define	SHOW		1	# Show logged data
define	NOLOG		2	# Turn off logging
define	LOG		3	# Turn on logging
define	DA		4	# Dispersion axis
define	NS		5	# Summing parameter
define	COMMENT		6	# Comment
define	UNITS		7	# Units
define	AUTO		8	# Option auto graph
define	ZERO		9	# Option for zero y minimum
define	XYDRAW		10	# Draw connection X,Y pairs
define	HIST		11	# Draw histogram style lines
define	NOSYSID		12	# Don't include system id
define	WRESET		13	# Reset window for each new spectrum

define	OPOFF		7	# Offset in options array

# SPLOT_COLON -- Respond to colon command.

procedure splot_colon (command, options, gp, gt, sh, units, fname1, fname2,
	fd1, fd2, newgraph)

char	command[ARB]		# Colon command
int	options[ARB]		# Options
pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
pointer	sh			# SHIO pointer
char	units[SZ_FNAME]		# Units string
char	fname1[SZ_FNAME]	# Log file
char	fname2[SZ_FNAME]	# Temporary log file
int	fd1, fd2		# Log file descriptors
int	newgraph		# New graph?

bool	bval
char	cmd[SZ_LINE]
real	x, gt_getr()
int	ncmd, ival, access(), nscan(), strdic(), btoi()
errchk	un_changer

begin
	# Scan the command string and get the first word.
	call sscan (command)
	call gargwrd (cmd, SZ_LINE)
	ncmd = strdic (cmd, cmd, SZ_LINE, CMDS)

	switch (ncmd) {
	case SHOW:
	    if (fd2 != NULL) {
		call close (fd2)
		fd2 = NULL
	    }
	    if (access (fname2, 0, 0) == YES)
		call gpagefile (gp, fname2, "splot data")
	    else
		call printf ("No measurements\n")
	case NOLOG:
	    call printf ("Logging to %s disabled")
		call pargstr (fname1)
	    fname1[1] = EOS
	    if (fd1 != NULL) {
		call close (fd1) 
		fd1 = NULL
	    }
	case LOG:
	    call clgstr ("save_file", fname1, SZ_FNAME)
	    call printf ("Logging to %s enabled")
		call pargstr (fname1)
	case DA:
	    if (FORMAT(sh) == TWODSPEC) {
		call gargi (ival)
		if (nscan() == 2) {
		    if (ival < 1) {
			call printf ("Bad value for dispaxis (%d)\n")
			    call pargi (ival)
		    } else if (ival != DAXISP(sh)) {
			call shdr_close (sh)
			call shdr_2d (NULL, ival, INDEFI)
		    }
		} else {
		    call printf ("dispaxis %d\n")
			call pargi (DAXISP(sh))
		}
	    } else
		call printf ("Image is not two dimensional\n")
	case NS:
	    if (FORMAT(sh) == TWODSPEC) {
		call gargi (ival)
		if (nscan() == 2) {
		    if (ival < 1) {
			call printf ("Bad value for nsum (%d)\n")
			    call pargi (ival)
		    } else if (ival != NSUM(sh)) {
			call shdr_close (sh)
			call shdr_2d (NULL, INDEFI, ival)
		    }
		} else {
		    call printf ("nsum %d\n")
			call pargi (NSUM(sh))
		}
	    } else
		call printf ("Image is not two dimensional\n")
	case COMMENT:
	    call ans_hdr (sh, NO, fname1, fname2, fd1, fd2)
	    call gargstr (cmd, SZ_LINE)
	    if (fd1 != NULL) {
		call fprintf (fd1, "%s\n")
		    call pargstr (command)
	    }
	    if (fd2 != NULL) {
		call fprintf (fd2, "%s\n")
		    call pargstr (command)
	    }
	case UNITS:
	    call gargstr (cmd, SZ_LINE)
	    iferr {
		x = gt_getr (gt, GTXMIN)
		if (!IS_INDEF(x)) {
		    call un_changer (UN(sh), cmd, x, 1, NO)
		    call gt_setr (gt, GTXMIN, x)
		}
		x = gt_getr (gt, GTXMAX)
		if (!IS_INDEF(x)) {
		    call un_changer (UN(sh), cmd, x, 1, NO)
		    call gt_setr (gt, GTXMAX, x)
		}
		call un_changer (UN(sh), cmd, Memr[SX(sh)], SN(sh), YES)
		call strcpy (cmd, units, SZ_FNAME)
		call gt_sets (gt, GTXLABEL, UN_LABEL(UN(sh)))
		call gt_sets (gt, GTXUNITS, UN_UNITS(UN(sh)))
		newgraph = YES
	    } then
		call erract (EA_WARN)
	case AUTO:
	    call gargb (bval)
	    if (nscan() == 2)
		options[AUTO-OPOFF] = btoi (bval)
	    else {
		call printf ("auto %b\n")
		    call pargi (options[AUTO-OPOFF])
	    }
	case ZERO:
	    call gargb (bval)
	    if (nscan() == 2) {
		options[ZERO-OPOFF] = btoi (bval)
		if (bval)
		    call gt_setr (gt, GTYMIN, 0.)
		newgraph = options[AUTO-OPOFF]
	    } else {
		call printf ("zero %b\n")
		    call pargi (options[ZERO-OPOFF])
	    }
	case XYDRAW:
	    call gargb (bval)
	    if (nscan() == 2)
		options[XYDRAW-OPOFF] = btoi (bval)
	    else {
		call printf ("xydraw %b\n")
		    call pargi (options[XYDRAW-OPOFF])
	    }
	case HIST:
	    call gargb (bval)
	    if (nscan() == 2) {
		options[HIST-OPOFF] = btoi (bval)
		if (bval)
		    call gt_sets (gt, GTTYPE, "histogram")
		else
		    call gt_sets (gt, GTTYPE, "line")
		newgraph = options[AUTO-OPOFF]
	    } else {
		call printf ("hist %b\n")
		    call pargi (options[HIST-OPOFF])
	    }
	case NOSYSID:
	    call gargb (bval)
	    if (nscan() == 2) {
		options[NOSYSID-OPOFF] = btoi (bval)
		if (bval)
		    call gt_seti (gt, GTSYSID, NO)
		else
		    call gt_seti (gt, GTSYSID, YES)
		newgraph = options[AUTO-OPOFF]
	    } else {
		call printf ("nosysid %b\n")
		    call pargi (options[NOSYSID-OPOFF])
	    }
	case WRESET:
	    call gargb (bval)
	    if (nscan() == 2)
		options[WRESET-OPOFF] = btoi (bval)
	    else {
		call printf ("wreset %b\n")
		    call pargi (options[WRESET-OPOFF])
	    }

	default:
	    call printf ("Unrecognized or ambiguous command\007")
	}
end
