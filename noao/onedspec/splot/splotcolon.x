include	<error.h>
include	<pkg/gtools.h>
include	<smw.h>
include	<units.h>
include	<ctype.h>

# List of colon commands.
define	CMDS		 "|show|nolog|log|dispaxis|nsum|#|units|auto|zero\
			  |xydraw|histogram|nosysid|wreset|flip|overplot\
			  |label|mabove|mbelow|"
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
define	FLIP		14	# Flip the dispersion coordinates
define	OVERPLOT	15	# Toggle overplot
define	LABEL		16	# Label spectrum
define	MABOVE		17	# Tick mark plus label above spectrum
define	MBELOW		18	# Tick mark plus label below spectrum

define	OPOFF		7	# Offset in options array

# SPLOT_COLON -- Respond to colon command.

procedure splot_colon (command, options, gp, gt, sh, x, y, units,
	fname1, fname2, fd1, fd2, newgraph)

char	command[ARB]		# Colon command
int	options[ARB]		# Options
pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
pointer	sh			# SHIO pointer
real	x, y			# Coordinate
char	units[SZ_FNAME]		# Units string
char	fname1[SZ_FNAME]	# Log file
char	fname2[SZ_FNAME]	# Temporary log file
int	fd1, fd2		# Log file descriptors
int	newgraph		# New graph?

bool	bval
char	cmd[SZ_LINE]
real	xval, gt_getr()
int	ncmd, ival, access(), nscan(), strdic(), btoi(), gt_geti()
pointer	sp, str, smw
errchk	un_changer

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Scan the command string and get the first word.
	call sscan (command)
	call gargwrd (cmd, SZ_LINE)
	ncmd = strdic (cmd, cmd, SZ_LINE, CMDS)

	smw = MW(sh)

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
	    if (SMW_FORMAT(smw) == SMW_ND) {
		call gargi (ival)
		if (nscan() == 2) {
		    if (ival < 1) {
			call printf ("Bad value for dispaxis (%d)\n")
			    call pargi (ival)
		    } else if (ival != SMW_PAXIS(smw,1)) {
			call smw_daxis (smw, IM(sh), ival, INDEFI, INDEFI)
			call smw_saxes (smw, NULL, IM(sh))
			call shdr_close (sh)
		    }
		} else {
		    call printf ("dispaxis %d\n")
			call pargi (SMW_PAXIS(smw,1))
		}
	    } else
		call printf ("Image is not two dimensional\n")
	case NS:
	    if (SMW_FORMAT(smw) == SMW_ND) {
		call gargi (ival)
		call gargi (ncmd)
		if (nscan() == 1) {
		    call printf ("nsum %d %d\n")
			call pargi (SMW_NSUM(smw,1))
			call pargi (SMW_NSUM(smw,2))
		} else {
		    if (nscan() == 2)
			ncmd = INDEFI
		    if ((!IS_INDEFI(ival) && ival != SMW_NSUM(smw,1)) ||
			(!IS_INDEFI(ncmd) && ncmd != SMW_NSUM(smw,2))) {
			call smw_daxis (smw, IM(sh), INDEFI, ival, ncmd)
			call smw_saxes (smw, NULL, IM(sh))
			call shdr_close (sh)
		    }
		}
	    } else
		call printf ("Invalid image format\n")
	case COMMENT:
	    call ans_hdr (sh, NO, 'm', fname1, fname2, fd1, fd2)
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
	    for (ival=1; IS_WHITE(cmd[ival]); ival=ival+1)
		;
	    iferr {
		xval = gt_getr (gt, GTXMIN)
		if (!IS_INDEF(xval)) {
		    call un_changer (UN(sh), cmd[ival], xval, 1, NO)
		    call gt_setr (gt, GTXMIN, xval)
		}
		xval = gt_getr (gt, GTXMAX)
		if (!IS_INDEF(xval)) {
		    call un_changer (UN(sh), cmd[ival], xval, 1, NO)
		    call gt_setr (gt, GTXMAX, xval)
		}
		call un_changer (UN(sh), cmd[ival], Memr[SX(sh)], SN(sh), YES)
		call strcpy (cmd[ival], units, SZ_FNAME)
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
	case FLIP:
	    call gargb (bval)
	    if (nscan() == 2) {
		options[FLIP-OPOFF] = btoi (bval)
		call gt_seti (gt, GTXFLIP, options[FLIP-OPOFF])
	    } else {
		options[FLIP-OPOFF] = gt_geti (gt, GTXFLIP)
		call printf ("flip %b\n")
		    call pargi (options[FLIP-OPOFF])
	    }
	case OVERPLOT:
	    call gargb (bval)
	    if (nscan() == 2) {
		options[OVERPLOT-OPOFF] = btoi (bval)
	    } else {
		call printf ("overplot %b\n")
		    call pargi (options[OVERPLOT-OPOFF])
	    }
	case LABEL, MABOVE, MBELOW:
	    call gargwrd (cmd, SZ_LINE)
	    for (ival=1; IS_WHITE(cmd[ival]); ival=ival+1)
		;
	    call strcpy (cmd[ival], cmd, SZ_LINE)
	    call gargwrd (Memc[str], SZ_LINE)
	    for (ival=1; IS_WHITE(Memc[str+ival-1]); ival=ival+1)
		;
	    call strcpy (Memc[str+ival-1], Memc[str], SZ_LINE)

	    switch (ncmd) {
	    case LABEL:
		call splabel ("label", sh, gp, x, y, cmd, Memc[str])
	    case MABOVE:
		call splabel ("mabove", sh, gp, x, y, cmd, Memc[str])
	    case MBELOW:
		call splabel ("mbelow", sh, gp, x, y, cmd, Memc[str])
	    }

	default:
	    call printf ("Unrecognized or ambiguous command\007")
	}

	call sfree (sp)
end
