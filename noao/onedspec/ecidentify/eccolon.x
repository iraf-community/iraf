include	<gset.h>
include	<error.h>
include	<pkg/center1d.h>
include	"ecidentify.h"

# List of colon commands.
define	CMDS "|show|features|image|database|read|write|coordlist|match|\
	|maxfeatures|minsep|zwidth|labels|fwidth|ftype|cradius|threshold|"

define	SHOW		1	# Show parameters
define	FEATURES	2	# Show list of features
define	IMAGE		3	# Set new image
define	DATABASE	4	# Set new database
define	READ		5	# Read database entry
define	WRITE		6	# Write database entry
define	COORDLIST	7	# Set new coordinate list
define	MATCH		8	# Set coordinate list matching distance
# newline		9
define	MAXFEATURES	10	# Set maximum number of features for auto find
define	MINSEP		11	# Set minimum separation distance
define	ZWIDTH		12	# Set zoom window width
define	LABEL		13	# Set label type
define	WIDTH		14	# Set centering width
define	TYPE		15	# Set centering type
define	RADIUS		16	# Set centering radius
define	THRESHOLD	17	# Set the centering threshold

# EC_COLON -- Respond to colon command.

procedure ec_colon (ec, cmdstr, newimage, prfeature)

pointer	ec			# ID pointer
char	cmdstr[ARB]		# Colon command
char	newimage[ARB]		# New image name
int	prfeature		# Print current feature on status line

char	cmd[SZ_LINE]
int	i, ncmd, ival
real	rval[2]
pointer	im

int	nscan(), strdic(), ec_next()
pointer	immap()
errchk	immap, ec_dbread, ec_dbwrite, ec_log

begin
	# Scan the command string and get the first word.
	call sscan (cmdstr)
	call gargwrd (cmd, SZ_LINE)
	ncmd = strdic (cmd, cmd, SZ_LINE, CMDS)

	switch (ncmd) {
	case SHOW: # :show - show values of parameters
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call gdeactivate (EC_GP(ec), AW_CLEAR)
	        call ec_show (ec, "STDOUT")
		call greactivate (EC_GP(ec), AW_PAUSE)
	    } else {
		iferr (call ec_show (ec, cmd)) {
		    call erract (EA_WARN)
		    prfeature = NO
		}
	    }
	case FEATURES: # :features - list features
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call gdeactivate (EC_GP(ec), AW_CLEAR)
	        call ec_log (ec, "STDOUT")
		call greactivate (EC_GP(ec), AW_PAUSE)
	    } else {
		iferr (call ec_log (ec, cmd)) {
		    call erract (EA_WARN)
		    prfeature = NO
		}
	    }
	case IMAGE: # :image - set image
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call printf ("image %s\n")
		    call pargstr (Memc[EC_IMAGE(ec)])
		prfeature = NO
	    } else {
		call strcpy (cmd, newimage, SZ_FNAME)
		iferr {
		    im = immap (newimage, READ_ONLY, 0)
		    call imunmap (im)
		} then {
		    newimage[1] = EOS
		    call erract (EA_WARN)
		    prfeature = NO
		}
	    }
	case DATABASE: # :database - set database
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call printf ("database %s\n")
		    call pargstr (Memc[EC_DATABASE(ec)])
		prfeature = NO
	    } else {
	        call strcpy (cmd, Memc[EC_DATABASE(ec)], SZ_FNAME)
		EC_NEWDBENTRY(ec) = YES
	    }
	case READ: # :read - read database entry
	    prfeature = NO
	    iferr {
	        call gargwrd (cmd, SZ_LINE)
	        if (nscan() == 1)
		    call ec_dbread (ec, Memc[EC_IMAGE(ec)], YES)
	        else {
		    call xt_stripwhite (cmd)
		    if (cmd[1] == EOS)
		        call ec_dbread (ec, Memc[EC_IMAGE(ec)], YES)
		    else
		        call ec_dbread (ec, cmd, YES)
		}
		EC_CURRENT(ec) = 0
		i = ec_next (ec, EC_CURRENT(ec))
	    } then
		call erract (EA_WARN)
	case WRITE: # :write - write database entry
	    prfeature = NO
	    iferr {
	        call gargwrd (cmd, SZ_LINE)
	        if (nscan() == 1)
		    call ec_dbwrite (ec, Memc[EC_IMAGE(ec)], YES)
	        else {
		    call xt_stripwhite (cmd)
		    if (cmd[1] == EOS)
		        call ec_dbwrite (ec, Memc[EC_IMAGE(ec)], YES)
		    else
		        call ec_dbwrite (ec, cmd, YES)
		}
	    } then
		call erract (EA_WARN)
	case COORDLIST: # :coordlist - set coordinate list
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call printf ("coordlist %s\n")
		    call pargstr (Memc[EC_COORDLIST(ec)])
		prfeature = NO
	    } else {
	        call strcpy (cmd, Memc[EC_COORDLIST(ec)], SZ_FNAME)
		call ec_unmapll (ec)
		call ec_mapll (ec)
	    }
	case MATCH: # :match - set matching distance for coordinate list
	    call gargr (rval[1])
	    if (nscan() == 1) {
	        call printf ("match %g\n")
		    call pargr (EC_MATCH(ec))
		prfeature = NO
	    } else
		EC_MATCH(ec) = rval[1]
	case MAXFEATURES: # :maxfeatures - set max num features for auto find
	    call gargi (ival)
	    if (nscan() == 1) {
	        call printf ("maxfeatures %d\n")
		    call pargi (EC_MAXFEATURES(ec))
		prfeature = NO
	    } else
		EC_MAXFEATURES(ec) = ival
	case MINSEP: # :minsep - set minimum feature separation allowed
	    call gargr (rval[1])
	    if (nscan() == 1) {
		call printf ("minsep %g\n")
		    call pargr (EC_MINSEP(ec))
		prfeature = NO
	    } else
		EC_MINSEP(ec) = rval[1]
	case ZWIDTH: # :zwidth - set zoom window width
	    call gargr (rval[1])
	    if (nscan() == 1) {
		call printf ("zwidth %g\n")
		    call pargr (EC_ZWIDTH(ec))
		prfeature = NO
	    } else {
		EC_ZWIDTH(ec) = rval[1]
		if (EC_GTYPE(ec) == 2)
		    EC_NEWGRAPH(ec) = YES
	    }
	case LABEL: # :labels - set label type
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		switch (EC_LABELS(ec)) {
		case 2:
		    call printf ("labels index\n")
		case 3:
		    call printf ("labels pixel\n")
		case 4:
		    call printf ("labels user\n")
		default:
		    call printf ("labels none\n")
		}
	        prfeature = NO
	    } else {
	        EC_LABELS(ec) = strdic (cmd, cmd, SZ_LINE, LABELS)
		do i = 1, EC_NFEATURES(ec) {
		    if (APN(ec,i) == EC_AP(ec))
		        call ec_mark (ec, i)
		}
	    }
	case WIDTH: # :fwidth - set centering width
	    call gargr (rval[1])
	    if (nscan() == 1) {
	        call printf ("fwidth %g\n")
		    call pargr (EC_FWIDTH(ec))
	        prfeature = NO
	    } else
		EC_FWIDTH(ec) = rval[1]
	case TYPE:	# :ftype - set centering type
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		switch (EC_FTYPE(ec)) {
		case EMISSION:
		    call printf ("ftype emission\n")
		case ABSORPTION:
		    call printf ("ftype absorption\n")
		}
	        prfeature = NO
	    } else
	        EC_FTYPE(ec) = strdic (cmd, cmd, SZ_LINE, FTYPES)
	case RADIUS: # :cradius - set centering radius
	    call gargr (rval[1])
	    if (nscan() == 1) {
		call printf ("cradius %g\n")
		    call pargr (EC_CRADIUS(ec))
	        prfeature = NO
	    } else
		EC_CRADIUS(ec) = rval[1]
	case THRESHOLD: # :threshold - set centering threshold
	    call gargr (rval[1])
	    if (nscan() == 1) {
		call printf ("threshold %g\n")
		    call pargr (EC_THRESHOLD(ec))
	        prfeature = NO
	    } else
		EC_THRESHOLD(ec) = rval[1]
	default:
	    call printf ("Unrecognized or ambiguous command\007")
	    prfeature = NO
	}
end
