include	<gset.h>
include	<error.h>
include	<pkg/center1d.h>
include	"identify.h"

# List of colon commands.
define	CMDS "|show|features|image|nsum|database|read|write|coordlist|match|\
	|maxfeatures|minsep|zwidth|labels|fwidth|ftype|cradius|threshold|"

define	SHOW		1	# Show parameters
define	FEATURES	2	# Show list of features
define	IMAGE		3	# Set new image
define	NSUM		4	# Set the number of lines or columns to sum
define	DATABASE	5	# Set new database
define	READ		6	# Read database entry
define	WRITE		7	# Write database entry
define	COORDLIST	8	# Set new coordinate list
define	MATCH		9	# Set coordinate list matching distance
# newline		10
define	MAXFEATURES	11	# Set maximum number of features for auto find
define	MINSEP		12	# Set minimum separation distance
define	ZWIDTH		13	# Set zoom window width
define	LABEL		14	# Set label type
define	WIDTH		15	# Set centering width
define	TYPE		16	# Set centering type
define	RADIUS		17	# Set centering radius
define	THRESHOLD	18	# Set the centering threshold

# ID_COLON -- Respond to colon command.

procedure id_colon (id, cmdstr, id_ll, newimage, prfeature)

pointer	id			# ID pointer
char	cmdstr[ARB]		# Colon command
pointer	id_ll			# Line list pointer
char	newimage[ARB]		# New image name
int	prfeature		# Print current feature on status line

char	cmd[SZ_LINE]
int	i, ncmd, ival
real	rval[2]
pointer	im

int	nscan(), strdic()
pointer	immap()
errchk	immap, id_dbread, id_dbwrite, id_log

begin
	# Scan the command string and get the first word.
	call sscan (cmdstr)
	call gargwrd (cmd, SZ_LINE)
	ncmd = strdic (cmd, cmd, SZ_LINE, CMDS)

	switch (ncmd) {
	case SHOW: # :show - show values of parameters
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call gdeactivate (ID_GP(id), AW_CLEAR)
	        call id_show (id, "STDOUT")
		call greactivate (ID_GP(id), AW_PAUSE)
	    } else {
		iferr (call id_show (id, cmd)) {
		    call erract (EA_WARN)
		    prfeature = NO
		}
	    }
	case FEATURES: # :features - list features
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call gdeactivate (ID_GP(id), AW_CLEAR)
	        call id_log (id, "STDOUT")
		call greactivate (ID_GP(id), AW_PAUSE)
	    } else {
		iferr (call id_log (id, cmd)) {
		    call erract (EA_WARN)
		    prfeature = NO
		}
	    }
	case IMAGE: # :image - set image to identify
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call printf ("image %s\n")
		    call pargstr (Memc[ID_IMAGE(id)])
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
	case NSUM: # :nsum - set number of lines or columns to sum in image
	    call gargi (ival)
	    if (nscan() == 1) {
	        call printf ("nsum %d\n")
		    call pargi (ID_NSUM(id))
		prfeature = NO
	    } else
		ID_NSUM(id) = ival
	case DATABASE: # :database - set database
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call printf ("database %s\n")
		    call pargstr (Memc[ID_DATABASE(id)])
		prfeature = NO
	    } else {
	        call strcpy (cmd, Memc[ID_DATABASE(id)], SZ_FNAME)
		ID_NEWDBENTRY(id) = YES
	    }
	case READ: # :read - read database entry
	    prfeature = NO
	    iferr {
	        call gargwrd (cmd, SZ_LINE)
	        if (nscan() == 1)
		    call id_dbread (id, Memc[ID_IMAGE(id)], YES)
	        else {
		    call xt_stripwhite (cmd)
		    if (cmd[1] == EOS)
		        call id_dbread (id, Memc[ID_IMAGE(id)], YES)
		    else
		        call id_dbread (id, cmd, YES)
		}
	    } then
		call erract (EA_WARN)
	case WRITE: # :write - write database entry
	    prfeature = NO
	    iferr {
	        call gargwrd (cmd, SZ_LINE)
	        if (nscan() == 1)
		    call id_dbwrite (id, Memc[ID_IMAGE(id)], YES)
	        else {
		    call xt_stripwhite (cmd)
		    if (cmd[1] == EOS)
		        call id_dbwrite (id, Memc[ID_IMAGE(id)], YES)
		    else
		        call id_dbwrite (id, cmd, YES)
		}
	    } then
		call erract (EA_WARN)
	case COORDLIST: # :coordlist - set coordinate list
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call printf ("coordlist %s\n")
		    call pargstr (Memc[ID_COORDLIST(id)])
		prfeature = NO
	    } else {
	        call strcpy (cmd, Memc[ID_COORDLIST(id)], SZ_FNAME)
		call id_unmapll (id_ll)
		call id_mapll (id_ll, Memc[ID_COORDLIST(id)])
	    }
	case MATCH: # :match - set matching distance for coordinate list
	    call gargr (rval[1])
	    if (nscan() == 1) {
	        call printf ("match %g\n")
		    call pargr (ID_MATCH(id))
		prfeature = NO
	    } else
		ID_MATCH(id) = rval[1]
	case MAXFEATURES: # :maxfeatures - set max num features for auto find
	    call gargi (ival)
	    if (nscan() == 1) {
	        call printf ("maxfeatures %d\n")
		    call pargi (ID_MAXFEATURES(id))
		prfeature = NO
	    } else
		ID_MAXFEATURES(id) = ival
	case MINSEP: # :minsep - set minimum feature separation allowed
	    call gargr (rval[1])
	    if (nscan() == 1) {
		call printf ("minsep %g\n")
		    call pargr (ID_MINSEP(id))
		prfeature = NO
	    } else
		ID_MINSEP(id) = rval[1]
	case ZWIDTH: # :zwidth - set zoom window width
	    call gargr (rval[1])
	    if (nscan() == 1) {
		call printf ("zwidth %g\n")
		    call pargr (ID_ZWIDTH(id))
		prfeature = NO
	    } else {
		ID_ZWIDTH(id) = rval[1]
		if (ID_GTYPE(id) == 2)
		    ID_NEWGRAPH(id) = YES
	    }
	case LABEL: # :labels - set label type
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		switch (ID_LABELS(id)) {
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
	        ID_LABELS(id) = strdic (cmd, cmd, SZ_LINE, LABELS)
		do i = 1, ID_NFEATURES(id)
		    call id_mark (id, i)
	    }
	case WIDTH: # :fwidth - set centering width
	    call gargr (rval[1])
	    if (nscan() == 1) {
	        call printf ("fwidth %g\n")
		    call pargr (ID_FWIDTH(id))
	        prfeature = NO
	    } else
		ID_FWIDTH(id) = rval[1]
	case TYPE:	# :ftype - set centering type
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		switch (ID_FTYPE(id)) {
		case EMISSION:
		    call printf ("ftype emission\n")
		case ABSORPTION:
		    call printf ("ftype absorption\n")
		}
	        prfeature = NO
	    } else
	        ID_FTYPE(id) = strdic (cmd, cmd, SZ_LINE, FTYPES)
	case RADIUS: # :cradius - set centering radius
	    call gargr (rval[1])
	    if (nscan() == 1) {
		call printf ("cradius %g\n")
		    call pargr (ID_CRADIUS(id))
	        prfeature = NO
	    } else
		ID_CRADIUS(id) = rval[1]
	case THRESHOLD: # :threshold - set centering threshold
	    call gargr (rval[1])
	    if (nscan() == 1) {
		call printf ("threshold %g\n")
		    call pargr (ID_THRESHOLD(id))
	        prfeature = NO
	    } else
		ID_THRESHOLD(id) = rval[1]
	}
end
