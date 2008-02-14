# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>
include	"mwcs.h"

# MW_SWTYPE -- Set the coordinate (WCS function) type and any related
# attributes for an axis, or set of related axes, of a WCS.  Each call
# defines a group of one or more axes which share the same WCS function
# and which are dependent, i.e., all axes are required to evaluate the
# coordinate of any axis in the group.  Independent axes or groups of
# axes should be defined in separate calls.
#
# Although the attributes for each axis in the group are all entered in
# a single call via the WATTR string, each attribute is still assigned
# to a single axis.  The syntax is as follows:
#
#	axis 1: format="...", label="..."
#	axis 2: ...(etc.)
#
# where the axis number is relative to the start of the group.
# The WATTR string may be any length and may contain multiple lines of text.
# A typical use of attributes is to define WCS specific parameters; these
# may be read by the initialization routine in the WCS function driver,
# called when a coordinate transformation is compiled.

procedure mw_swtype (mw, axis, naxes, wtype, wattr)

pointer	mw			#I pointer to MWCS descriptor
int	axis[naxes]		#I axis number, 1:ndim
int	naxes			#I number of axes in function group
char	wtype[ARB]		#I axis coordinate type
char	wattr[ARB]		#I axis attributes, "attr=value, ..."

pointer	sp, atname, valstr, wp, op, wf
int	ip, ch, fn, wfno, ax, sz_valstr, i
int	ctowrd(), mw_flookup(), ctoi(), strlen()
errchk	syserrs, mw_swattrs, mw_flookup
bool	streq()

begin
	call smark (sp)
	sz_valstr = strlen (wattr)
	call salloc (valstr, sz_valstr, TY_CHAR)
	call salloc (atname, SZ_ATNAME, TY_CHAR)

	# Get the current WCS.
	wp = MI_WCS(mw)
	if (wp == NULL)
	    call syserrs (SYS_MWNOWCS, "mw_swtype")

	# Set the function type?
	if (wtype[1] != EOS) {
	    # Determine the function type for this axis group.
	    fn = mw_flookup (mw, wtype)
	    if (fn == ERR)
		call syserrs (SYS_MWUNKFN, wtype)

	    # For anything except a simple linear relation, add a new function
	    # descriptor to the WCS.

	    if (fn != F_LINEAR) {
		# Allocate new WCS function descriptor.
		wfno = WCS_NFUNC(wp) + 1
		if (wfno > MAX_FUNC)
		    call syserrs (SYS_MWFUNCOVFL, wtype)
		WCS_NFUNC(wp) = wfno

		# Initialize the descriptor. 
		wf = WCS_FUNC(wp,wfno)
		WF_FN(wf) = fn
		WF_NAXES(wf) = naxes
		call amovi (axis, WF_AXIS(wf,1), naxes)
	    } else
		wfno = 0

	    # Set the axis type and class.
	    do i = 1, naxes {
		call mw_swattrs (mw, axis[i], "wtype", wtype)
		WCS_AXCLASS(wp,axis[i]) = wfno
	    }
	}

	# Process the attributes into the WCS descriptor.
	ax = axis[1]
	for (ip=1;  wattr[ip] != EOS;  ) {
	    # Skip to next token.
	    ch = wattr[ip]
	    while (IS_WHITE(ch) || ch == ',' || ch == '\n' || ch == ':') {
		ip = ip + 1
		ch = wattr[ip]
	    }

	    # Done?
	    if (ch == EOS)
		break

	    # Extract attribute name string.
	    op = atname
	    ch = wattr[ip]
	    while (IS_ALNUM(ch) || ch == '_' || ch == '$') {
		Memc[op] = ch
		op = min (atname+SZ_ATNAME, op + 1)
		ip = ip + 1
		ch = wattr[ip]
	    }
	    Memc[op] = EOS

	    # Check for "axis N:" and set AX if encountered.
	    if (streq (Memc[atname], "axis"))
		if (ctoi (wattr, ip, i) > 0) {
		    ax = axis[i]
		    next
		}

	    # Skip to value string.
	    ch = wattr[ip]
	    while (IS_WHITE(ch) || ch == '=' || ch == '\n') {
		ip = ip + 1
		ch = wattr[ip]
	    }

	    # Extract value string.
	    ch = ctowrd (wattr, ip, Memc[valstr], sz_valstr)

	    # Add the attribute to the WCS.
	    call mw_swattrs (mw, ax, Memc[atname], Memc[valstr])
	}

	call sfree (sp)
end
