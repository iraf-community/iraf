include	<error.h>
include	"refspectra.h"

# REFOPEN   -- Set verbose and log file descriptors and open symbol table.
# REFCLOSE  -- Close file descriptors and symbol table
# REFGSPEC  -- Get a spectrum from the symbol table.  Map it only once.
# REFGINPUT -- Get input spectrum.  Apply various checks.
# REFGREF   -- Get reference spectrum.  Apply various checks.

define	REF_LEN		4		# Length of reference structure
define	REF_AP		Memi[$1]	# Aperture number
define	REF_SORTVAL	Memr[$1+1]	# Sort value
define	REF_SPEC1	Memi[$1+2]	# Offset for reference spectrum 1
define	REF_SPEC2	Memi[$1+3]	# Offset for reference spectrum 2


# REFOPEN  -- Set verbose and log file descriptors and open symbol table.
# The file descriptors and symbol table pointer are in common.  A null
# file descriptor indicates no output.

procedure refopen ()

bool	clgetb()
pointer	stopen()
int	fd, btoi(), clpopnu(), clgfil(), open()
pointer	sp, logfile
errchk	open()

include	"refspectra.com"

begin
	# Set log files and verbose output.
	call smark (sp)
	call salloc (logfile, SZ_FNAME, TY_CHAR)

	verbose = btoi (clgetb ("verbose"))
	logfiles = clpopnu ("logfiles")
	while (clgfil (logfiles, Memc[logfile], SZ_FNAME) != EOF) {
	    fd = open (Memc[logfile], APPEND, TEXT_FILE)
	    call close (fd)
	}
	call clprew (logfiles)

	# Open symbol table.
	stp = stopen ("refspectra", 10, 20, 10*SZ_FNAME)
end


# REFCLOSE  -- Close file descriptors and symbol table.

procedure refclose ()

include	"refspectra.com"

begin
	call clpcls (logfiles)
	call stclose (stp)
end


# REFGSPEC  -- Get a spectrum from the symbol table.  Map it only once.
# All access to spectra is through this routine.  It returns header parameters.
# Because the spectra may be accessed in very random order and many times
# the information is stored in a symbol table keyed on the spectrum name.
# The spectrum need be mapped only once!  Any error from IMMAP is returned.

procedure refgspec (spec, sort, time, timewrap, ap, sortval, ref1, ref2)

char	spec[ARB]	# Spectrum image name
char	sort[ARB]	# Sort value key
bool	time		# Time parameter?
real	timewrap	# Time wrap
int	ap		# Spectrum aperture number
real	sortval		# Spectrum sort value
char	ref1[SZ_FNAME]	# Reference spectrum 1
char	ref2[SZ_FNAME]	# Reference spectrum 2

pointer	sym, stfind(), stenter(), stpstr(), strefsbuf()
pointer	im, immap()
int	imgeti(), strlen()
real	imgetr()
errchk	immap, imgetr

include	"refspectra.com"

begin
	# Check if spectrum is in the symbol table from a previous call.
	# If not in the symbol table map the image, get the header parameters,
	# and store them in the symbol table.

	sym = stfind (stp, spec)
	if (sym == NULL) {
	    im = immap (spec, READ_ONLY, 0)
	    iferr (ap = imgeti (im, "BEAM-NUM"))
		ap = 1
	    iferr {
		sortval = imgetr (im, sort)
		if (time)
		    sortval = mod (sortval + 24. - timewrap, 24.)
	    } then
		sortval = INDEFR
	    iferr (call imgstr (im, "refspec1", ref1, SZ_FNAME))
		ref1[1] = EOS
	    iferr (call imgstr (im, "refspec2", ref2, SZ_FNAME))
		ref2[1] = EOS
	    call imunmap (im)
	    sym = stenter (stp, spec, REF_LEN)
	    REF_AP(sym) = ap
	    REF_SORTVAL(sym) = sortval
	    REF_SPEC1(sym) = stpstr (stp, ref1, strlen (ref1)+1)
	    REF_SPEC2(sym) = stpstr (stp, ref2, strlen (ref2)+1)
	} else {
	    ap = REF_AP(sym)
	    sortval = REF_SORTVAL(sym)
	    call strcpy (Memc[strefsbuf (stp, REF_SPEC1(sym))], ref1, SZ_FNAME)
	    call strcpy (Memc[strefsbuf (stp, REF_SPEC2(sym))], ref2, SZ_FNAME)
	}
end


# REFGINPUT -- Get input spectrum.  Apply various checks.
# This calls REFGSPEC and then checks:
#	1. The spectrum is found.
#	2. The spectrum has not been assigned reference spectra previously.
#	   If it has then determine whether to override the assignment.
#	3. Check if the aperture is correct.
# Return true if the spectrum is acceptable and false if not.

bool procedure refginput (spec, aps, sort, time, timewrap, ap, val)

char	spec[ARB]	# Spectrum image name
int	aps		# Aperture list
char	sort[ARB]	# Sort value key
bool	time		# Time parameter?
real	timewrap	# Time wrap
int	ap		# Spectrum aperture number (returned)
real	val		# Spectrum sort value (returned)

bool	clgetb(), is_in_range()
pointer	sp, ref1, ref2
errchk	refgspec

define	err_	99

begin
	call smark (sp)
	call salloc (ref1, SZ_FNAME, TY_CHAR)
	call salloc (ref2, SZ_FNAME, TY_CHAR)

	# Get the spectrum from the symbol table.
	iferr (call refgspec (spec, sort, time, timewrap, ap, val,
	    Memc[ref1], Memc[ref2])) {
	    call refmsgs (NO_SPEC, spec, ap, "", "")
	    goto err_
	}

	# Check if it has a previous reference spectrum.  Override if desired.
	if (Memc[ref1] != EOS) {
	    if (!clgetb ("override")) {
		call refmsgs (DEF_REFSPEC, spec, ap, Memc[ref1], Memc[ref2])
	       goto err_
	    } else {
		call refmsgs (OVR_REFSPEC, spec, ap, Memc[ref1], Memc[ref2])
	    }
	}
	
	# Check aperture numbers.
	if (aps != NULL) {
	    if (!is_in_range (Memi[aps], ap)) {
	        call refmsgs (BAD_AP, spec, ap, "", "")
	        goto err_
	    }
	}

	call sfree (sp)
	return (true)

err_
	call sfree (sp)
	return (false)
end


# REFGREF   -- Get reference spectrum.  Apply various checks.
# This calls REFGSPEC and then checks:
#	1. The spectrum is found.
#	2. The spectrum is a reference spectrum, i.e. has an IDENTIFY
#	   record.  This is signaled by having a reference equivalent to
#	   itself.
#	3. Check if the aperture is correct.
# Return true if the spectrum is acceptable and false if not.

bool procedure refgref (spec, aps, sort, time, timewrap, ap, val)

char	spec[ARB]	# Spectrum image name
pointer	aps		# Aperture list
char	sort[ARB]	# Sort value key
bool	time		# Time parameter?
real	timewrap	# Time wrap
int	ap		# Spectrum aperture number (returned)
real	val		# Spectrum sort value (returned)

bool	strne(), is_in_range()
pointer	sp, ref1, ref2
errchk	refgspec

define	err_	99

begin
	call smark (sp)
	call salloc (ref1, SZ_FNAME, TY_CHAR)
	call salloc (ref2, SZ_FNAME, TY_CHAR)

	# Get spectrum from symbol table.
	iferr (call refgspec (spec, sort, time, timewrap, ap, val,
	    Memc[ref1], Memc[ref2])) {
	    call refmsgs (NO_REF, spec, ap, "", "")
	    goto err_
	}

	# Check if spectrum is a reference spectrum.
	if (strne (spec, Memc[ref1])) {
	    call refmsgs (NOT_REFSPEC, spec, ap, "", "")
	    goto err_
	}
	
	# Check aperture numbers.
	if (aps != NULL) {
	    if (!is_in_range (Memi[aps], ap)) {
	        call refmsgs (BAD_REFAP, spec, ap, "", "")
	        goto err_
	    }
	}

	call sfree (sp)
	return (true)

err_
	call sfree (sp)
	return (false)
end
