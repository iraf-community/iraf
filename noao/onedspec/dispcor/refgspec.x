include	<error.h>
include	"refspectra.h"

# REFOPEN   -- Set verbose and log file descriptors and open symbol table.
# REFCLOSE  -- Close file descriptors and symbol table
# REFGSPEC  -- Get a spectrum from the symbol table.  Map it only once.
# REFGINPUT -- Get input spectrum.  Apply various checks.
# REFGREF   -- Get reference spectrum.  Apply various checks.

define	REF_LEN		6		# Length of reference structure
define	REF_SORTVAL	Memd[P2D($1)]	# Sort value
define	REF_AP		Memi[$1+2]	# Aperture number
define	REF_GVAL	Memi[$1+3]	# Sort value
define	REF_SPEC1	Memi[$1+4]	# Offset for reference spectrum 1
define	REF_SPEC2	Memi[$1+5]	# Offset for reference spectrum 2


# REFOPEN  -- Set verbose and log file descriptors and open symbol table.
# The file descriptors and symbol table pointer are in common.  A null
# file descriptor indicates no output.

procedure refopen ()

bool	clgetb()
real	clgetr()
pointer	rng_open(), stopen()
int	fd, btoi(), clpopnu(), clgfil(), open(), nowhite()
errchk	open()

include	"refspectra.com"

begin
	call malloc (sort, SZ_FNAME, TY_CHAR)
	call malloc (group, SZ_FNAME, TY_CHAR)

	# Check log files
	logfiles = clpopnu ("logfiles")
	while (clgfil (logfiles, Memc[sort], SZ_FNAME) != EOF) {
	    fd = open (Memc[sort], APPEND, TEXT_FILE)
	    call close (fd)
	}
	call clprew (logfiles)

	# Get other parameters
	call clgstr ("apertures", Memc[sort], SZ_FNAME)
	iferr (aps = rng_open (Memc[sort], INDEF, INDEF, INDEF))
	    call error (0, "Bad aperture list")
	call clgstr ("refaps", Memc[sort], SZ_FNAME)
	iferr (raps = rng_open (Memc[sort], INDEF, INDEF, INDEF))
	    call error (0, "Bad reference aperture list")
	call clgstr ("sort", Memc[sort], SZ_FNAME)
	call clgstr ("group", Memc[group], SZ_FNAME)
	time = btoi (clgetb ("time"))
	timewrap = clgetr ("timewrap")
	verbose = btoi (clgetb ("verbose"))

	fd = nowhite (Memc[sort], Memc[sort], SZ_FNAME)
	fd = nowhite (Memc[group], Memc[group], SZ_FNAME)

	# Open symbol table.
	stp = stopen ("refspectra", 10, 20, 10*SZ_FNAME)
end


# REFCLOSE  -- Finish up

procedure refclose ()

include	"refspectra.com"

begin
	call mfree (sort, TY_CHAR)
	call mfree (group, TY_CHAR)
	call clpcls (logfiles)
	call stclose (stp)
	call rng_close (raps)
	call rng_close (aps)
end


# REFGSPEC  -- Get a spectrum from the symbol table.  Map it only once.
# All access to spectra is through this routine.  It returns header parameters.
# Because the spectra may be accessed in very random order and many times
# the information is stored in a symbol table keyed on the spectrum name.
# The spectrum need be mapped only once!  Any error from IMMAP is returned.

procedure refgspec (spec, ap, sortval, gval, ref1, ref2)

char	spec[ARB]	# Spectrum image name
int	ap		# Spectrum aperture number
double	sortval		# Spectrum sort value
pointer	gval		# Group string
pointer	ref1		# Reference spectrum 1
pointer	ref2		# Reference spectrum 2

pointer	sym, stfind(), stenter(), stpstr(), strefsbuf()
pointer	im, str, immap()
bool	streq()
int	imgeti(), strlen()
double	imgetd()
errchk	immap, imgetd, imgstr

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

	    # Failure to find a specified keyword is a fatal error.
	    iferr {
		if (Memc[sort] == EOS || streq (Memc[sort], "none") ||
		    select == MATCH || select == AVERAGE)
		    sortval = INDEFD
		else {
		    sortval = imgetd (im, Memc[sort])
		    if (time == YES)
			sortval = mod (sortval + 24. - timewrap, 24.0D0)
		}

		call malloc (str, SZ_FNAME, TY_CHAR)
		if (Memc[group] == EOS || streq (Memc[group], "none") ||
		    select == MATCH || select == AVERAGE)
		    Memc[str] = EOS
		else
		    call imgstr (im, Memc[group], Memc[str], SZ_FNAME)
		gval = stpstr (stp, Memc[str], strlen (Memc[str])+1)
	    } then
		call erract (EA_FATAL)

	    iferr (call imgstr (im, "refspec1", Memc[str], SZ_FNAME))
		Memc[str] = EOS
	    ref1 = stpstr (stp, Memc[str], strlen (Memc[str])+1)
	    iferr (call imgstr (im, "refspec2", Memc[str], SZ_FNAME))
		Memc[str] = EOS
	    ref2 = stpstr (stp, Memc[str], strlen (Memc[str])+1)
	    call mfree (str, TY_CHAR)

	    call imunmap (im)

	    sym = stenter (stp, spec, REF_LEN)
	    REF_AP(sym) = ap
	    REF_SORTVAL(sym) = sortval
	    REF_GVAL(sym) = gval
	    REF_SPEC1(sym) = ref1
	    REF_SPEC2(sym) = ref2
	}
	ap = REF_AP(sym)
	sortval = REF_SORTVAL(sym)
	gval = strefsbuf (stp, REF_GVAL(sym))
	ref1 = strefsbuf (stp, REF_SPEC1(sym))
	ref2 = strefsbuf (stp, REF_SPEC2(sym))
end


# REFGINPUT -- Get input spectrum.  Apply various checks.
# This calls REFGSPEC and then checks:
#	1. The spectrum is found.
#	2. The spectrum has not been assigned reference spectra previously.
#	   If it has then determine whether to override the assignment.
#	3. Check if the aperture is correct.
# Return true if the spectrum is acceptable and false if not.

bool procedure refginput (spec, ap, val, gval)

char	spec[ARB]	# Spectrum image name
int	ap		# Spectrum aperture number (returned)
double	val		# Spectrum sort value (returned)
pointer	gval		# Spectrum group value (returned)

bool	clgetb(), rng_elementi()
pointer	ref1, ref2
errchk	refgspec

include	"refspectra.com"

define	err_	99

begin
	# Get the spectrum from the symbol table.
	iferr (call refgspec (spec, ap, val, gval, ref1, ref2)) {
	    call refmsgs (NO_SPEC, spec, "", "", "", ap, 0, "")
	    goto err_
	}

	# Check if it has a previous reference spectrum.  Override if desired.
	if (Memc[ref1] != EOS) {
	    if (!clgetb ("override")) {
		call refmsgs (DEF_REFSPEC, spec, Memc[ref1], "", "", ap, 0,
		    Memc[ref2])
	       goto err_
	    } else {
		call refmsgs (OVR_REFSPEC, spec, Memc[ref1], "", "", ap, 0,
		    Memc[ref2])
	    }
	}
	
	# Check aperture numbers.
	if (aps != NULL) {
	    if (!rng_elementi (aps, ap)) {
	        call refmsgs (BAD_AP, spec, "", "", "", ap, 0, "")
	        goto err_
	    }
	}

	return (true)

err_
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

bool procedure refgref (spec, ap, val, gval)

char	spec[ARB]	# Spectrum image name
int	ap		# Spectrum aperture number (returned)
double	val		# Spectrum sort value (returned)
pointer	gval		# Spectrum group value (returned)

bool	strne(), rng_elementi()
pointer	ref1, ref2
errchk	refgspec

include	"refspectra.com"

define	err_	99

begin
	# Get spectrum from symbol table.
	iferr (call refgspec (spec, ap, val, gval, ref1, ref2)) {
	    call refmsgs (NO_REF, spec, "", "", "", ap, 0, "")
	    goto err_
	}

	# Check if spectrum is a reference spectrum.
	if (strne (spec, Memc[ref1])) {
	    call refmsgs (NOT_REFSPEC, spec, "", "", "", ap, 0, "")
	    goto err_
	}
	
	# Check aperture numbers.
	if (raps != NULL) {
	    if (!rng_elementi (raps, ap)) {
	        call refmsgs (BAD_REFAP, spec, "", "", "", ap, 0, "")
	        goto err_
	    }
	}

	return (true)

err_
	return (false)
end
