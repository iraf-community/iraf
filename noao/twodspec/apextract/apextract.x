include	<imhdr.h>
include	<error.h>
include	<pkg/xtanswer.h>
include	<pkg/gtools.h>
include	"apertures.h"

# APEXTRACT -- Main control loop for the APEXTRACT tasks.

procedure apextract (interactive, recenter, find, edit, trace, fittrace, sum,
	review, strip, dbwrite)

int	interactive		# Run interactively?
int	recenter		# Recenter reference apertures?
int	find			# Find apertures automatically?
int	edit			# Edit apertures?
int	trace			# Trace apertures?
int	fittrace		# Fit traced points interactively?
int	sum			# Extract 1D aperture sum?
int	review			# Review 1D spectra?
int	strip			# Extract 2D aperture strip?
int	dbwrite			# Write aperture data to database?

pointer	aps			# Pointer to array of aperture pointers
int	naps			# Number of apertures

int	listimages, listrefs, listsum, listsky, liststrip
int	listprofs1, listprofs2
int	i, line, nsum, trline
pointer	sp, image, reference, profiles, str, str1

int	imtopenp(), ap_getim(), clgeti()
bool	clgetb(), strne()

errchk	tr_trace, ex_sum, ex_strip, ap_find, ap_edit, ap_getaps, ap_recenter
errchk	ap_dbread

begin
	# Allocate memory for the apertures, filenames, and strings.
	# Open the image and reference lists.
	# Initialize parameters, procedures, and I/O.

	call smark (sp)
	call salloc (aps, AP_MAXAPS, TY_INT)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (reference, SZ_FNAME, TY_CHAR)
	call salloc (profiles, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (str1, SZ_LINE, TY_CHAR)

	listimages = imtopenp ("input")
	listrefs = imtopenp ("references")
	listsum = imtopenp ("apsum.output")
	listsky = imtopenp ("apsum.sky")
	listprofs1 = imtopenp ("apsum.profiles")
	listprofs2 = imtopenp ("apstrip.profiles")
	liststrip = imtopenp ("apstrip.output")

	naps = 0
	Memc[reference] = EOS
	Memc[profiles] = EOS

	line = clgeti ("apedit.line")
	nsum = clgeti ("apedit.nsum")
	trline = clgeti ("aptrace.line")

	# Initialize the trace procedure.
	call tr_init ()

	# Extract the apertures from each input image.
	while (ap_getim (listimages, Memc[image], SZ_FNAME) != EOF) {
	    if (ap_getim (listrefs, Memc[str], SZ_LINE) != EOF)
		call strcpy (Memc[str], Memc[reference], SZ_FNAME)

	    iferr {
		if (clgetb ("apio.verbose"))
		    call printf ("Searching aperture database ...\n")

		# Get the reference apertures.
		iferr (call ap_dbread (Memc[reference], Memi[aps], naps))
		    call error (0, "Reference image apertures not found")
		if ((naps != 0) && strne (Memc[reference], Memc[image])) {
		    if (recenter == YES || recenter == ALWAYSYES)
		        call ap_recenter (Memc[image], line, nsum,
			    recenter, dbwrite, Memi[aps], naps)
		    else
	                call ap_dbwrite (Memc[image], dbwrite, Memi[aps], naps)
		}

		# If no apertures then get the image apertures and
		# possible recenter them.
		if (naps == 0)
	    	    iferr (call ap_dbread (Memc[image], Memi[aps], naps))
			;

		# Find apertures.
	        call ap_find (Memc[image], line, nsum, find, dbwrite, 
		    Memi[aps], naps)

		# Edit apertures.
		call ap_edit (Memc[image], line, nsum, edit, dbwrite,
		    Memi[aps], naps)

		# Do the tracing.
		call tr_trace (Memc[image], trline, trace, fittrace,
		    dbwrite, Memi[aps], naps)

		# Do the sum extraction.
		call sprintf (Memc[str], SZ_LINE,
		    "Extract 1D aperture sums for %s?")
	            call pargstr (Memc[image])
		call xt_answer (Memc[str], sum)
		if ((sum == YES) || (sum == ALWAYSYES)) {
		    if (ap_getim (listsum, Memc[str], SZ_LINE) == EOF)
		        Memc[str] = EOS
	    	    if (ap_getim (listprofs1, Memc[str1], SZ_LINE) != EOF)
			call strcpy (Memc[str1], Memc[profiles], SZ_FNAME)
		    if (ap_getim (listsky, Memc[str1], SZ_LINE) == EOF)
		        Memc[str1] = EOS
	            call ex_sum (Memc[image], Memc[str], Memc[str1],
			Memc[profiles], review, Memi[aps], naps)
		}

		# Do the strip extraction.
		call sprintf (Memc[str], SZ_LINE,
		    "Extract 2D aperture strips for %s?")
	            call pargstr (Memc[image])
		call xt_answer (Memc[str], strip)
		if ((strip == YES) || (strip == ALWAYSYES)) {
		    if (ap_getim (liststrip, Memc[str], SZ_LINE) == EOF)
	    	        call imgcluster (Memc[image], Memc[str], SZ_LINE)
	    	    if (ap_getim (listprofs2, Memc[str1], SZ_LINE) != EOF)
			call strcpy (Memc[str1], Memc[profiles], SZ_FNAME)
	            call ex_strip (Memc[image], Memc[str], Memc[profiles],
			Memi[aps], naps)
		}

		# Make an aperture plot.
		call ap_plot (Memc[image], line, nsum, Memi[aps], naps)

		# Record last apertures.
		if ((dbwrite == YES) || (dbwrite == ALWAYSYES))
	            call ap_dbwrite ("last", ALWAYSYES, Memi[aps], naps)

		# Free memory.
		for (i = 1; i <= naps; i = i + 1)
		    call ap_free (Memi[aps+i-1])
		naps = 0
	    } then
		call erract (EA_WARN)
	}

	# Free memory and finish up.
	call ap_gclose ()
	call tr_done ()
	call imtclose (listimages)
	call imtclose (listrefs)
	call imtclose (listsum)
	call imtclose (listsky)
	call imtclose (liststrip)
	call imtclose (listprofs1)
	call imtclose (listprofs2)

	call sfree (sp)
end
