include	<math/curfit.h>
include	<pkg/dttext.h>
include	<pkg/xtanswer.h>
include	"apertures.h"

# AP_DBWRITE -- Write aperture data to the database.  The database is obtained
# with a CL query.

procedure ap_dbwrite (image, query, aps, naps)

char	image[ARB]		# Image
int	query			# Write query?
pointer	aps[AP_MAXAPS]		# Apertures
int	naps

int	i, j, ncoeffs
pointer	sp, database, str, dt, coeffs, ap

int	cvstati(), ic_geti()
real	ic_getr()
bool	strne()
pointer	dtmap1()

errchk	dtmap1

begin
	# Set the aperture database file name and map as a NEW_FILE.
	# The file name is "ap" appended with the image name with the
	# special image section characters replaced by '_'.
	# The reason for making image sections separate database
	# files rather than combining all database entries for an image
	# in one file is that then previous entries can be deleted
	# by using NEW_FILE mode which deletes any existing database
	# file before writing out the new apertures.

	call smark (sp)
	call salloc (database, SZ_FNAME, TY_CHAR)
	call clgstr ("apio.database", Memc[database], SZ_FNAME)

	if ((Memc[database] == EOS) || (image[1] == EOS)) {
	    call sfree (sp)
	    return
	}

	# Query the user.
	call salloc (str, SZ_LINE, TY_CHAR)
	call sprintf (Memc[str], SZ_LINE, "Write apertures for %s to %s")
	    call pargstr (image)
	    call pargstr (Memc[database])
	call xt_answer (Memc[str], query)
	if ((query == NO) || (query == ALWAYSNO)) {
	    call sfree (sp)
	    return
	}

	# Map the database file name replacing special characters with '_'.
	call sprintf (Memc[str], SZ_LINE, "ap%s")
	    call pargstr (image)
	for (i=str; Memc[i] != EOS; i = i + 1)
	    switch (Memc[i]) {
	    case '[', ':', ',',']','*',' ', '/':
		Memc[i] = '_'
	    }
	dt = dtmap1 (Memc[database], Memc[str], NEW_FILE)

	# Write aperture entries for all apertures.
	for (j = 1; j <= naps; j = j + 1) {
	    ap = aps[j]

	    call dtptime (dt)
	    call dtput (dt, "begin\taperture %s %d %g %g\n")
	        call pargstr (image)
		call pargi (AP_ID(ap))
	        call pargr (AP_CEN(ap, 1))
	        call pargr (AP_CEN(ap, 2))
	    call dtput (dt, "\timage\t%s\n")
	        call pargstr (image)
	    call dtput (dt, "\taperture\t%d\n")
	        call pargi (AP_ID(ap))
	    call dtput (dt, "\tbeam\t%d\n")
	        call pargi (AP_BEAM(ap))
	    call dtput (dt, "\tcenter\t%g %g\n")
	        call pargr (AP_CEN(ap, 1))
	        call pargr (AP_CEN(ap, 2))
	    call dtput (dt, "\tlow\t%g %g\n")
	        call pargr (AP_LOW(ap, 1))
	        call pargr (AP_LOW(ap, 2))
	    call dtput (dt, "\thigh\t%g %g\n")
	        call pargr (AP_HIGH(ap, 1))
	        call pargr (AP_HIGH(ap, 2))
	    if (AP_IC(ap) != NULL) {
		call dtput (dt, "\tbackground\n")
		call dtput (dt, "\t\txmin %g\n")
		    call pargr (ic_getr (AP_IC(ap), "xmin"))
		call dtput (dt, "\t\txmax %g\n")
		    call pargr (ic_getr (AP_IC(ap), "xmax"))
		call dtput (dt, "\t\tfunction %s\n")
		    call ic_gstr (AP_IC(ap), "function", Memc[str], SZ_LINE)
		    call pargstr (Memc[str])
		call dtput (dt, "\t\torder %d\n")
		    call pargi (ic_geti (AP_IC(ap), "order"))
		call dtput (dt, "\t\tsample %s\n")
		    call ic_gstr (AP_IC(ap), "sample", Memc[str], SZ_LINE)
		    call pargstr (Memc[str])
		call dtput (dt, "\t\tnaverage %d\n")
		    call  pargi (ic_geti (AP_IC(ap), "naverage"))
		call dtput (dt, "\t\tniterate %d\n")
		    call  pargi (ic_geti (AP_IC(ap), "niterate"))
		call dtput (dt, "\t\tlow_reject %g\n")
		    call  pargr (ic_getr (AP_IC(ap), "low"))
		call dtput (dt, "\t\thigh_reject %g\n")
		    call  pargr (ic_getr (AP_IC(ap), "high"))
		call dtput (dt, "\t\tgrow %g\n")
		    call  pargr (ic_getr (AP_IC(ap), "grow"))
	    }

	    # Write out the curve.
	    call dtput (dt, "\taxis\t%d\n")
	        call pargi (AP_AXIS(ap))
	    ncoeffs = cvstati (AP_CV(ap), CVNSAVE)
	    call malloc (coeffs, ncoeffs, TY_REAL)
	    call cvsave (AP_CV(ap), Memr[coeffs])
	    call dtput (dt, "\tcurve\t%d\n")
	        call pargi (ncoeffs)
	    do i = 1, ncoeffs {
	        call dtput (dt, "\t\t%g\n")
		    call pargr (Memr[coeffs+i-1])
	    }
	    call mfree (coeffs, TY_REAL)

	    call dtput (dt, "\n")
	}
	call dtunmap (dt)

	# Log the write operation unless the output file is "last".
	if (strne (image, "last")) {
	    call sprintf (Memc[str], SZ_LINE,
		"APWRITE - %d apertures for %s written to %s.")
	        call pargi (naps)
	        call pargstr (image)
	        call pargstr (Memc[database])
	    call ap_log (Memc[str])
	}

	call sfree (sp)
end


# AP_DBREAD - Get aperture information from the database.
# If no apertures are found then the input apertures are unchanged.
# The database is obtained with a CL query.

procedure ap_dbread (image, aps, naps)

char	image[ARB]		# Image
pointer	aps[AP_MAXAPS]		# Apertures
int	naps			# Number of apertures

int	i, j, n, ncoeffs
pointer	sp, database, str, ap, dt, coeffs

bool	strne()
int	dtgeti()
real	dtgetr()
pointer	dtmap1()

errchk	dtmap1

begin
	# Return if the database or image are undefined.
	call smark (sp)
	call salloc (database, SZ_FNAME, TY_CHAR)
	call clgstr ("apio.database", Memc[database], SZ_FNAME)

	if ((Memc[database] == EOS) || (image[1] == EOS)) {
	    call sfree (sp)
	    return
	}

	# Set the aperture database file name and map it.
	# The file name is "ap" appended with the image name with the
	# special image section characters replaced by '_'.
	call salloc (str, SZ_LINE, TY_CHAR)
	call sprintf (Memc[str], SZ_LINE, "ap%s")
	    call pargstr (image)
	for (i=str; Memc[i] != EOS; i = i + 1)
	    switch (Memc[i]) {
	    case '[', ':', ',',']','*',' ', '/':
		Memc[i] = '_'
	    }

	# If an error occurs return the error.
	dt = dtmap1 (Memc[database], Memc[str], READ_ONLY)

	# Read through the database finding records matching the input image.
	n = naps
	naps = 0
	do i = 1, DT_NRECS(dt) {
	    if (naps >= AP_MAXAPS)
		call error (0, "Too many apertures")

	    call dtgstr (dt, i, "image", Memc[str], SZ_LINE)
	    if (strne (Memc[str], image))
		next

	    # If an aperture is found delete any input apertures.
	    if (naps == 0)
		for (j = 1; j <= n; j = j + 1)
		    call ap_free (aps[j])

	    call ap_alloc (ap)
	    AP_ID(ap) = dtgeti (dt, i, "aperture")
	    iferr (AP_BEAM(ap) = dtgeti (dt, i, "beam"))
		AP_BEAM(ap) = AP_ID(ap)
	    call dtgstr (dt, i, "center", Memc[str], SZ_LINE)
	    call sscan (Memc[str])
	    call gargr (AP_CEN(ap, 1))
	    call gargr (AP_CEN(ap, 2))
	    call dtgstr (dt, i, "low", Memc[str], SZ_LINE)
	    call sscan (Memc[str])
	    call gargr (AP_LOW(ap, 1))
	    call gargr (AP_LOW(ap, 2))
	    call dtgstr (dt, i, "high", Memc[str], SZ_LINE)
	    call sscan (Memc[str])
	    call gargr (AP_HIGH(ap, 1))
	    call gargr (AP_HIGH(ap, 2))
	    ifnoerr (call dtgstr (dt, i, "background", Memc[str], SZ_LINE)) {
		call ic_open (AP_IC(ap))
		call ic_putr (AP_IC(ap), "xmin", dtgetr (dt, i, "xmin"))
		call ic_putr (AP_IC(ap), "xmax", dtgetr (dt, i, "xmax"))
		call dtgstr (dt, i, "function", Memc[str], SZ_LINE)
		call ic_pstr (AP_IC(ap), "function", Memc[str])
		call ic_puti (AP_IC(ap), "order", dtgeti (dt, i, "order"))
		call dtgstr (dt, i, "sample", Memc[str], SZ_LINE)
		call ic_pstr (AP_IC(ap), "sample", Memc[str])
		call ic_puti (AP_IC(ap), "naverage", dtgeti (dt, i, "naverage"))
		call ic_puti (AP_IC(ap), "niterate", dtgeti (dt, i, "niterate"))
		call ic_putr (AP_IC(ap), "low", dtgetr (dt, i, "low_reject"))
		call ic_putr (AP_IC(ap), "high", dtgetr (dt, i, "high_reject"))
		call ic_putr (AP_IC(ap), "grow", dtgetr (dt, i, "grow"))
	    }

	    AP_AXIS(ap) = dtgeti (dt, i, "axis")
	    ncoeffs = dtgeti (dt, i, "curve")
	    call malloc (coeffs, ncoeffs, TY_REAL)
	    call dtgar (dt, i, "curve", Memr[coeffs], ncoeffs, ncoeffs)
	    call cvrestore (AP_CV(ap), Memr[coeffs])
	    call mfree (coeffs, TY_REAL)

	    naps = naps + 1
	    aps[naps] = ap
	}
	call dtunmap (dt)

	# Log the read operation.
	call sprintf (Memc[str], SZ_LINE,
	    "APREAD  - %d apertures read for %s from %s.")
	    call pargi (naps)
	    call pargstr (image)
	    call pargstr (Memc[database])
	call ap_log (Memc[str])

	# If no apertures were found then reset the number to the input value.
	if (naps == 0)
	    naps = n

	call sfree (sp)
end
