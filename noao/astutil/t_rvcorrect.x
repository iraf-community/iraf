include	<error.h>

# T_RVCORRECT -- Compute the radial velocity components of an observer.
#
# Input may be from text files, images, or CL parameters.  Output is
# to STDOUT and to CL parameters.

procedure t_rvcorrect ()

int	list			# List of files or images
int	header			# Print header?
int	input			# Print input data?
int	imupdate		# Update image headers?

int	btoi(), clpopnu(), clplen(), imtopenp(), imtlen()
bool	clgetb()
double	clgetd()

include	"rvcorrect.com"

begin
	# Location of observation.
	latitude = clgetd ("latitude")
	longitude = clgetd ("longitude")
	altitude = clgetd ("altitude")

	# Solar motion relative to desired standard of rest.
	vs = clgetd ("vsun")
	ras = clgetd ("ra_vsun")
	decs = clgetd ("dec_vsun")
	eps = clgetd ("epoch_vsun")

	# Print header and input data?
	header = btoi (clgetb ("header"))
	input = btoi (clgetb ("input"))
	imupdate = btoi (clgetb ("imupdate"))

	# Read observations from a list of files.
	list = clpopnu ("files")
	if (clplen (list) > 0) {
	    call rvc_files (list, header, input)
	    call clpcls (list)
	    return
	}

	# Read observations from a list of images.
	list = imtopenp ("images")
	if (imtlen (list) > 0) {
	    call rvc_images (list, header, input, imupdate)
	    call imtclose (list)
	    return
	}

	# Get observation from CL.
	call rvc_cl (header, input)
end


# RVC_FILES -- Compute radial velocities from a list of files.

procedure rvc_files (list, header, input)

int	list				# List of files.
int	header				# Print header?
int	input				# Print input data?

double	ra, dec, ep			# Coordinates of observation
int	year, month, day		# Date of observation
double	ut				# Time of observation
double	vobs				# Observed velocity

int	fd
char	file[SZ_FNAME]
double	hjd, vrot, vorb, vbary, vsol

int	clgfil(), open(), fscan(), nscan()

begin
	# Loop through files.
	while (clgfil (list, file, SZ_FNAME) != EOF) {
	    iferr (fd = open (file, READ_ONLY, TEXT_FILE)) {
		call erract (EA_WARN)
		next
	    }
	    while (fscan (fd) != EOF) {
		call gargi (year)
		call gargi (month)
		call gargi (day)
		call gargd (ut)
		call gargd (ra)
		call gargd (dec)
		if (nscan() != 6)
		    next
		call gargd (ep)
		if (nscan() != 7)
		    ep = INDEFD
		call gargd (vobs)
		if (nscan() != 8)
		    vobs = 0.

		# Compute the radial velocities and output the results.
		call rvcorrect (ra, dec, ep, year, month, day, ut, hjd, vrot,
		    vbary, vorb, vsol)
		call rvc_output (year, month, day, ut, ra, dec, hjd, vobs, vrot,
		    vbary, vorb, vsol, header, input)
	    }
	    call close (fd)
	}
end


# RVC_IMAGES -- Compute radial velocities from a list of images.

procedure rvc_images (list, header, input, imupdate)

int	list				# List of files.
int	header				# Print header?
int	input				# Print input data?
int	imupdate			# Update image header?

double	ra, dec, ep			# Coordinates of observation
int	year, month, day		# Date of observation
double	ut				# Time of observation
double	vobs				# Observed velocity

int	ip
char	image[SZ_FNAME], date[SZ_LINE]
double	hjd, vrot, vorb, vbary, vsol
pointer	im

int	imtgetim(), ctoi()
double	imgetd()
pointer	immap()

errchk	imgetd, imgstr
include	"rvcorrect.com"

begin
	# Loop through images.
	while (imtgetim (list, image, SZ_FNAME) != EOF) {
	    iferr (im = immap (image, READ_WRITE, 0)) {
		call erract (EA_WARN)
		next
	    }

	    iferr {
	        call imgstr (im, "date-obs", date, SZ_LINE)
	        ip = 1
	        if (ctoi (date, ip, day) == 0)
		    next
	        ip = ip + 1
	        if (ctoi (date, ip, month) == 0)
		    next
	        ip = ip + 1
	        if (ctoi (date, ip, year) == 0)
		    next

	        ut = imgetd (im, "ut")
	        ra = imgetd (im, "ra")
	        dec = imgetd (im, "dec")
	        ep = imgetd (im, "epoch")
		iferr (vobs = imgetd (im, "vobs"))
		    vobs = 0.

		# Compute the radial velocities and output the results.
	        call rvcorrect (ra, dec, ep, year, month, day, ut, hjd, vrot,
		    vbary, vorb, vsol)
	        call rvc_output (year, month, day, ut, ra, dec, hjd, vobs, vrot,
		    vbary, vorb, vsol, header, input)

		# Write the corrected velocity to the image.
		if (imupdate == YES) {
		    call imaddd (im, "hjd", hjd)
		    call imaddd (im, "vhelio", vobs+vrot+vbary+vorb)
		    call imaddd (im, "vlsr", vobs+vrot+vbary+vorb+vsol)
		    call sprintf (date, SZ_FNAME, "%6g %6g %6g %6g")
		        call pargd (vs)
		        call pargd (ras)
		        call pargd (decs)
		        call pargd (eps)
		    call imastr (im, "vsun", date)
		}

	    } then
		call erract (EA_WARN)

	    call imunmap (im)
	}
end


# RVC_CL -- Compute radial velocities from the CL parameters.

procedure rvc_cl (header, input)

int	header				# Print header?
int	input				# Print input data?

double	ra, dec, ep			# Coordinates of observation
int	year, month, day		# Date of observation
double	ut				# Time of observation
double	vobs				# Observed velocity

double	hjd, vrot, vorb, vbary, vsol

int	clgeti()
double	clgetd()

begin
	# Date of observation.
	year = clgeti ("year")
	month = clgeti ("month")
	day = clgeti ("day")
	ut = clgetd ("ut")

	# Direction of observation.
	ra = clgetd ("ra")
	dec = clgetd ("dec")
	ep = clgetd ("epoch")

	# Observed velocity.

	vobs = clgetd ("vobs")

	# Compute radial velocities and output resutls.
	call rvcorrect (ra, dec, ep, year, month, day, ut, hjd, vrot, vbary,
	    vorb, vsol)
	call rvc_output (year, month, day, ut, ra, dec, hjd, vobs, vrot, vbary,
	    vorb, vsol, header, input)

	# Record velocities in the parameter file.
	call clputd ("hjd", hjd)
	call clputd ("vhelio", vobs+vrot+vbary+vorb)
	call clputd ("vlsr", vobs+vrot+vbary+vorb+vsol)
end


# RVCORRECT -- Compute the radial velocities.

procedure rvcorrect (ra, dec, ep, year, month, day, ut, hjd, vrot, vbary,
	vorb, vsol)

double	ra, dec, ep			# Coordinates of observation
int	year, month, day		# Date of observation
double	ut				# Time of observation
double	hjd				# Helocentric Julian Day
double	vrot, vbary, vorb, vsol		# Returned velocity components

double	epoch, ra_obs, dec_obs, ra_vsun, dec_vsun, t

include	"rvcorrect.com"

begin
	# Determine epoch of observation and precess coordinates.
	call ast_date_to_epoch (year, month, day, ut, epoch)
	call ast_precess (ra, dec, ep, ra_obs, dec_obs, epoch)
	call ast_precess (ras, decs, eps, ra_vsun, dec_vsun, epoch)
	call ast_hjd (ra_obs, dec_obs, epoch, t, hjd)

	# Determine velocity components.
	call ast_vr (ra_vsun, dec_vsun, vs, ra_obs, dec_obs, vsol)
	call ast_vorbit (ra_obs, dec_obs, epoch, vorb)
	call ast_vbary (ra_obs, dec_obs, epoch, vbary)
	call ast_vrotate (ra_obs, dec_obs, epoch, latitude, longitude,
	    altitude, vrot)
end


# RVC_OUTPUT -- Output radial velocities.

procedure rvc_output (year, month, day, ut, ra, dec, hjd, vobs, vrot, vbary,
	vorb, vsol, header, input)

int	year, month, day		# Date of observation
double	ut				# Time of observation
double	ra, dec				# Coordinates of observation
double	hjd				# Helocentric Julian Day
double	vobs				# Observed radial velocity
double	vrot, vbary, vorb, vsol		# Velocity components
int	input				# Print input data?
int	header				# Print header?

begin
	# Print header.
	if (header == YES) {
	    if (input == YES) {
	        call printf ("%4s %2s %2s %8s %8s %9s %8s\n")
		    call pargstr ("##YR")
		    call pargstr ("MO")
		    call pargstr ("DY")
		    call pargstr ("   UT   ")
		    call pargstr ("   RA   ")
		    call pargstr ("   DEC   ")
		    call pargstr ("   VOBS ")
	    }
	    call printf ("%13s %8s %8s %8s   %8s %8s %8s %8s\n")
		call pargstr ("##   HJD     ")
		call pargstr ("VOBS")
		call pargstr ("VHELIO")
		call pargstr ("VLSR")
		call pargstr ("VDIURNAL")
		call pargstr ("VLUNAR")
		call pargstr ("VANNUAL")
		call pargstr ("VSOLAR")
	    header = NO
	}

	# Print input if desired.
	if (input == YES) {
	    call printf ("%4d %2d %2d %8.0h %8.0h %9.0h %8.1f\n")
		call pargi (year)
		call pargi (month)
		call pargi (day)
		call pargd (ut)
		call pargd (ra)
		call pargd (dec)
		call pargd (vobs)
	}

	# Print helocentric Julian day and velocities.
	call printf (
	  "%13.5f %8.2f %8.2f %8.2f   %8.3f %8.3f %8.3f %8.3f\n")
	    call pargd (hjd)
	    call pargd (vobs)
	    call pargd (vobs+vrot+vbary+vorb)
	    call pargd (vobs+vrot+vbary+vorb+vsol)
	    call pargd (vrot)
	    call pargd (vbary)
	    call pargd (vorb)
	    call pargd (vsol)
end
