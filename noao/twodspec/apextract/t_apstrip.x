include	<pkg/xtanswer.h>

# T_APSTRIP -- Extract 2D aperture strips
#
# The apertures may specified from another image and/or edited in this
# task or the entire image extracted.  Apertures are defined with
# APDEFINE and/or TRACE and/or this task.  A weighted extractions of the
# pixels within an extraction aperture centered on the traced position is
# made on each column or line of the input images.  Multiple features in
# the same image are extracted to output images with the specified root
# name and a sequential extension.  Either the two dimensional aperture
# or the one dimensional weighted integral over the aperture may be extracted.

procedure t_apstrip ()

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

int	btoi(), ap_answer()
bool	clgetb()

begin
	interactive = btoi (clgetb ("interactive"))
	recenter = ap_answer ("recenter", interactive)
	find = ap_answer ("find", interactive)
	edit = ap_answer ("edit", interactive)
	trace = ap_answer ("trace", interactive)
	fittrace = ap_answer ("aptrace.fittrace", interactive)
	strip = ap_answer ("extract", interactive)
	dbwrite = ap_answer ("apio.dbwrite", interactive)

	if (interactive == NO) {
	    edit = ALWAYSNO
	    fittrace = ALWAYSNO
	}
	sum = ALWAYSNO
	review = ALWAYSNO

	call apextract (interactive, recenter, find, edit, trace, fittrace,
	    sum, review, strip, dbwrite)
end
