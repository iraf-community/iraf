include	<pkg/xtanswer.h>

# T_APTRACE -- Trace features in two dimensional images.
#
# For each image determine a set of apertures (from a database and / or
# interactively) marking the features to be traced at a starting
# dispersion point.  The feature positions are traced to other dispersion
# points.  The user specifies the step size for tracing and the number of
# lines or columns to sum before determining feature positions.  The set
# of traced feature positions for each feature are then fit by a curve.
# The traced apertures are then written back to the aperture database.

procedure t_aptrace ()

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
	fittrace = ap_answer ("fittrace", interactive)
	dbwrite = ap_answer ("apio.dbwrite", interactive)

	if (interactive == NO) {
	    edit = ALWAYSNO
	    fittrace = ALWAYSNO
	}
	sum = ALWAYSNO
	review = ALWAYSNO
	strip = ALWAYSNO

	call apextract (interactive, recenter, find, edit, trace, fittrace,
	    sum, review, strip, dbwrite)
end
