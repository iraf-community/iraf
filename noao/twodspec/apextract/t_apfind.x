include	<pkg/xtanswer.h>

# T_APFIND -- CL task to find apertures automatically.

procedure t_apfind ()

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
	dbwrite = ap_answer ("apio.dbwrite", interactive)

	if (interactive == NO)
	    edit = ALWAYSNO
	trace = ALWAYSNO
	fittrace = ALWAYSNO
	sum = ALWAYSNO
	review = ALWAYSNO
	strip = ALWAYSNO

	call apextract (interactive, recenter, find, edit, trace, fittrace,
	    sum, review, strip, dbwrite)
end
