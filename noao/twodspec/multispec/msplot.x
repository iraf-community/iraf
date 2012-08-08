include	<imhdr.h>
include "ms.h"

# MSPLOT -- Plot image and model values.
#
# The output list format is column, image line, data value, model value.
# This task differs from t_new_image primarily in that there is no profile
# interpolation.  The model is evaluated only at the sample lines.  It
# is used to check the results of the model fitting tasks.

procedure msplot ()

char	image[SZ_FNAME]			# Image
int	line				# Image line to plot
int	naverage			# Number of image lines to average
real	lower				# Lower limit of profile model
real	upper				# Upper limit of profile model

int	sample
pointer	ms, im
pointer	sp, data, model

int	clgeti(), get_sample_line
real	clgetr()
pointer	msmap(), immap()

begin
	# Get the task parameters.

	call clgstr ("image", image, SZ_FNAME)
	line = clgeti ("line")
	naverage = clgeti ("naverage")
	lower = clgetr ("lower")
	upper = clgetr ("upper")

	# Access the database and image.

	ms = msmap (image, READ_ONLY, 0)
	im = immap (image, READ_ONLY, 0)

	# Allocate memory for the data and model.

	call smark (sp)
	call salloc (data, IM_LEN(im, 1), TY_REAL)
	call salloc (model, IM_LEN(im, 1), TY_REAL)

	sample = get_sample_line (ms, line)
	line = LINE(ms, sample)
	call msgimage (im, line, naverage, Memr[data])
	call gauss5_model (ms, sample, lower, upper, Memr[model])

	call ms_graph (Memr[data], Memr[model], IM_LEN(im, 1))

	call sfree (sp)
	call msunmap (ms)
	call imunmap (im)
end


include	<gset.h>

# MS_GRAPH -- For the selected line get the data line and compute a model line.
# Graph the data and model values.

procedure ms_graph (data, model, npts)

real	data[npts]	# Image data
real	model[npts]	# Model data
int	npts		# Number of data points

char	str[SZ_LINE]
real	x1, x2
pointer	gp, gt

real	wx, wy			# Cursor position
int	wcs, key		# WCS and cursor key

int	gt_gcur()
pointer	gopen(), gt_init()

begin
	call clgstr ("graphics", str, SZ_LINE)
	gp = gopen (str, NEW_FILE, STDGRAPH)
	gt = gt_init ()

	x1 = 1
	x2 = npts
	call gswind (gp, x1, x2, INDEF, INDEF)
	call gascale (gp, data, npts, 2)
	call grscale (gp, model, npts, 2)
	call gt_swind (gp, gt)
	call gt_labax (gp, gt)

	call gseti (gp, G_PLTYPE, 1)
	call gvline (gp, data, npts, x1, x2)
	call gseti (gp, G_PLTYPE, 2)
	call gvline (gp, model, npts, x1, x2)

	while (gt_gcur ("cursor", wx, wy, wcs, key, str, SZ_LINE) != EOF)
	    ;

	call gclose (gp)
	call gt_free (gt)
end
