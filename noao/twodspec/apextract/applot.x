include	<pkg/gtools.h>
include	"apertures.h"

# AP_PLOT -- Make a plot of the apertures if plot output is defined.

procedure ap_plot (image, line, nsum, aps, naps)

char	image[SZ_FNAME]		# Image to be edited
int	line			# Dispersion line
int	nsum			# Number of dispersion lines to sum

pointer	aps[AP_MAXAPS]		# Aperture pointers
int	naps			# Number of apertures

int	npts, apaxis, fd
pointer	im, imdata, title, gp, gt, gt_init()
errchk	ap_getdata, ap_popen

begin
	call ap_popen (gp, fd)
	if (gp == NULL)
	    return

	# Map the image and get the image data.
	call ap_getdata (image, line, nsum, im, imdata, npts, apaxis, title)

	gt = gt_init()
	call gt_sets (gt, GTTITLE, Memc[title])
	call gt_sets (gt, GTPARAMS, "")
	call gt_setr (gt, GTXMIN, INDEF)
	call gt_setr (gt, GTXMAX, INDEF)
	call gt_setr (gt, GTYMIN, INDEF)
	call gt_setr (gt, GTYMAX, INDEF)

	call ap_graph (gp, gt, Memr[imdata], npts, line, aps, naps)

	call mfree (imdata, TY_REAL)
	call mfree (title, TY_CHAR)
	call ap_pclose (gp, fd)
	call gt_free (gt)
	call imunmap (im)
end
