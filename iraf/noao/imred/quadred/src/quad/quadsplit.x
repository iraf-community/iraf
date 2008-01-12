include <imhdr.h>
include "quadgeom.h"

procedure t_quadsplit ()

char	input[SZ_FNAME]		#TI Input image name.
char	output[SZ_FNAME]	#TI Output image root name.
char	instrument[SZ_FNAME]	#TI Instrument translation file.
bool	clobber			#TI Clobber any existing sub-images.
int     xtrim1         	#TI Number of pixels to trim at right.
int     xtrim2         	#TI Number of pixels to trim at left.
int     ytrim1         	#TI Number of pixels to trim at bottom.
int     ytrim2         	#TI Number of pixels to trim at top.
int     xskip1         	#TI Number of pixels to skip at start of overscan in X
int     xskip2         	#TI Number of pixels to skip at end   of overscan in X

pointer	in, qg, out[QG_MAXAMPS]
int	amp, namps
char	logstr[SZ_LINE]

pointer	immap()
bool	streq(), clgetb()
int	quadmap(), hdmaccf()

begin

	# Open instrument file
	call clgstr    ("instrument",  instrument,  SZ_FNAME)
	call hdmopen   (instrument)

	# Map input image
	call clgstr ("input",  input,  SZ_FNAME)
	in = immap  (input, READ_ONLY, 0)

	# Get root name for output image
	call clgstr  ("output", output, SZ_FNAME)
	if (streq (output, ""))
	    call strcpy (input, output, SZ_FNAME)
	call xt_imroot (output, output, SZ_FNAME)

	# Set-up section translation
	call quadalloc (qg)

	if (hdmaccf (in, "HDR_REV") == NO) {
	    call quadgeom  (in, qg, "", "")
	} else {
	    call qghdr2 (in, qg)
	}

	# Adjust quadgeom structure for user trim and overscan margins
	#xtrim1 = clgeti ("xtrim1")
	#xtrim2 = clgeti ("xtrim2")
	#ytrim1 = clgeti ("ytrim1")
	#ytrim2 = clgeti ("ytrim2")
	#xskip1 = clgeti ("xskip1")
	#xskip2 = clgeti ("xskip2")
	xtrim1 = INDEFI
	xtrim2 = INDEFI
	ytrim1 = INDEFI
	ytrim2 = INDEFI
	xskip1 = INDEFI
	xskip2 = INDEFI
	call qguser (qg, xtrim1, xtrim2, ytrim1, ytrim2, xskip1, xskip2)

#	call quaddump  (qg)

	# Map output images one for each readout
	clobber = clgetb ("clobber")
	namps = quadmap (output, NEW_COPY, clobber, in, qg, out)

	# Split the image using the appropriately typed routine
        switch (IM_PIXTYPE(in)) {
        case TY_USHORT, TY_SHORT:
	    call qsplits (in, out, qg)

        case TY_LONG:
	    call qsplitl (in, out, qg)

	case TY_INT:
	    call qspliti (in, out, qg)

        case TY_REAL:
	    call qsplitr (in, out, qg)

        case TY_DOUBLE:
	    call qsplitd (in, out, qg)

        default:
            call error (1, "unsupported pixel datatype")
        }

	# Log opperation
	if (QG_NAMPSX(qg) == 2 && QG_NAMPSY(qg) == 2) {
	    call sprintf (logstr, SZ_LINE, "Quad-readout image")
	} else if (QG_NAMPSX(qg) == 2 || QG_NAMPSY(qg) == 2) {
	    call sprintf (logstr, SZ_LINE,
	    "Dual-readout image: nampsx=%d nampsy=%d")
		call pargi (QG_NAMPSX(qg))
		call pargi (QG_NAMPSY(qg))
	} else {
	    call sprintf (logstr, SZ_LINE, "Single-readout image")
	}
	call timelog (logstr, SZ_LINE)
	call ccdlog (input, logstr)

	# Tidy up
	call imunmap (in)
	do amp = 1, namps {
	    if (out[amp] != NULL) {
		call imunmap (out[amp])
	    }
	}
	call quadfree (qg)
	call hdmclose ()
end
