include	<pkg/inlfit.h>
include	"inlfitdef.h"

# IN_DUMP -- INLFIT debugging routine.

procedure in_dumpr (fd, in)

int	fd		# file descriptor
pointer	in		# INLFIT descriptor

int	i, npars, nfpars, nvars

begin
	# Test INLFIT pointer.
	if (in == NULL) {
	    call fprintf (fd, "\n****** in_dump: Null INLFIT pointer\n")
	    call flush (fd)
	    return
	}

	# File and INLFIT descriptors.
	call fprintf (fd, "\n****** in_dump: (fd=%d), (in=%d)\n")
	    call pargi (fd)
	    call pargi (in)
	call flush (fd)

	# Function and derivative pointers.
	call fprintf (fd, "Fitting function pointer    = %d\n")
	    call pargi (IN_FUNC (in))
	call fprintf (fd, "Derivative function pointer = %d\n")
	    call pargi (IN_DFUNC (in))
	call flush (fd)

	# Number of parameters, fitting parameters, and variables.
	npars  = IN_NPARAMS (in)
	nfpars = IN_NFPARAMS (in)
	nvars  = IN_NVARS (in)
	call fprintf (fd, "Number of parameters        = %d\n")
	    call pargi (npars)
	call fprintf (fd, "Number of fitted parameters = %d\n")
	    call pargi (nfpars)
	call fprintf (fd, "Number of variables         = %d\n")
	    call pargi (nvars)
	call fprintf (fd, "Number of points            = %d\n")
	    call pargi (IN_NPTS (in))
	call flush (fd)

	# Parameter values.
	    call fprintf (fd, "Parameter values (%d):\n")
		call pargi (npars)
	if (IN_PARAM (in) != NULL) {
	    do i = 1, npars {
		call fprintf (fd, "%d -> %g\n")
		    call pargi (i)
		    call pargr (Memr [IN_PARAM (in) + i - 1])
	    }
	} else
	    call fprintf (fd, "Null parameter value pointer\n")
	call flush (fd)

	# Parameter changes.
	if (IN_PARAM (in) != NULL) {
	    call fprintf (fd, "Parameter changes (%d):\n")
		call pargi (npars)
	    do i = 1, npars {
		call fprintf (fd, "%d -> %g\n")
		    call pargi (i)
		    call pargr (Memr [IN_DPARAM (in) + i - 1])
	    }
	} else
	    call fprintf (fd, "Null parameter change pointer\n")
	call flush (fd)

	# Parameter list.
	if (IN_PARAM (in) != NULL) {
	    call fprintf (fd, "Parameter list (%d):\n")
		call pargi (npars)
	    do i = 1, npars {
		call fprintf (fd, "%d -> %d\n")
		    call pargi (i)
		    call pargi (Memi[IN_PLIST (in) + i - 1])
	    }
	} else
	    call fprintf (fd, "Null parameter list pointer\n")
	call flush (fd)

	# Floating point parameters.
	if (IN_SFLOAT (in) != NULL) {
	    call fprintf (fd, "Fit tolerance  = %g\n")
		call pargr (IN_TOLR (in))
	    call fprintf (fd, "Low reject     = %g\n")
		call pargr (IN_LOWR (in))
	    call fprintf (fd, "High reject    = %g\n")
		call pargr (IN_HIGHR (in))
	    call fprintf (fd, "Growing radius = %g\n")
		call pargr (IN_GROWR (in))
	} else
	    call fprintf (fd, "Null floating point pointer\n")
	call flush (fd)

	# Max number of iterations, and rejection iterations.
	call fprintf (fd, "Maximum number of iterations   = %d\n")
	    call pargi (IN_MAXITER (in))
	call fprintf (fd, "Number of rejection iterations = %d\n")
	    call pargi (IN_MAXITER (in))

	# Rejected points.
	call fprintf (fd, "Number of rejected points   = %d\n")
	    call pargi (IN_NREJPTS (in))
	call fprintf (fd, "Rejected point list pointer = %d\n")
	    call pargi (IN_REJPTS (in))

	# User procedures.
	call fprintf (fd, "User axis procedure pointer  = %d\n")
	    call pargi (IN_UAXES (in))
	call fprintf (fd, "User colon procedure pointer = %d\n")
	    call pargi (IN_UCOLON (in))
	call fprintf (fd, "User fit procedure pointer   = %d\n")
	    call pargi (IN_UFIT (in))

	# Minimum variable values.
	if (IN_XMIN (in) != NULL) {
	    call fprintf (fd, "Minimum variable values (%d):\n")
		call pargi (nvars)
	    do i = 1, nvars {
		call fprintf (fd, "%d -> %g\n")
		    call pargi (i)
		    call pargr (Memr[IN_XMIN (in) + i - 1])
	    }
	} else
	    call fprintf (fd, "Null minimum value pointer\n")
	call flush (fd)

	# Maximum variable values.
	if (IN_XMAX (in) != NULL) {
	    call fprintf (fd, "Maximum variable values (%d):\n")
		call pargi (nvars)
	    do i = 1, nvars {
		call fprintf (fd, "%d -> %g\n")
		    call pargi (i)
		    call pargr (Memr[IN_XMAX (in) + i - 1])
	    }
	} else
	    call fprintf (fd, "Null maximum value pointer\n")
	call flush (fd)

	# Flags.
	call fprintf (fd, "Overplot next flag = %d\n")
	    call pargi (IN_OVERPLOT (in))
	call fprintf (fd, "Overplot fit flag  = %d\n")
	    call pargi (IN_PLOTFIT (in))
	call fprintf (fd, "Fit error code     = %d\n")
	    call pargi (IN_FITERROR (in))

	# Strings.
	if (IN_LABELS (in) != NULL) {
	    call fprintf (fd, "Axis labels         = [%s]\n")
		call pargstr (Memc[IN_LABELS (in)])
	} else
	    call fprintf (fd, "Null axis label pointer\n")
	if (IN_UNITS (in) != NULL) {
	    call fprintf (fd, "Axis units          = [%s]\n")
		call pargstr (Memc[IN_UNITS (in)])
	} else
	    call fprintf (fd, "Null axis unit pointer\n")
	if (IN_FLABELS (in) != NULL) {
	    call fprintf (fd, "Function/fit labels = [%s]\n")
		call pargstr (Memc[IN_FLABELS (in)])
	} else
	    call fprintf (fd, "Null function/fit label pointer\n")
	if (IN_FUNITS (in) != NULL) {
	    call fprintf (fd, "Function/fit units  = [%s]\n")
		call pargstr (Memc[IN_FUNITS (in)])
	} else
	    call fprintf (fd, "Null function/fit unit pointer\n")
	if (IN_PLABELS (in) != NULL) {
	    call fprintf (fd, "Parameter labels    = [%s]\n")
		call pargstr (Memc[IN_PLABELS (in)])
	} else
	    call fprintf (fd, "Null parameter label pointer\n")
	if (IN_PUNITS (in) != NULL) {
	    call fprintf (fd, "Parameter units     = [%s]\n")
		call pargstr (Memc[IN_PUNITS (in)])
	} else
	    call fprintf (fd, "Null parameter unit pointer\n")
	if (IN_VLABELS (in) != NULL) {
	    call fprintf (fd, "Variable labels     = [%s]\n")
		call pargstr (Memc[IN_VLABELS (in)])
	} else
	    call fprintf (fd, "Null variable label pointer\n")
	if (IN_VUNITS (in) != NULL) {
	    call fprintf (fd, "Variable units      = [%s]\n")
		call pargstr (Memc[IN_VUNITS (in)])
	} else
	    call fprintf (fd, "Null variable unit pointer\n")
	if (IN_USERLABELS (in) != NULL) {
	    call fprintf (fd, "User plot labels    = [%s]\n")
		call pargstr (Memc[IN_USERLABELS (in)])
	} else
	    call fprintf (fd, "Null user plot label pointer\n")
	if (IN_USERUNITS (in) != NULL) {
	    call fprintf (fd, "User plot units     = [%s]\n")
		call pargstr (Memc[IN_USERUNITS (in)])
	} else
	    call fprintf (fd, "Null user plot unit pointer\n")
	if (IN_HELP (in) != NULL) {
	    call fprintf (fd, "Help page          = [%s]\n")
		call pargstr (Memc[IN_HELP (in)])
	} else
	    call fprintf (fd, "Null help page pointer\n")
	if (IN_PROMPT (in) != NULL) {
	    call fprintf (fd, "Help prompt        = [%s]\n")
		call pargstr (Memc[IN_PROMPT (in)])
	} else
	    call fprintf (fd, "Null help prompt\n")
	call flush (fd)

	# Graph keys.
	if (IN_SGAXES (in) != NULL) {
	    call fprintf (fd, "Current graph key = %d\n")
		call pargi (IN_GKEY (in))
	    do i = 1, INLNGKEYS {
		call fprintf (fd, "%d, xtype=%d, xnum=%d, ytype=%d, ynum=%d\n")
		    call pargi (i)
		    call pargi (IN_GXTYPE   (in, i))
		    call pargi (IN_GXNUMBER (in, i))
		    call pargi (IN_GYTYPE   (in, i))
		    call pargi (IN_GYNUMBER (in, i))
	    }
	} else
	    call fprintf (fd, "Null key pointer\n")
	call flush (fd)
end
