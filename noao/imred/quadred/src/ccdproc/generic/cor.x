include	"ccdred.h"


.help cor Feb87 noao.imred.ccdred
.nf ----------------------------------------------------------------------------
cor -- Process CCD image lines

These procedures are the heart of the CCD processing.  They do the desired
set of processing operations on the image line data as efficiently as
possible.  They are called by the PROC procedures.  There are four procedures
one for each readout axis and one for short and real image data.
Some sets of operations are coded as single compound operations for efficiency.
To keep the number of combinations managable only the most common
combinations are coded as compound operations.  The combinations
consist of any set of line overscan, column overscan, zero level, dark
count, and flat field and any set of illumination and fringe
correction.  The corrections are applied in place to the output vector.

The column readout procedure is more complicated in order to handle
zero level and flat field corrections specified as one dimensional
readout corrections instead of two dimensional calibration images.
Column readout format is probably extremely rare and the 1D readout
corrections are used only for special types of data.
.ih
SEE ALSO
proc, ccdred.h
.endhelp -----------------------------------------------------------------------


# COR1 -- Correct image lines with readout axis 1 (lines).

procedure cor1s (cors, out, overscan, zero, dark, flat, illum,
	fringe, n, darkscale, flatscale, illumscale, frgscale)

int	cors[ARB]	# Correction flags
short	out[n]		# Output data
real	overscan	# Overscan value
short	zero[n]		# Zero level correction
short	dark[n]		# Dark count correction
short	flat[n]		# Flat field correction
short	illum[n]	# Illumination correction
short	fringe[n]	# Fringe correction
int	n		# Number of pixels
real	darkscale	# Dark count scale factor
real	flatscale	# Flat field scale factor
real	illumscale	# Illumination scale factor
real	frgscale	# Fringe scale factor

int	i, op

begin
	op = cors[OVERSCAN] + cors[ZEROCOR] + cors[DARKCOR] + cors[FLATCOR]
	switch (op) {
	case O: # overscan
	    do i = 1, n
	        out[i] = out[i] - overscan
	case Z: # zero level
	    do i = 1, n
		out[i] = out[i] - zero[i]

	case ZO: # zero level + overscan
	    do i = 1, n
		out[i] = out[i] - overscan - zero[i]

	case D: # dark count
	    do i = 1, n
		out[i] = out[i] - darkscale * dark[i]
	case DO: # dark count + overscan
	    do i = 1, n
		out[i] = out[i] - overscan - darkscale * dark[i]
	case DZ: # dark count + zero level
	    do i = 1, n
		out[i] = out[i] - zero[i] - darkscale * dark[i]
	case DZO: # dark count + zero level + overscan
	    do i = 1, n
		out[i] = out[i] - overscan - zero[i] - darkscale * dark[i]

	case F: # flat field
	    do i = 1, n
		out[i] = out[i] * flatscale / flat[i]
	case FO: # flat field + overscan
	    do i = 1, n
	        out[i] = (out[i] - overscan) * flatscale / flat[i]
	case FZ: # flat field + zero level
	    do i = 1, n
		out[i] = (out[i] - zero[i]) * flatscale / flat[i]
	case FZO: # flat field + zero level + overscan
	    do i = 1, n
		out[i] = (out[i] - overscan - zero[i]) * flatscale /
		    flat[i]
	case FD: # flat field + dark count
	    do i = 1, n
		out[i] = (out[i] - darkscale * dark[i]) * flatscale / flat[i]
	case FDO: # flat field + dark count + overscan
	    do i = 1, n
		out[i] = (out[i] - overscan - darkscale * dark[i]) *
		    flatscale / flat[i]
	case FDZ: # flat field + dark count + zero level
	    do i = 1, n
		out[i] = (out[i] - zero[i] - darkscale * dark[i]) *
		    flatscale / flat[i]
	case FDZO: # flat field + dark count + zero level + overscan
	    do i = 1, n
		out[i] = (out[i] - overscan - zero[i] -
		    darkscale * dark[i]) * flatscale / flat[i]
	}

	# Often these operations will not be performed so test for no
	# correction rather than go through the switch.

	op = cors[ILLUMCOR] + cors[FRINGECOR]
	if (op != 0) {
	    switch (op) {
	    case I: # illumination
	        do i = 1, n
		    out[i] = out[i] * illumscale / illum[i]
	    case Q: # fringe
	        do i = 1, n
		    out[i] = out[i] - frgscale * fringe[i]
	    case QI: # fringe + illumination
	        do i = 1, n
		    out[i] = out[i]*illumscale/illum[i] - frgscale*fringe[i]
	    }
	}
end


# COR2 -- Correct lines for readout axis 2 (columns).  This procedure is
# more complex than when the readout is along the image lines because the
# zero level and/or flat field corrections may be single readout column
# vectors.

procedure cor2s (line, cors, out, overscan, zero, dark, flat, illum,
	fringe, n, zeroim, flatim, darkscale, flatscale, illumscale, frgscale)

int	line		# Line to be corrected
int	cors[ARB]	# Correction flags
short	out[n]		# Output data
real	overscan[n]	# Overscan value
short	zero[n]		# Zero level correction
short	dark[n]		# Dark count correction
short	flat[n]		# Flat field correction
short	illum[n]	# Illumination correction
short	fringe[n]	# Fringe correction
int	n		# Number of pixels
pointer	zeroim		# Zero level IMIO pointer (NULL if 1D vector)
pointer	flatim		# Flat field IMIO pointer (NULL if 1D vector)
real	darkscale	# Dark count scale factor
real	flatscale	# Flat field scale factor
real	illumscale	# Illumination scale factor
real	frgscale	# Fringe scale factor

short	zeroval
real	flatval
int	i, op

begin
	op = cors[OVERSCAN] + cors[ZEROCOR] + cors[DARKCOR] + cors[FLATCOR]
	switch (op) {
	case O: # overscan
	    do i = 1, n
	        out[i] = out[i] - overscan[i]
	case Z: # zero level
	    if (zeroim != NULL)
	        do i = 1, n
		    out[i] = out[i] - zero[i]
	    else {
		zeroval = zero[line]
	        do i = 1, n
		    out[i] = out[i] - zeroval
	    }

	case ZO: # zero level + overscan
	    if (zeroim != NULL)
	        do i = 1, n
		    out[i] = out[i] - overscan[i] - zero[i]
	    else {
		zeroval = zero[line]
	        do i = 1, n
		    out[i] = out[i] - overscan[i] - zeroval
	    }

	case D: # dark count
	    do i = 1, n
		out[i] = out[i] - darkscale * dark[i]
	case DO: # dark count + overscan
	    do i = 1, n
		out[i] = out[i] - overscan[i] - darkscale * dark[i]
	case DZ: # dark count + zero level
	    if (zeroim != NULL)
	        do i = 1, n
		    out[i] = out[i] - zero[i] - darkscale * dark[i]
	    else {
		zeroval = zero[line]
	        do i = 1, n
		    out[i] = out[i] - zeroval - darkscale * dark[i]
	    }
	case DZO: # dark count + zero level + overscan
	    if (zeroim != NULL)
	        do i = 1, n
		    out[i] = out[i] - overscan[i] - zero[i] -
			darkscale * dark[i]
	    else {
		zeroval = zero[line]
	        do i = 1, n
		    out[i] = out[i] - overscan[i] - zeroval -
			darkscale * dark[i]
	    }

	case F: # flat field
	    if (flatim != NULL) {
	        do i = 1, n
		    out[i] = out[i] * flatscale / flat[i]
	    } else {
		flatval = flatscale / flat[line]
	        do i = 1, n
		    out[i] = out[i] * flatval
	    }
	case FO: # flat field + overscan
	    if (flatim != NULL) {
	        do i = 1, n
	            out[i] = (out[i] - overscan[i]) * flatscale / flat[i]
	    } else {
		flatval = flatscale / flat[line]
	        do i = 1, n
	            out[i] = (out[i] - overscan[i]) * flatval
	    }
	case FZ: # flat field + zero level
	    if (flatim != NULL) {
	        if (zeroim != NULL) {
	            do i = 1, n
		        out[i] = (out[i] - zero[i]) * flatscale / flat[i]
		} else {
		    zeroval = zero[line]
	            do i = 1, n
		        out[i] = (out[i] - zeroval) * flatscale / flat[i]
		}
	    } else {
		flatval = flatscale / flat[line]
		if (zeroim != NULL) {
	            do i = 1, n
		        out[i] = (out[i] - zero[i]) * flatval
	        } else {
		    zeroval = zero[line]
	            do i = 1, n
		        out[i] = (out[i] - zeroval) * flatval
		}
	    }
	case FZO: # flat field + zero level + overscan
	    if (flatim != NULL) {
	        if (zeroim != NULL) {
	            do i = 1, n
		        out[i] = (out[i] - overscan[i] - zero[i]) *
			    flatscale / flat[i]
		} else {
		    zeroval = zero[line]
	            do i = 1, n
		        out[i] = (out[i] - overscan[i] - zeroval) *
			    flatscale / flat[i]
		}
	    } else {
		flatval = flatscale / flat[line]
		if (zeroim != NULL) {
	            do i = 1, n
		        out[i] = (out[i] - overscan[i] - zero[i]) * flatval
	        } else {
		    zeroval = zero[line]
	            do i = 1, n
		        out[i] = (out[i] - overscan[i] - zeroval) * flatval
		}
	    }
	case FD: # flat field + dark count
	    if (flatim != NULL) {
	        do i = 1, n
		    out[i] = (out[i] - darkscale * dark[i]) * flatscale/flat[i]
	    } else {
		flatval = flatscale / flat[line]
	        do i = 1, n
		    out[i] = (out[i] - darkscale * dark[i]) * flatval
	    }
	case FDO: # flat field + dark count + overscan
	    if (flatim != NULL) {
	        do i = 1, n
		    out[i] = (out[i] - overscan[i] - darkscale * dark[i]) *
		        flatscale / flat[i]
	    } else {
		flatval = flatscale / flat[line]
	        do i = 1, n
		    out[i] = (out[i] - overscan[i] - darkscale * dark[i]) *
		        flatval
	    }
	case FDZ: # flat field + dark count + zero level
	    if (flatim != NULL) {
	        if (zeroim != NULL) {
	            do i = 1, n
		        out[i] = (out[i] - zero[i] - darkscale * dark[i]) *
			    flatscale / flat[i]
		} else {
		    zeroval = zero[line]
	            do i = 1, n
		        out[i] = (out[i] - zeroval - darkscale * dark[i]) *
			    flatscale / flat[i]
		}
	    } else {
		flatval = flatscale / flat[line]
		if (zeroim != NULL) {
	            do i = 1, n
		        out[i] = (out[i] - zero[i] - darkscale * dark[i]) *
			    flatval
	        } else {
		    zeroval = zero[line]
	            do i = 1, n
		        out[i] = (out[i] - zeroval - darkscale * dark[i]) *
			    flatval
		}
	    }
	case FDZO: # flat field + dark count + zero level + overscan
	    if (flatim != NULL) {
	        if (zeroim != NULL) {
	            do i = 1, n
		        out[i] = (out[i] - overscan[i] - zero[i] -
			    darkscale * dark[i]) * flatscale / flat[i]
		} else {
		    zeroval = zero[line]
	            do i = 1, n
		        out[i] = (out[i] - overscan[i] - zeroval -
			    darkscale * dark[i]) * flatscale / flat[i]
		}
	    } else {
		flatval = flatscale / flat[line]
		if (zeroim != NULL) {
	            do i = 1, n
		        out[i] = (out[i] - overscan[i] - zero[i] -
			    darkscale * dark[i]) * flatval
	        } else {
		    zeroval = zero[line]
	            do i = 1, n
		        out[i] = (out[i] - overscan[i] - zeroval -
			    darkscale * dark[i]) * flatval
		}
	    }
	}

	# Often these operations will not be performed so test for no
	# correction rather than go through the switch.

	op = cors[ILLUMCOR] + cors[FRINGECOR]
	if (op != 0) {
	    switch (op) {
	    case I: # illumination
	        do i = 1, n
		    out[i] = out[i] * illumscale / illum[i]
	    case Q: # fringe
	        do i = 1, n
		    out[i] = out[i] - frgscale * fringe[i]
	    case QI: # fringe + illumination
	        do i = 1, n
		    out[i] = out[i]*illumscale/illum[i] - frgscale*fringe[i]
	    }
	}
end

# COR1 -- Correct image lines with readout axis 1 (lines).

procedure cor1r (cors, out, overscan, zero, dark, flat, illum,
	fringe, n, darkscale, flatscale, illumscale, frgscale)

int	cors[ARB]	# Correction flags
real	out[n]		# Output data
real	overscan	# Overscan value
real	zero[n]		# Zero level correction
real	dark[n]		# Dark count correction
real	flat[n]		# Flat field correction
real	illum[n]	# Illumination correction
real	fringe[n]	# Fringe correction
int	n		# Number of pixels
real	darkscale	# Dark count scale factor
real	flatscale	# Flat field scale factor
real	illumscale	# Illumination scale factor
real	frgscale	# Fringe scale factor

int	i, op

begin
	op = cors[OVERSCAN] + cors[ZEROCOR] + cors[DARKCOR] + cors[FLATCOR]
	switch (op) {
	case O: # overscan
	    do i = 1, n
	        out[i] = out[i] - overscan
	case Z: # zero level
	    do i = 1, n
		out[i] = out[i] - zero[i]

	case ZO: # zero level + overscan
	    do i = 1, n
		out[i] = out[i] - overscan - zero[i]

	case D: # dark count
	    do i = 1, n
		out[i] = out[i] - darkscale * dark[i]
	case DO: # dark count + overscan
	    do i = 1, n
		out[i] = out[i] - overscan - darkscale * dark[i]
	case DZ: # dark count + zero level
	    do i = 1, n
		out[i] = out[i] - zero[i] - darkscale * dark[i]
	case DZO: # dark count + zero level + overscan
	    do i = 1, n
		out[i] = out[i] - overscan - zero[i] - darkscale * dark[i]

	case F: # flat field
	    do i = 1, n
		out[i] = out[i] * flatscale / flat[i]
	case FO: # flat field + overscan
	    do i = 1, n
	        out[i] = (out[i] - overscan) * flatscale / flat[i]
	case FZ: # flat field + zero level
	    do i = 1, n
		out[i] = (out[i] - zero[i]) * flatscale / flat[i]
	case FZO: # flat field + zero level + overscan
	    do i = 1, n
		out[i] = (out[i] - overscan - zero[i]) * flatscale /
		    flat[i]
	case FD: # flat field + dark count
	    do i = 1, n
		out[i] = (out[i] - darkscale * dark[i]) * flatscale / flat[i]
	case FDO: # flat field + dark count + overscan
	    do i = 1, n
		out[i] = (out[i] - overscan - darkscale * dark[i]) *
		    flatscale / flat[i]
	case FDZ: # flat field + dark count + zero level
	    do i = 1, n
		out[i] = (out[i] - zero[i] - darkscale * dark[i]) *
		    flatscale / flat[i]
	case FDZO: # flat field + dark count + zero level + overscan
	    do i = 1, n
		out[i] = (out[i] - overscan - zero[i] -
		    darkscale * dark[i]) * flatscale / flat[i]
	}

	# Often these operations will not be performed so test for no
	# correction rather than go through the switch.

	op = cors[ILLUMCOR] + cors[FRINGECOR]
	if (op != 0) {
	    switch (op) {
	    case I: # illumination
	        do i = 1, n
		    out[i] = out[i] * illumscale / illum[i]
	    case Q: # fringe
	        do i = 1, n
		    out[i] = out[i] - frgscale * fringe[i]
	    case QI: # fringe + illumination
	        do i = 1, n
		    out[i] = out[i]*illumscale/illum[i] - frgscale*fringe[i]
	    }
	}
end


# COR2 -- Correct lines for readout axis 2 (columns).  This procedure is
# more complex than when the readout is along the image lines because the
# zero level and/or flat field corrections may be single readout column
# vectors.

procedure cor2r (line, cors, out, overscan, zero, dark, flat, illum,
	fringe, n, zeroim, flatim, darkscale, flatscale, illumscale, frgscale)

int	line		# Line to be corrected
int	cors[ARB]	# Correction flags
real	out[n]		# Output data
real	overscan[n]	# Overscan value
real	zero[n]		# Zero level correction
real	dark[n]		# Dark count correction
real	flat[n]		# Flat field correction
real	illum[n]	# Illumination correction
real	fringe[n]	# Fringe correction
int	n		# Number of pixels
pointer	zeroim		# Zero level IMIO pointer (NULL if 1D vector)
pointer	flatim		# Flat field IMIO pointer (NULL if 1D vector)
real	darkscale	# Dark count scale factor
real	flatscale	# Flat field scale factor
real	illumscale	# Illumination scale factor
real	frgscale	# Fringe scale factor

real	zeroval
real	flatval
int	i, op

begin
	op = cors[OVERSCAN] + cors[ZEROCOR] + cors[DARKCOR] + cors[FLATCOR]
	switch (op) {
	case O: # overscan
	    do i = 1, n
	        out[i] = out[i] - overscan[i]
	case Z: # zero level
	    if (zeroim != NULL)
	        do i = 1, n
		    out[i] = out[i] - zero[i]
	    else {
		zeroval = zero[line]
	        do i = 1, n
		    out[i] = out[i] - zeroval
	    }

	case ZO: # zero level + overscan
	    if (zeroim != NULL)
	        do i = 1, n
		    out[i] = out[i] - overscan[i] - zero[i]
	    else {
		zeroval = zero[line]
	        do i = 1, n
		    out[i] = out[i] - overscan[i] - zeroval
	    }

	case D: # dark count
	    do i = 1, n
		out[i] = out[i] - darkscale * dark[i]
	case DO: # dark count + overscan
	    do i = 1, n
		out[i] = out[i] - overscan[i] - darkscale * dark[i]
	case DZ: # dark count + zero level
	    if (zeroim != NULL)
	        do i = 1, n
		    out[i] = out[i] - zero[i] - darkscale * dark[i]
	    else {
		zeroval = zero[line]
	        do i = 1, n
		    out[i] = out[i] - zeroval - darkscale * dark[i]
	    }
	case DZO: # dark count + zero level + overscan
	    if (zeroim != NULL)
	        do i = 1, n
		    out[i] = out[i] - overscan[i] - zero[i] -
			darkscale * dark[i]
	    else {
		zeroval = zero[line]
	        do i = 1, n
		    out[i] = out[i] - overscan[i] - zeroval -
			darkscale * dark[i]
	    }

	case F: # flat field
	    if (flatim != NULL) {
	        do i = 1, n
		    out[i] = out[i] * flatscale / flat[i]
	    } else {
		flatval = flatscale / flat[line]
	        do i = 1, n
		    out[i] = out[i] * flatval
	    }
	case FO: # flat field + overscan
	    if (flatim != NULL) {
	        do i = 1, n
	            out[i] = (out[i] - overscan[i]) * flatscale / flat[i]
	    } else {
		flatval = flatscale / flat[line]
	        do i = 1, n
	            out[i] = (out[i] - overscan[i]) * flatval
	    }
	case FZ: # flat field + zero level
	    if (flatim != NULL) {
	        if (zeroim != NULL) {
	            do i = 1, n
		        out[i] = (out[i] - zero[i]) * flatscale / flat[i]
		} else {
		    zeroval = zero[line]
	            do i = 1, n
		        out[i] = (out[i] - zeroval) * flatscale / flat[i]
		}
	    } else {
		flatval = flatscale / flat[line]
		if (zeroim != NULL) {
	            do i = 1, n
		        out[i] = (out[i] - zero[i]) * flatval
	        } else {
		    zeroval = zero[line]
	            do i = 1, n
		        out[i] = (out[i] - zeroval) * flatval
		}
	    }
	case FZO: # flat field + zero level + overscan
	    if (flatim != NULL) {
	        if (zeroim != NULL) {
	            do i = 1, n
		        out[i] = (out[i] - overscan[i] - zero[i]) *
			    flatscale / flat[i]
		} else {
		    zeroval = zero[line]
	            do i = 1, n
		        out[i] = (out[i] - overscan[i] - zeroval) *
			    flatscale / flat[i]
		}
	    } else {
		flatval = flatscale / flat[line]
		if (zeroim != NULL) {
	            do i = 1, n
		        out[i] = (out[i] - overscan[i] - zero[i]) * flatval
	        } else {
		    zeroval = zero[line]
	            do i = 1, n
		        out[i] = (out[i] - overscan[i] - zeroval) * flatval
		}
	    }
	case FD: # flat field + dark count
	    if (flatim != NULL) {
	        do i = 1, n
		    out[i] = (out[i] - darkscale * dark[i]) * flatscale/flat[i]
	    } else {
		flatval = flatscale / flat[line]
	        do i = 1, n
		    out[i] = (out[i] - darkscale * dark[i]) * flatval
	    }
	case FDO: # flat field + dark count + overscan
	    if (flatim != NULL) {
	        do i = 1, n
		    out[i] = (out[i] - overscan[i] - darkscale * dark[i]) *
		        flatscale / flat[i]
	    } else {
		flatval = flatscale / flat[line]
	        do i = 1, n
		    out[i] = (out[i] - overscan[i] - darkscale * dark[i]) *
		        flatval
	    }
	case FDZ: # flat field + dark count + zero level
	    if (flatim != NULL) {
	        if (zeroim != NULL) {
	            do i = 1, n
		        out[i] = (out[i] - zero[i] - darkscale * dark[i]) *
			    flatscale / flat[i]
		} else {
		    zeroval = zero[line]
	            do i = 1, n
		        out[i] = (out[i] - zeroval - darkscale * dark[i]) *
			    flatscale / flat[i]
		}
	    } else {
		flatval = flatscale / flat[line]
		if (zeroim != NULL) {
	            do i = 1, n
		        out[i] = (out[i] - zero[i] - darkscale * dark[i]) *
			    flatval
	        } else {
		    zeroval = zero[line]
	            do i = 1, n
		        out[i] = (out[i] - zeroval - darkscale * dark[i]) *
			    flatval
		}
	    }
	case FDZO: # flat field + dark count + zero level + overscan
	    if (flatim != NULL) {
	        if (zeroim != NULL) {
	            do i = 1, n
		        out[i] = (out[i] - overscan[i] - zero[i] -
			    darkscale * dark[i]) * flatscale / flat[i]
		} else {
		    zeroval = zero[line]
	            do i = 1, n
		        out[i] = (out[i] - overscan[i] - zeroval -
			    darkscale * dark[i]) * flatscale / flat[i]
		}
	    } else {
		flatval = flatscale / flat[line]
		if (zeroim != NULL) {
	            do i = 1, n
		        out[i] = (out[i] - overscan[i] - zero[i] -
			    darkscale * dark[i]) * flatval
	        } else {
		    zeroval = zero[line]
	            do i = 1, n
		        out[i] = (out[i] - overscan[i] - zeroval -
			    darkscale * dark[i]) * flatval
		}
	    }
	}

	# Often these operations will not be performed so test for no
	# correction rather than go through the switch.

	op = cors[ILLUMCOR] + cors[FRINGECOR]
	if (op != 0) {
	    switch (op) {
	    case I: # illumination
	        do i = 1, n
		    out[i] = out[i] * illumscale / illum[i]
	    case Q: # fringe
	        do i = 1, n
		    out[i] = out[i] - frgscale * fringe[i]
	    case QI: # fringe + illumination
	        do i = 1, n
		    out[i] = out[i]*illumscale/illum[i] - frgscale*fringe[i]
	    }
	}
end

