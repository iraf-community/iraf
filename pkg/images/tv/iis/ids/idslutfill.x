# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gki.h>

# IDSLUTFILL -- Fill a lookup table from a set of line end points

procedure idslfill (in, icount, out, lenlut, lutmin, lutmax)

short	in[ARB]			# input: line end points
int	icount			# number of input data items
short	out[ARB]		# output: the lookup table
int	lenlut			# lut size
int	lutmin,lutmax		# inclusive range for lut values

int	i,j
int	xs, ys, xe, ye
real	slope

begin
	# xs and xe are zero based coordinates
	xs = real(in[1]) * (lenlut - 1)/GKI_MAXNDC. + 0.5
	ys = real(in[2]) * (lutmax - lutmin)/GKI_MAXNDC. + lutmin + 0.5
	do i = 3, icount, 2 {
	    xe = real(in[i]) * (lenlut - 1)/GKI_MAXNDC. + 0.5
	    ye = real(in[i+1]) * (lutmax - lutmin)/GKI_MAXNDC. + lutmin + 0.5
	    if (xe != xs) {
	        slope = real(ye - ys) / (xe - xs)
	        do j = xs, xe {
	            out[j+1] = ys + (j - xs) * slope
	        }
	    }
	    xs = xe
	    ys = ye
	}
	out[1] = 0			# keep background at zero
end
