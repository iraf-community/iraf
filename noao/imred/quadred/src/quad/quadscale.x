include <imhdr.h>
include "quadgeom.h"

define	OPPERATIONS	"|multiply|divide|"
define	OPMULTIPLY	1
define	OPDIVIDE	2
define	TOL1		 1.0e-7
define	TOL2		-1.0e-7

procedure t_quadscale ()

char	input[SZ_FNAME]		#TI Input image name.
char	output[SZ_FNAME]	#TI Output image name.
char    instrument[SZ_FNAME]    #TI Instrument translation file

real	gain[QG_MAXAMPS]	#TI Gain factor for each quadrant
int	op			#TI Multiply or divide by gain factors

int	i
pointer	in, out, qg
char	buffer[SZ_LINE]

real	clgetr()
int	clgwrd(), hdmaccf()
pointer	immap()

begin
        # Open instrument file
        call clgstr ("instrument",  instrument,  SZ_FNAME)
        call hdmopen (instrument)

	# Open input image
	call clgstr ("input",  input,  SZ_FNAME)
	in = immap  (input, READ_ONLY, 0)

	# Allocate quadgeom structure and initialise it from image header
	call quadalloc (qg)


	# Fill out quadgeom structure from header depending on revision level
	if (hdmaccf (in, "HDR_REV") == NO) {

	    # Check to see if the image has been processed or not
	    if (hdmaccf (in, "ccdproc") == YES) {
		call quadgeomred (in, qg)
	    } else {
		call quadgeom (in, qg, "", "")
	    }

	} else {
	    call qghdr2 (in, qg)
	}

	# Open output image
	call clgstr ("output",  output,  SZ_FNAME)
	out = immap (output, NEW_COPY, in)
	IM_PIXTYPE(out) = TY_REAL

	# Get gain factors
	gain[1] = clgetr ("gain11")
	gain[2] = clgetr ("gain12")
	gain[3] = clgetr ("gain21")
	gain[4] = clgetr ("gain22")

	# Get direction of opperation
	op = clgwrd ("opperation", buffer, SZ_LINE, OPPERATIONS)

	switch (op) {
	case OPMULTIPLY:
	    call quadmult (in, out, gain, qg)

	case OPDIVIDE:
	    # Check for zero gain --> divide by zero
	    do i = 1, 4 {
		if ((gain[i] < TOL1) && (gain[i] > TOL2)) {
		    call error (0, "Attempt to divide by gain value of zero")
		}
	    }
	    call quaddiv (in, out, gain, qg)

	}

	call imunmap (in)
	call imunmap (out)
	call quadfree (qg)
	call hdmclose ()
end

procedure quadmult (in, out, gain, qg)

pointer	in				#I imio pointer for input image.
pointer	out				#I imio pointer for output image.
real	gain[ARB]			#I Array of gain factors.
pointer	qg				#I Pointer to quadgeom structure.

pointer	inbuf, obuf
int	junk, nx, x, y, line, amp, amp2, off
long	invec[IM_MAXDIM], ovec[IM_MAXDIM]

int	imgnlr(), impnlr()

begin

	# Setup start vector for sequential reads
	call amovkl (long(1), invec, IM_MAXDIM)
	call amovkl (long(1), ovec,  IM_MAXDIM)

	do y = 1, QG_NAMPS(qg) {
	    amp2 = QG_AMP(qg, 1, y)
	    do line = 1, QG_NY(qg, amp2) {
		junk = imgnlr (in,  inbuf, invec)
		junk = impnlr (out, obuf,  ovec) 
		off = 0
		do x = 1, QG_NAMPSX(qg) {
		    amp = QG_AMP(qg, x, y)
		    nx = QG_NX(qg, amp)
		    call amulkr (Memr[inbuf+off], gain[amp], Memr[obuf+off], nx)
		    off = off + nx
		}
	    }
	}

end

procedure quaddiv (in, out, gain, qg)

pointer	in				#I imio pointer for input image.
pointer	out				#I imio pointer for output image.
real	gain[ARB]			#I Array of gain factors.
pointer	qg				#I Pointer to quadgeom structure.

pointer	inbuf, obuf
int	junk, nx, x, y, line, amp, amp2, off
long	invec[IM_MAXDIM], ovec[IM_MAXDIM]

int	imgnlr(), impnlr()

begin

	# Setup start vector for sequential reads
	call amovkl (long(1), invec, IM_MAXDIM)
	call amovkl (long(1), ovec,  IM_MAXDIM)

	do y = 1, QG_NAMPS(qg) {
	    amp2 = QG_AMP(qg, 1, y)
	    do line = 1, QG_NY(qg, amp2) {
		junk = imgnlr (in,  inbuf, invec)
		junk = impnlr (out, obuf,  ovec) 
		off = 0
		do x = 1, QG_NAMPSX(qg) {
		    amp = QG_AMP(qg, x, y)
		    nx = QG_NX(qg, amp)
		    call adivkr (Memr[inbuf+off], gain[amp], Memr[obuf+off], nx)
		    off = off + nx
		}
	    }
	}

end
