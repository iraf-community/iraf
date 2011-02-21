# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

.help IEEE
.nf ------------------------------------------------------------------------
Low level primitives for IEEE to native floating point datatype conversions.
See also the MII package, which provides a higher level interface, and the
IEEE related definitions in <mach.h>.

	         ieepak[rd] (datum)			# scalar conversions
	         ieeupk[rd] (datum)
		ieevpak[rd] (native, ieee, nelem)	# vector conversions
		ieevupk[rd] (ieee, native, nelem)
	     iee[sg]nan[rd] (NaN)			# NaN handling
	     iee[sg]map[rd] (mapin, mapout)
		ieestat[rd] (nin, nout)
	       ieezstat[rd] ()

The first two routines handle scalar conversions, the second two routines
vector conversions.  The input and output vectors may be the same.
Unfortunately, for portability reasons, functions cannot be used, so the
scalar operators do an in-place conversion instead, and are a no-op on an
unswapped IEEE system.  The routines iee[sg]nan[rd] set/get the native
floating value used to replace NaNs or overflows occuring when converting
IEEE to the native floating format (any floating value will do, e.g., zero or
INDEFD).  If NaN mapping is enabled, the ieestat[rd] routines may be used to
determine the number of input or output NaN conversions occuring since the
last call to ieezstat[rd].

The NaN mapping enable switch and statistics counters are UNDEFINED at
process startup; programs which use the IEEE conversion package should call
ieesmap[rd] to enable or disable NaN mapping, and ieezstat[rd] to initialize
the statistics counters.

The routines in this file are the "portable" versions.  The "portable"
solution it to merely copy the array, swapping the bytes if necessary - this
works on any host that uses the IEEE floating format.  NaN mapping is
implemented in the portable code, but will work properly only for input
conversions; for output, the IEEE NaN value is undefined in the portable
version of the code (it is trivial to supply this value in an as$ieee.gx
version of the code).
If the local host does
not use IEEE floating, or if a significant efficiency gain can be realized
by programming in assembler or C, a host specific version of this file should
be written, placed in AS, and referenced in the MKPKG special file list.
.endhelp -------------------------------------------------------------------


# Give the generic preprocessor some help.
define	IEEE_SWAP	IEEE_SWAP8
define	BSWAP		bswap8
define	NSWAP		8
define	IOFF		1	# MACHDEP (normally 1, 2 on e.g. Intel)


# IEEVPAK -- Convert an array in the native floating point format into an
# array in IEEE floating format.  The input and output arrays can be the same.

procedure ieevpakd (native, ieee, nelem)

double	native[ARB]		#I input native floating format array
double	ieee[ARB]		#O output IEEE floating format array
int	nelem			#I number of floating point numbers

int	i
double	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenand/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

begin
	if (mapout == NO) {
	    if (IEEE_SWAP == YES)
		call BSWAP (native, 1, ieee, 1, nelem * NSWAP)
	    else
		call amovd (native, ieee, nelem)
	} else {
	    do i = 1, nelem
		if (native[i] == native_NaN) {
		    ieee(i) = ieee_NaN
		    nout = nout + 1
		} else
		    ieee[i] = native[i]

	    # Byteswap if necessary.
	    if (IEEE_SWAP == YES)
		call BSWAP (ieee, 1, ieee, 1, nelem * NSWAP)
	}
end


# IEEVUPK -- Convert an array in IEEE floating format into the native
# floating point format.  The input and output arrays can be the same.

procedure ieevupkd (ieee, native, nelem)

double	ieee[ARB]		#I input IEEE floating format array
double	native[ARB]		#O output native floating format array
int	nelem			#I number of floating point numbers

int	expon, i, val
double	fval
int	ival[2]
%	equivalence (fval, ival)
int	iand32()
%	equivalence (ival, val)

double	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenand/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

begin
	if (IEEE_SWAP == YES) {
	    call BSWAP (ieee, 1, native, 1, nelem * NSWAP)
	    if (mapin != NO) {
		# Check for IEEE exceptional values and map NaN to the native
		# NaN value, and denormalized numbers (zero exponent) to zero.

		do i = 1, nelem {
		    fval = native[i]
		    if (SZ_INT == SZ_INT32)
		        expon = and (ival[IOFF], NaNmask)
		    else
		        expon = iand32 (val, NaNmask)
		    if (expon == 0) {
			native[i] = 0
		    } else if (expon == NaNmask) {
			native[i] = native_NaN
			nin = nin + 1
		    }
		}
	    }
	} else {
	    if (mapin == NO)
		call amovd (ieee, native, nelem)
	    else {
		# Check for IEEE exceptional values and map NaN to the native
		# NaN value, and denormalized numbers (zero exponent) to zero.

		do i = 1, nelem {
		    fval = ieee[i]
		    if (SZ_INT == SZ_INT32)
		        expon = and (ival[IOFF], NaNmask)
		    else
		        expon = iand32 (val, NaNmask)
		    if (expon == 0) {
			native[i] = 0
		    } else if (expon == NaNmask) {
			native[i] = native_NaN
			nin = nin + 1
		    } else
			native[i] = ieee[i]
		}
	    }
	}
end


# IEEPAK -- Convert a native floating point number into IEEE format.

procedure ieepakd (x)

double	x			#U datum to be converted

double	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenand/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

begin
	if (mapout != NO)
	    if (x == native_NaN) {
		x = ieee_NaN
		nout = nout + 1
	    }
	if (IEEE_SWAP == YES)
	    call BSWAP (x, 1, x, 1, NSWAP)
end


# IEEUPK -- Convert an IEEE format number into native floating point.

procedure ieeupkd (x)

double	x			#U datum to be converted

int	expon, val
double	fval
int	ival[2]
%	equivalence (fval, ival)
int	iand32()
%	equivalence (val, ival)

double	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenand/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

begin
	if (IEEE_SWAP == YES)
	    call BSWAP (x, 1, x, 1, NSWAP)

	# Check for IEEE exceptional values and map NaN to the native NaN
	# value, and denormalized numbers (zero exponent) to zero.

	if (mapin != NO) {
	    fval = x
	    if (SZ_INT == SZ_INT32)
	        expon = and (ival[IOFF], NaNmask)
	    else
	        expon = iand32 (val, NaNmask)
	    if (expon == 0)
		x = 0
	    else if (expon == NaNmask) {
	        x = native_NaN
		nin = nin + 1
	    }
	}
end


# IEESNAN -- Set the native floating point value used to replace NaNs and
# overflows when converting IEEE to native.  This must be a legal (finite)
# native floating point value.

procedure ieesnand (x)

double	x			#I native value which will replace NaN

double	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenand/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

begin
	native_NaN = x
	nin = 0
	nout = 0
end


# IEEGNAN -- Get the NaN value.

procedure ieegnand (x)

double	x			#O native value which will replace NaN

double	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenand/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

begin
	x = native_NaN
end


# IEESTAT -- Return statistics on the number of NaNs encountered in input
# conversions (unpack) and output conversions (pack).

procedure ieestatd (o_nin, o_nout)

int	o_nin				#O number of NaN seen on input
int	o_nout				#O number of NaN values output

double	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenand/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

begin
	o_nin = nin
	o_nout = nout
end


# IEEZSTAT -- Zero the statistics counters.

procedure ieezstatd ()

double	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenand/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

begin
	nin = 0
	nout = 0
end


# IEEMAP -- Same as IEESMAP.  Retained for backwards compatibility.

procedure ieemapd (inval, outval)

int	inval				#I enable mapping on input
int	outval				#I enable mapping on output

begin
	call ieesmapd (inval, outval)
end


# IEEGMAP -- Query the current values of the input and output mapping
# enables.

procedure ieegmapd (inval, outval)

int	inval				#O get input mapping enable flag
int	outval				#O get output mapping enable flag

double	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenand/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

begin
	inval = mapin
	outval = mapout
end


# MACHINE DEPENDENT PART.
# ---------------------------

# IEESMAP -- Enable or disable NaN mapping.
#
# sEEE EEEE Emmm mmmm mmmm mmmm mmmm mmmm
#  3           2            1           0
# 1098 7654 3210 9876 5432 1098 7654 3210
#    7    f    8    0    0    0    0    0

procedure ieesmapd (inval, outval)

int	inval				#I enable NaN mapping for input?
int	outval				#I enable NaN mapping for output?

# MACHDEP.
#$if (datatype == r)
#%	real r_quiet_nan
#$else
#%	double precision d_quiet_nan
#$endif

double	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenand/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

begin
	mapin = inval
	mapout = outval

	# MACHDEP.
#	if (mapout == YES)
#	$if (datatype == r)
#%	    ieeenn = r_quiet_NaN()
#	$else
#%	    ieeenn = d_quiet_NaN()
#	$endif

	if (mapin == YES)
	    NaNmask = 7FF00000X
end
