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
		 ieemap[rd] (mapin, mapout)
		ieestat[rd] (nin, nout)
	       ieezstat[rd] ()

The first two routines handle scalar conversions, the second two routines
vector conversions.  The input and output vectors may be the same.
Unfortunately, for portability reasons, functions cannot be used, so the
scalar operators do an in-place conversion instead, and are a no-op on an
unswapped IEEE system.  The routines iee[sg]nan[rd] set/get the native
floating value used to replace NaNs or overflows occuring when converting
IEEE to the native floating format (any floating value will do, e.g., zero or
INDEFR).  If NaN mapping is enabled, the ieestat[rd] routines may be used to
determine the number of input or output NaN conversions occuring since the
last call to ieezstat[rd].

The NaN mapping enable switch and statistics counters are UNDEFINED at
process startup; programs which use the IEEE conversion package should call
ieemap[rd] to enable or disable NaN mapping, and ieezstat[rd] to initialize
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
define	IEEE_SWAP	IEEE_SWAP4
define	BSWAP		bswap4
define	NSWAP		4


# IEEVPAK -- Convert an array in the native floating point format into an
# array in IEEE floating format.  The input and output arrays can be the same.

procedure ieevpakr (native, ieee, nelem)

real	native[ARB]		#I input native floating format array
real	ieee[ARB]		#O output IEEE floating format array
int	nelem			#I number of floating point numbers

int	i
real	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenanr/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

begin
	if (mapout == NO) {
	    if (IEEE_SWAP == YES)
		call BSWAP (native, 1, ieee, 1, nelem * NSWAP)
	    else
		call amovr (native, ieee, nelem)
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

procedure ieevupkr (ieee, native, nelem)

real	ieee[ARB]		#I input IEEE floating format array
real	native[ARB]		#O output native floating format array
int	nelem			#I number of floating point numbers

int	i
real	fval
int	ival[1]
%	equivalence (fval, ival)

real	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenanr/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

begin
	if (IEEE_SWAP == YES) {
	    call BSWAP (ieee, 1, native, 1, nelem * NSWAP)
	    if (mapin != NO)
		do i = 1, nelem {
		    fval = native[i]
	            if (and (ival[1], NaNmask) == NaNmask) {
			native[i] = native_NaN
			nin = nin + 1
		    }
		}
	} else {
	    if (mapin == NO)
		call amovr (ieee, native, nelem)
	    else {
		do i = 1, nelem {
		    fval = ieee[i]
	            if (and (ival[1], NaNmask) == NaNmask) {
			native[i] = native_NaN
			nin = nin + 1
		    } else
			native[i] = ieee[i]
		}
	    }
	}
end


# IEEPAK -- Convert a native floating point number into IEEE format.

procedure ieepakr (x)

real	x			#U datum to be converted

real	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenanr/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

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

procedure ieeupkr (x)

real	x			#U datum to be converted

real	fval
int	ival[1]
%	equivalence (fval, ival)

real	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenanr/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

begin
	if (IEEE_SWAP == YES)
	    call BSWAP (x, 1, x, 1, NSWAP)
	if (mapin != NO) {
	    fval = x
	    if (and (ival[1], NaNmask) == NaNmask) {
	        x = native_NaN
		nin = nin + 1
	    }
	}
end


# IEESNAN -- Set the native floating point value used to replace NaNs and
# overflows when converting IEEE to native.  This must be a legal (finite)
# native floating point value.  Setting the reserved native pseudo-NaN value
# has the side effect of enabling NaN mapping and zeroing the statistics
# counters.

procedure ieesnanr (x)

real	x			#I native value which will replace NaN

real	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenanr/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

begin
	native_NaN = x
	call ieemapr (YES, YES)
	nin = 0
	nout = 0
end


# IEEGNAN -- Get the NaN value.

procedure ieegnanr (x)

real	x			#O native value which will replace NaN

real	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenanr/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

begin
	x = native_NaN
end


# IEESTAT -- Return statistics on the number of NaNs encountered in input
# conversions (unpack) and output conversions (pack).

procedure ieestatr (o_nin, o_nout)

int	o_nin				#O number of NaN seen on input
int	o_nout				#O number of NaN values output

real	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenanr/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

begin
	o_nin = nin
	o_nout = nout
end


# IEEZSTAT -- Zero the statistics counters.

procedure ieezstatr ()

real	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenanr/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

begin
	nin = 0
	nout = 0
end


# MACHINE DEPENDENT PART.
# ---------------------------

# IEEMAP -- Enable or disable NaN mapping.
#
# sEEE EEEE Emmm mmmm mmmm mmmm mmmm mmmm
#  3           2            1           0
# 1098 7654 3210 9876 5432 1098 7654 3210
#    7    f    8    0    0    0    0    0

procedure ieemapr (inval, outval)

int	inval				#I enable NaN mapping for input?
int	outval				#I enable NaN mapping for output?

# MACHDEP.
real	fval
int	ival[1]
%	equivalence (fval, ival)
%	data	ival(1) / '7ff7ffff'x /

real	native_NaN, ieee_NaN
int	mapin, mapout, nin, nout, NaNmask
common	/ieenanr/ native_NaN, ieee_NaN, NaNmask, mapin, mapout, nin, nout

begin
	mapin = inval
	mapout = outval

	# MACHDEP.
	if (mapout == YES)
	    ieee_NaN = fval

	if (mapin == YES)
	    NaNmask = 7F800000X
end
