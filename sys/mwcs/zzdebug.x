# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<math.h>
include	<mwset.h>
include	"imwcs.h"

task	simple	= t_simple,
	wcs	= t_wcs,
	float	= t_float,
	imtest	= t_imtest,
	inv	= t_inv,
	save	= t_save,
	load	= t_load

define	SAVELEN		10240


# SIMPLE -- Simple test of the most common interface routines.

procedure t_simple()

pointer	mw, ct, bp
int	buflen, nchars
real	ltm[2,2], ltv[2], x1,y1, x2,y2
pointer	mw_open(), mw_sctran()
int	mw_save()

begin
	call memchk()
	mw = mw_open (NULL, 2)

	ltm[1,1] = 1.0;  ltm[1,2] = 0.0
	ltm[2,1] = 0.0;  ltm[2,2] = 1.0
	ltv[1]   = 0.0;  ltv[2]   = 0.0
	call mw_sltermr (mw, ltm, ltv, 2)

	ct = mw_sctran (mw, "logical", "physical", 0)
	x1 = 0.5;  y1 = 0.5
	call mw_c2tranr (ct, x1, y1, x2, y2)

	call eprintf ("[%g,%g] -> [%g,%g]\n")
	    call pargr (x1);  call pargr (y1)
	    call pargr (x2);  call pargr (y2)

	bp = NULL
	nchars = mw_save (mw, bp, buflen)
	call mw_load (mw, bp)

	call eprintf ("save/load, save buflen = %d chars, nchars=%d\n")
	    call pargi (buflen)
	    call pargi (nchars)

	ct = mw_sctran (mw, "logical", "physical", 0)
	x1 = 0.5;  y1 = 0.5
	call mw_c2tranr (ct, x1, y1, x2, y2)

	call eprintf ("[%g,%g] -> [%g,%g]\n")
	    call pargr (x1);  call pargr (y1)
	    call pargr (x2);  call pargr (y2)

	call mw_close (mw)
end


# WCS -- Test the creation and use of a world coordinate system.

procedure t_wcs()

pointer	mw, ct1, ct2, ct3
real	pv[100], wv[100]
real	theta, center[2], scale[2], shift[2]
real	ltm[3,3], ltv[3], x1,y1, x2,y2
double	r[3], w[3], cd[3,3]
double	l2m[2,2], l2v_1[2], l2v_2[2], d_theta
int	ndim, axes[3], naxes, npts, i
pointer	mw_open(), mw_sctran()
real	mw_c1tranr()

begin
	call memchk()
	ndim = 3

	# Create a unitary, 3 dim WCS.
	mw = mw_open (NULL, ndim)

	# Examine the Lterm.
	call plterm (mw, ltm, ltv, ndim)

	# Apply a transform to the first 2 axes.
	d_theta = DEGTORAD(30.0D0)
	l2m[1,1] =  cos(d_theta);  l2m[2,1] = sin(d_theta)
	l2m[1,2] = -sin(d_theta);  l2m[2,2] = cos(d_theta)
	l2v_1[1] = 0.0;   l2v_1[2] = 0.0
	l2v_2[1] = 10.0;  l2v_2[2] = 20.0
	#l2v_2[1] = 0.0;  l2v_2[2] = 0.0

	#call mw_translated (mw, l2v_1, l2m, l2v_2, 2)
	theta = d_theta;  call aclrr (center, 2)
	call mw_rotate (mw, theta, center, 3B)
	shift[1] = 10.0;  shift[2] = 20.0
	call mw_shift (mw, shift, 3B)
	scale[1] = 4.0;  scale[2] = 0.2
	call mw_scale (mw, scale, 3B)

	# Examine the Lterm.
	call plterm (mw, ltm, ltv, ndim)

	# Apply the inverse transform.
	d_theta = -d_theta
	l2m[1,1] =  cos(d_theta);  l2m[2,1] = sin(d_theta)
	l2m[1,2] = -sin(d_theta);  l2m[2,2] = cos(d_theta)
	call amovd (l2v_2, l2v_1, 2);  call aclrd (l2v_2, 2)

	#call mw_translated (mw, l2v_1, l2m, l2v_2, 2)
	scale[1] = 1.0/scale[1];  scale[2] = 1.0/scale[2]
	call mw_scale (mw, scale, 3B)
	shift[1] = -shift[1];  shift[2] = -shift[2]
	call mw_shift (mw, shift, 3B)
	call mw_rotate (mw, -theta, center, 3B)

	# Examine the Lterm.
	call plterm (mw, ltm, ltv, ndim)

	# Add a WCS.
	call mw_newsystem (mw, "sky", 3)

	cd[1,1] = .01D0;  cd[2,1] = 0;    cd[3,1] = 0
	cd[1,2] = 0;    cd[2,2] = .01D0;  cd[3,2] = 0
	cd[1,3] = 0;    cd[2,3] = 0;    cd[3,3] = 1
	r[1]    = 0;    r[2]    = 0;    r[3]    = 0
	w[1]    = 100;  w[2]    = 20;   w[3]    = 0

	# Put a tangent projection on axis 1&2.
	call mw_swtermd (mw, r, w, cd, ndim)
	axes[1] = 1;  axes[2] = 2;  naxes = 2
	call mw_swtype (mw, axes, naxes, "tan",
	    "axis 1: axtype=ra  axis 2: axtype=dec")

	# Put a simple sampled curve on axis 3.
	call mw_swtype (mw, 3, 1, "sampled", "")
	npts = 10
	do i = 1, npts {
	    pv[i] = i
	    wv[i] = i * 2
	}
	call mw_swsampr (mw, 3, pv, wv, npts)

	# Try a transform on the axis 1-2 plane.
	ct1 = mw_sctran (mw, "logical", "sky", 3B)
	x1 = 50.0;  y1 = -20.0
	call mw_c2tranr (ct1, x1,y1, x2,y2)
	call eprintf ("[%g,%g]logical -> [%g,%g]sky\n")
	    call pargr (x1);  call pargr (y1)
	    call pargr (x2);  call pargr (y2)

	# Check out the reverse transform.
	ct2 = mw_sctran (mw, "sky", "logical", 3B)
	call mw_c2tranr (ct2, x2,y2, x1,y1)
	call eprintf ("[%g,%g]sky -> [%g,%g]logical\n")
	    call pargr (x2);  call pargr (y2)
	    call pargr (x1);  call pargr (y1)

	# Try evaluating the sampled axis.
	ct3 = mw_sctran (mw, "physical", "sky", 4B)
	x1 = 4.5;  x2 = mw_c1tranr (ct3, x1)
	call eprintf ("axis 3: %gL -> %gS\n")
	    call pargr (x1)
	    call pargr (x2)

	call mw_close (mw)
end


# PLTERM -- Print the Lterm.

procedure plterm (mw, ltm, ltv, ndim)

pointer	mw
real	ltm[ndim,ndim]
real	ltv[ndim]
int	ndim

int	i, j

begin
	# Examine the Lterm.
	call mw_gltermr (mw, ltm, ltv, ndim)
	call eprintf ("----- lterm -----\n")

	do j = 1, ndim {
	    do i = 1, ndim {
		call eprintf (" %8.3f")
		    call pargr (ltm[i,j])
	    }
	    call eprintf (" : %8.3f\n")
		call pargr (ltv[j])
	}
end


# IMTEST -- Test the image header WCS save and load facilities.

procedure t_imtest()

double	cd[3,3], r[3], w[3]
int	ndim, naxes, axes[2], npts, i
pointer	mw, ct1, ct2, ct3, im, iw, cp
real	theta, center[3], shift[3], scale[3], x1,y1, x2,y2, pv[10], wv[10]
pointer	mw_open(), mw_sctran(), immap(), iw_rfits()
real	mw_c1tranr()

begin
	call memchk()
	ndim = 3

	# Create a unitary, 3 dim WCS.
	mw = mw_open (NULL, ndim)

	# Apply a transform to the first 2 axes.
	call aclrr (center, 2)
	theta = DEGTORAD(30.0D0)
	shift[1] = 10.0;  shift[2] = 20.0
	scale[1] = 4.0;   scale[2] = 0.2

	call mw_rotate (mw, theta, center, 3B)
	call mw_shift (mw, shift, 3B)
	call mw_scale (mw, scale, 3B)

	# Add a WCS.
	call mw_newsystem (mw, "sky", 3)

	cd[1,1] = .01D0;  cd[2,1] = 0;    cd[3,1] = 0
	cd[1,2] = 0;    cd[2,2] = .01D0;  cd[3,2] = 0
	cd[1,3] = 0;    cd[2,3] = 0;    cd[3,3] = 1
	r[1]    = 0;    r[2]    = 0;    r[3]    = 0
	w[1]    = 100;  w[2]    = 20;   w[3]    = 0

	# Put a tangent projection on axis 1&2.
	call mw_swtermd (mw, r, w, cd, ndim)
	axes[1] = 1;  axes[2] = 2;  naxes = 2
	call mw_swtype (mw, axes, naxes, "tan",
	    "axis 1: axtype=ra  axis 2: axtype=dec")

	# Put a simple sampled curve on axis 3.
	call mw_swtype (mw, 3, 1, "sampled", "")
	npts = 10
	do i = 1, npts {
	    pv[i] = i
	    wv[i] = i * 2
	}
	call mw_swsampr (mw, 3, pv, wv, npts)

	# Evaluate tests 1.
	# -----------------

	# Try a transform on the axis 1-2 plane.
	ct1 = mw_sctran (mw, "logical", "sky", 3B)
	x1 = 50.0;  y1 = -20.0
	call mw_c2tranr (ct1, x1,y1, x2,y2)
	call eprintf ("[%g,%g]logical -> [%g,%g]sky\n")
	    call pargr (x1);  call pargr (y1)
	    call pargr (x2);  call pargr (y2)

	# Check out the reverse transform.
	ct2 = mw_sctran (mw, "sky", "logical", 3B)
	call mw_c2tranr (ct2, x2,y2, x1,y1)
	call eprintf ("[%g,%g]sky -> [%g,%g]logical\n")
	    call pargr (x2);  call pargr (y2)
	    call pargr (x1);  call pargr (y1)

	# Try evaluating the sampled axis.
	ct3 = mw_sctran (mw, "physical", "sky", 4B)
	x1 = 4.5;  x2 = mw_c1tranr (ct3, x1)
	call eprintf ("axis 3: %gL -> %gS\n")
	    call pargr (x1)
	    call pargr (x2)

	# Test image header save/load.
	call eprintf ("save WCS in image header...\n")
	#iferr (call imdelete ("pix"))
	#    ;
	im = immap ("pix", READ_WRITE, 0)
	call mw_saveim (mw, im)

	# See what we saved.
	call printf ("-------- IMAGE HEADER --------\n")
	iw = iw_rfits (mw, im, RF_REFERENCE)
	do i = 1, IW_NCARDS(iw) {
	    cp = IW_CARD(iw,i)
	    call write (STDOUT, Memc[C_RP(cp)], 80)
	    call putci (STDOUT, '\n')
	}
	call iw_cfits (iw)
	call printf ("------------------------------\n")
	call flush (STDOUT)

	# Reload saved header.
	call mw_loadim (mw, im)

	# Evaluate tests 2.
	# -----------------

	# Try a transform on the axis 1-2 plane.
	ct1 = mw_sctran (mw, "logical", "sky", 3B)
	x1 = 50.0;  y1 = -20.0
	call mw_c2tranr (ct1, x1,y1, x2,y2)
	call eprintf ("[%g,%g]logical -> [%g,%g]sky\n")
	    call pargr (x1);  call pargr (y1)
	    call pargr (x2);  call pargr (y2)

	# Check out the reverse transform.
	ct2 = mw_sctran (mw, "sky", "logical", 3B)
	call mw_c2tranr (ct2, x2,y2, x1,y1)
	call eprintf ("[%g,%g]sky -> [%g,%g]logical\n")
	    call pargr (x2);  call pargr (y2)
	    call pargr (x1);  call pargr (y1)

	# Try evaluating the sampled axis.
	ct3 = mw_sctran (mw, "physical", "sky", 4B)
	x1 = 4.5;  x2 = mw_c1tranr (ct3, x1)
	call eprintf ("axis 3: %gL -> %gS\n")
	    call pargr (x1)
	    call pargr (x2)

	call mw_close (mw)
end


# INV -- Test matrix inversion.

procedure t_inv()

int	i, j
double	a[3,3], b[3,3], c[3,3]
long	seed, clktime()
real	urand()

begin
	# Construct the identity matrix.
	do i = 1, 3 {
	    do j = 1, 3
		a[i,j] = 0.0
	    a[i,i] = 1.0
	}

	# Invert the matrix.
	call mw_invertd (a, b, 3)

	# Print the inverse.
	call printf ("inverse of identity matrix:\n")
	do i = 1, 3 {
	    do j = 1, 3 {
		call printf ("  %20.*f")
		    call pargi (NDIGITS_DP)
		    call pargd (b[i,j])
	    }
	    call printf ("\n")
	}

	# Compute a random matrix.
	seed = clktime(0)
	do i = 1, 3
	    do j = 1, 3
		a[i,j] = urand (seed)

	# Invert the matrix.
	call mw_invertd (a, b, 3)
	call mw_invertd (b, c, 3)

	# Print the difference of the original and the inverted inverse.
	call printf ("difference of inverse of random matrix:\n")
	do i = 1, 3 {
	    do j = 1, 3 {
		call printf ("  %20.*f")
		    call pargi (NDIGITS_DP)
		    call pargd (a[i,j] - c[i,j])
	    }
	    call printf ("\n")
	}
end


# SAVE -- Save a test WCS to a file.

procedure t_save()

pointer	mw, bp
double	cd[3,3], r[3], w[3]
int	ndim, naxes, axes[2], npts, buflen, nchars, fd, i
real	theta, center[3], shift[3], scale[3], pv[10], wv[10]
int	open(), mw_save
pointer	mw_open()

begin
	ndim = 3

	# Create a unitary, 3 dim WCS.
	mw = mw_open (NULL, ndim)

	# Apply a transform to the first 2 axes.
	call aclrr (center, 2)
	theta = DEGTORAD(30.0D0)
	shift[1] = 10.0;  shift[2] = 20.0
	scale[1] = 4.0;   scale[2] = 0.2

	call mw_rotate (mw, theta, center, 3B)
	call mw_shift (mw, shift, 3B)
	call mw_scale (mw, scale, 3B)

	# Add a WCS.
	call mw_newsystem (mw, "sky", 3)

	cd[1,1] = .01D0;  cd[2,1] = 0;    cd[3,1] = 0
	cd[1,2] = 0;    cd[2,2] = .01D0;  cd[3,2] = 0
	cd[1,3] = 0;    cd[2,3] = 0;    cd[3,3] = 1
	r[1]    = 0;    r[2]    = 0;    r[3]    = 0
	w[1]    = 100;  w[2]    = 20;   w[3]    = 0

	# Put a tangent projection on axis 1&2.
	call mw_swtermd (mw, r, w, cd, ndim)
	axes[1] = 1;  axes[2] = 2;  naxes = 2
	call mw_swtype (mw, axes, naxes, "tan",
	    "axis 1: axtype=ra  axis 2: axtype=dec")

	# Put a simple sampled curve on axis 3.
	call mw_swtype (mw, 3, 1, "sampled", "")
	npts = 10
	do i = 1, npts {
	    pv[i] = i
	    wv[i] = i * 2
	}
	call mw_swsampr (mw, 3, pv, wv, npts)

	# Display the new WCS.
	call mw_show (mw, STDOUT, 0)

	# Save to a file.
	bp = NULL;  buflen = 0
	nchars = mw_save (mw, bp, buflen)

	fd = open ("mwcs.sav", NEW_FILE, BINARY_FILE)
	call write (fd, Memc[bp], nchars)
	call close (fd)

	call mfree (bp, TY_CHAR)
	call mw_close (mw)
end


# LOAD -- Load a test WCS from a file.

procedure t_load()

pointer	mw, bp
int	fd, nchars
char	fname[SZ_FNAME]
int	open(), read()
pointer	mw_open()

begin
	call clgstr ("savefile", fname, SZ_FNAME)
	call malloc (bp, SAVELEN, TY_CHAR)

	# Open and read save file.
	fd = open (fname, READ_ONLY, BINARY_FILE)
	nchars = read (fd, Memc[bp], SAVELEN)
	call printf ("read %d chars from %s\n")
	    call pargi (nchars)
	    call pargstr (fname)

	mw = mw_open (NULL, 3)
	call mw_load (mw, bp)

	# Display the new WCS.
	call mw_show (mw, STDOUT, 0)

	call mw_close (mw)
	call mfree (bp, TY_CHAR)
end


# FLOAT -- Test single to double conversions.

procedure t_float()

real	r
double	x

begin
	x = sin(0.34567D0)
	r = 1.0
	call achtrd (r, x, 1)
	call printf ("x = %g\n")
	    call pargd (x)
end


# MEMCHK -- Enable runtime dynamic memory verification.  System dependent,
# should be commented out unless a Fortran callable MEMVER is available for
# linking.

procedure memchk()

begin
	# call memver (2)
end
