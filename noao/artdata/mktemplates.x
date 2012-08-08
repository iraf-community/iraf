include	<error.h>
include	<imhdr.h>
include	<math.h>
include	<math/iminterp.h>

# Template data structure
define	LEN_MKT		18
define	MKT_PROF	Memi[$1]	# Pointer to profile
define	MKT_MSI		Memi[$1+1]	# MSI interpolation pointer
define	MKT_NXM		Memi[$1+2]	# Number of X points in model
define	MKT_NYM		Memi[$1+3]	# Number of Y points in model
define	MKT_F		Memr[P2R($1+4)]	# Fraction of total flux in profile
define	MKT_SCALE	Memr[P2R($1+5)]	# Radius scale

define	MKT_NALLOC	Memi[$1+6]	# Allocated space for saved templates
define	MKT_N		Memi[$1+7]	# Number of saved templates

define	MKT_DATA	Memi[$1+8]	# Data pointer
define	MKT_PTRS	Memi[$1+9]	# Data pointers
define	MKT_NX		Memi[$1+10]	# Number of X pixels
define	MKT_NY		Memi[$1+11]	# Number of Y pixels
define	MKT_XC		Memi[$1+12]	# Subpixel X center
define	MKT_YC		Memi[$1+13]	# Subpixel Y center
define	MKT_FLUX	Memi[$1+14]	# Flux
define	MKT_R		Memi[$1+15]	# Radius
define	MKT_AR		Memi[$1+16]	# Axial ratio
define	MKT_PA		Memi[$1+17]	# Position angle

define	NALLOC	25		# Allocation block for saved templates
define	NPROF	5001		# Profile length
define	NY	11		# BINPROF binning parameter


# MKT_INIT    -- Initialize template memory.
# MKT_FREE    -- Free template memory.
# MKT_SAVE    -- Save a template
# MKT_GET     -- Get a template
# MKT_STAR    -- Set star and seeing templates.
# MKT_OBJECT  -- Set object profiles.
# MKT_GOBJECT -- Get image raster.
# MKT_BINPROF -- Bin intensity profile
# MKT_GSTAR   -- Get the precomputed template with center nearest (x,y).
# MKT_PROFILE -- Make template from profile.
# MKT_MSI     -- Make template from image interpolation function.


# MKT_INIT -- Initialize template memory.
# The symbol table is used as a simple way to store the object types by name.

procedure mkt_init ()

int	clgeti()
real	clgetr()

pointer	stopen()
include	"mktemplates.com"

begin
	nxc = clgeti ("nxc")
	nyc = clgeti ("nyc")
	nxssub = clgeti ("nxsub")
	nyssub = clgeti ("nysub")
	nxgsub = clgeti ("nxgsub")
	nygsub = clgeti ("nygsub")
	dynrange = clgetr ("dynrange")
	psfrange = clgetr ("psfrange")
	stp = stopen ("mkt", 10, 10, 10*SZ_FNAME)
end


# MKT_FREE -- Free template memory.

procedure mkt_free ()

int	i
pointer sym, mkt, sthead(), stnext()
include	"mktemplates.com"

begin
	# For each object type free the profile and image interpolator data,
	# the last unsaved data buffer, all saved templates, and the object
	# structure.  Finally free the symbol table.

	for (sym = sthead (stp); sym != NULL; sym = stnext (stp, sym)) {
	    mkt = Memi[sym]
	    if (mkt != NULL) {
	        call mfree (MKT_PROF(mkt), TY_REAL)
		if (MKT_MSI(mkt) != NULL)
		   call msifree (MKT_MSI(mkt))
	        call mfree (MKT_DATA(mkt), TY_REAL)
		if (MKT_NALLOC(mkt) > 0) {
		    do i = 0, MKT_N(mkt)-1
		        call mfree (Memi[MKT_PTRS(mkt)+i], TY_REAL)
		    call mfree (MKT_PTRS(mkt), TY_POINTER)
		    call mfree (MKT_NX(mkt), TY_INT)
		    call mfree (MKT_NY(mkt), TY_INT)
		    call mfree (MKT_XC(mkt), TY_REAL)
		    call mfree (MKT_YC(mkt), TY_REAL)
		    call mfree (MKT_FLUX(mkt), TY_REAL)
		    call mfree (MKT_R(mkt), TY_REAL)
		    call mfree (MKT_AR(mkt), TY_REAL)
		    call mfree (MKT_PA(mkt), TY_REAL)
		}
	        call mfree (mkt, TY_STRUCT)
	    }
	}
	call stclose (stp)
end


# MKT_SAVE -- Save a template
# If a template may be used more than once it may be saved upon direction of
# the user in the object list.  Otherwise the last unsaved template is
# freed for the next object.

procedure mkt_save (mkt, data, nx, ny, xc, yc, flux, r, ar, pa, save)

pointer	mkt		#I MKT pointer
pointer	data		#I Pointer to template data
int	nx, ny		#I Size of template
real	xc, yc		#I Subpixel center
real	flux		#I Flux
real	r		#I Effective radius
real	ar		#I Axial ratio
real	pa		#I Position angle
int	save		#I Save data?

int	i

begin
	if (save == NO) {
	    MKT_DATA(mkt) = data
	    return
	}

	if (MKT_NALLOC(mkt) == 0) {
	    i = NALLOC
	    call malloc (MKT_PTRS(mkt), i, TY_POINTER)
	    call malloc (MKT_NX(mkt), i, TY_INT)
	    call malloc (MKT_NY(mkt), i, TY_INT)
	    call malloc (MKT_XC(mkt), i, TY_REAL)
	    call malloc (MKT_YC(mkt), i, TY_REAL)
	    call malloc (MKT_FLUX(mkt), i, TY_REAL)
	    call malloc (MKT_R(mkt), i, TY_REAL)
	    call malloc (MKT_AR(mkt), i, TY_REAL)
	    call malloc (MKT_PA(mkt), i, TY_REAL)
	    MKT_NALLOC(mkt) = i
	} else if (MKT_N(mkt) == MKT_NALLOC(mkt)) {
	    i = MKT_NALLOC(mkt) + NALLOC
	    call realloc (MKT_PTRS(mkt), i, TY_POINTER)
	    call realloc (MKT_NX(mkt), i, TY_INT)
	    call realloc (MKT_NY(mkt), i, TY_INT)
	    call realloc (MKT_XC(mkt), i, TY_REAL)
	    call realloc (MKT_YC(mkt), i, TY_REAL)
	    call realloc (MKT_FLUX(mkt), i, TY_REAL)
	    call realloc (MKT_R(mkt), i, TY_REAL)
	    call realloc (MKT_AR(mkt), i, TY_REAL)
	    call realloc (MKT_PA(mkt), i, TY_REAL)
	    MKT_NALLOC(mkt) = i
	}
	i = MKT_N(mkt)
	Memi[MKT_PTRS(mkt)+i] = data
	Memi[MKT_NX(mkt)+i] = nx
	Memi[MKT_NY(mkt)+i] = ny
	Memr[MKT_XC(mkt)+i] = xc
	Memr[MKT_YC(mkt)+i] = yc
	Memr[MKT_FLUX(mkt)+i] = flux
	Memr[MKT_R(mkt)+i] = r
	Memr[MKT_AR(mkt)+i] = ar
	Memr[MKT_PA(mkt)+i] = pa
	MKT_N(mkt) = i + 1
end


# MKT_GET -- Get a template
# If not a saved template just free last unsaved template.
# If saved search for match with position, size, axial ratio, and pa.
# Return null if not found.

procedure mkt_get (mkt, data, nx, ny, xc, yc, flux, r, ar, pa, save)

pointer	mkt		#I MKT pointer
pointer	data		#O Pointer to template data
int	nx, ny		#O Size of template
real	xc, yc		#I Subpixel center
real	flux		#I Flux
real	r		#I Effective radius
real	ar		#I Axial ratio
real	pa		#I Position angle
int	save		#I Get saved template?

int	i
real	f

begin
	data = NULL
	call mfree (MKT_DATA(mkt), TY_REAL)
	if (save == NO)
	    return

	for (i=0; i<MKT_N(mkt); i=i+1) {
	    if (xc != Memr[MKT_XC(mkt)+i])
		next
	    if (yc != Memr[MKT_YC(mkt)+i])
		next
	    if (r != Memr[MKT_R(mkt)+i])
		next
	    if (ar != Memr[MKT_AR(mkt)+i])
		next
	    if (pa != Memr[MKT_PA(mkt)+i])
		next
	    data = Memi[MKT_PTRS(mkt)+i]
	    nx = Memi[MKT_NX(mkt)+i]
	    ny = Memi[MKT_NY(mkt)+i]
	    f = Memr[MKT_FLUX(mkt)+i]
	    if (f != flux) {
	        call amulkr (Memr[data], flux/f, Memr[data], nx*ny)
	        Memr[MKT_FLUX(mkt)+i] = flux
	    }
	    return
	}
end


# MKT_STAR -- Define star and seeing templates.
# The seeing template has a smaller range for efficiency.
# THe star templates are determined once over a grid of centers and
# then not evaluated again.

pointer procedure mkt_star (name)

char	name[ARB]	# Profile name or file

# Star and seeing parameters obatined through CLIO.
real	r		# Major axis sigma (pixels)
real	ar		# Axial ratio (minor / major)
real	pa		# Position angle (radians)

int	i, j, nxm, nym, nx, ny, fd
real	dr, flux, radius, seeing, beta, xc, yc, dxc, dyc, der[2]
pointer	sym, mkt1, mkt2, prof, prof1, asi, msi, data, im

bool	streq()
real	clgetr(), asieval()
int	open(), fscan(), nscan()
pointer	immap(), imgs2r(), stfind(), stenter()
errchk	immap, open, imgs2r, asifit, asieval, asider

include	"mktemplates.com"

begin
	# Check if previously defined.
	sym = stfind (stp, "star")
	if (sym != NULL)
	    return (Memi[sym])

	# Select type of star profile and set intensity profile array.
	# Compute the fraction of the total flux in the profile.
	# Insure that the profile subsamples a subpixel.

	star = NULL
	see = NULL
	prof = NULL
	msi = NULL
	if (streq (name, "gaussian")) {
	    r = clgetr ("radius") / sqrt (log (2.))
	    radius = sqrt (log (dynrange))
	    seeing = sqrt (log (psfrange))
	    nxm = max (NPROF, 1 + nint (radius*r*nxssub*nyssub))
	    call malloc (prof, nxm, TY_REAL)

	    dr = radius / (nxm - 1)
	    do i = 0, nxm - 1
		Memr[prof+i] = exp (-(i * dr) ** 2)
	    flux = 1 - Memr[prof+nxm-1]

	    r = sqrt (log (2.))
	    radius = radius / r
	    seeing = seeing / r
	} else if (streq (name, "moffat")) {
	    beta = clgetr ("beta")
	    r = clgetr ("radius") / sqrt (2. ** (1/beta) - 1.)
	    radius = sqrt ((dynrange) ** (1/beta) - 1)
	    seeing = sqrt ((psfrange) ** (1/beta) - 1)
	    nxm = max (NPROF, 1 + nint (radius*r*nxssub*nyssub))
	    call malloc (prof, nxm, TY_REAL)

	    dr = radius / (nxm - 1)
	    flux = 0
	    do i = 0, nxm - 1 {
		r = i * dr
		Memr[prof+i] =  1. / ((1 + r**2) ** beta)
		flux = flux + r * Memr[prof+i]
	    }

	    # Compute the fraction of the total flux in the profile.
	    # The last part of the total flux below is computed by expanding
	    # (1+r**2) --> r**2 under the approximation that r >> 1.
	    # Note that it is possible to explicitly compute the total
	    # flux  F(total) = beta / (2 * beta - 2) (CRC 53rd edition)
	    # I found errors in other versions of CRC for this integral!

	    r = r + dr / 2
	    xc = 2 * beta - 2
	    flux = flux / (flux + 1. / (xc * r ** xc))

	    r = sqrt (2. ** (1/beta) - 1.)
	    radius = radius / r
	    seeing = seeing / r
	} else ifnoerr (im = immap (name, READ_ONLY, 0)) {
	    iferr {
		nxm = IM_LEN(im,1)
		nym = IM_LEN(im,2)
		data = imgs2r (im, 1, nxm, 1, nym)
		call msiinit (msi, II_BILINEAR)
		call msifit (msi, Memr[data], nxm, nym, nxm)
	    } then
		call erract (EA_WARN)
	    call imunmap (im)

	    flux = 1.
	    radius = 1
	    seeing = 0.8
	} else ifnoerr (fd = open (name, READ_ONLY, TEXT_FILE)) {
	    nxm = NPROF
	    call malloc (prof1, nxm, TY_REAL)

	    j = 0
	    while (fscan (fd) != EOF) {
	 	call gargr (flux)
		if (nscan() < 1)
		    next
		if (j == nxm) {
		    nxm = nxm + NPROF
		    call realloc (prof1, nxm, TY_REAL)
		}
		Memr[prof1+j] = flux
		j = j + 1
	    }
	    call close (fd)
	    if (j == 0) {
		call mfree (prof1, TY_REAL)
		call error (1, "PSF template not found")
	    }

	    r = clgetr ("radius")
	    nxm = max (NPROF, 1 + nint (r*nxssub*nyssub))
	    call malloc (prof, nxm, TY_REAL)
	    dr = 1. / (nxm - 1)
	    j = j - 1

	    call asiinit (asi, II_SPLINE3)
	    call asifit (asi, Memr[prof1], j)
	    xc = Memr[prof1]
	    call mfree (prof1, TY_REAL)

	    if (xc == 0.) {
		flux = 0.
		do i = 1, nxm - 1 {
		    r = i * dr
		    call asider (asi, 1+j*r, der, 2)
		    Memr[prof+i] = max (0., der[2] / r)
		    flux = flux + r * Memr[prof+i]
		}
		Memr[prof] = max (0., 2 * Memr[prof+1] - Memr[prof+2])
	    } else {
		flux = 0.
		do i = 0, nxm - 1 {
		    r = i * dr
		    Memr[prof+i] = asieval (asi, 1+j*r)
		    flux = flux + r * Memr[prof+i]
		}
	    }
	    call asifree (asi)

	    xc = 0.9 * flux
	    flux = 0.
	    for (i=1; i<nxm && flux<xc; i=i+1)
		flux = flux + i * dr * Memr[prof+i]

	    flux = 1.
	    radius = 1.
	    seeing = (i - 1.) * dr
	} else
	    call error (1, "PSF template not found")

	# Set size and orientation parameters.
	r = clgetr ("radius")
	ar = clgetr ("ar")
	pa = DEGTORAD (clgetr ("pa"))
	radius = r * radius
	seeing = r * seeing

	# Compute templates with subsampling  over a grid of centers.
	# Do this for the full star profile and a smaller region for
	# convolving extended objects.

	# Seeing kernel.
	sym = stenter (stp, "seeing", 1)
	call calloc (Memi[sym], LEN_MKT, TY_STRUCT)
	mkt1 = Memi[sym]

	nx = 2 * nint (seeing) + 1
	ny = nx
	dxc = 1. / nxc
	dyc = 1. / nyc
	if (prof != NULL) {
	    nym = 1 + nint (radius * nxssub * nyssub)
	    call malloc (prof1, nym, TY_REAL)
	    call mkt_binprof (Memr[prof], nxm, Memr[prof1], nym, radius, nxssub)
	    for (yc = -0.5+dyc/2; yc < 0.5; yc = yc+dyc) {
	        for (xc = -0.5+dxc/2; xc < 0.5; xc = xc+dxc) {
		    call malloc (data, nx*ny, TY_REAL)
		    call mkt_profile (data, nx, ny, xc, yc, 1., Memr[prof1],
		        nym, radius, ar, pa, nxssub, nyssub)
		    call mkt_save (mkt1, data, nx, ny, xc, yc, 1., 0., 0., 0.,
			YES)
	        }
	    }
	} else {
	    for (yc = -0.5+dyc/2; yc < 0.5; yc = yc+dyc) {
	        for (xc = -0.5+dxc/2; xc < 0.5; xc = xc+dxc) {
		    call malloc (data, nx*ny, TY_REAL)
		    call mkt_msi (data, nx, ny, xc, yc, 1., msi, nxm, nym,
		        radius, ar, pa, nxssub, nyssub)
		    call mkt_save (mkt1, data, nx, ny, xc, yc, 1., 0., 0., 0.,
			YES)
	        }
	    }
	}

	# Full star templates.
	sym = stenter (stp, "star", 1)
	call calloc (Memi[sym], LEN_MKT, TY_STRUCT)
	mkt2 = Memi[sym]

	nx = 2 * nint (radius) + 1
	ny = nx
	dxc = 1. / nxc
	dyc = 1. / nyc
	if (prof != NULL) {
	    for (yc = 0.5+dyc/2; yc < 1.5; yc = yc+dyc) {
	        for (xc = 0.5+dxc/2; xc < 1.5; xc = xc+dxc) {
		    call malloc (data, nx*ny, TY_REAL)
		    call mkt_profile (data, nx, ny, xc, yc, flux, Memr[prof1],
		        nym, radius, ar, pa, nxssub, nyssub)
		    call mkt_save (mkt2, data, nx, ny, xc, yc, 1., 0., 0., 0.,
			YES)
	        }
	    }
	    call mfree (prof, TY_REAL)
	    call mfree (prof1, TY_REAL)
	} else {
	    for (yc = 0.5+dyc/2; yc < 1.5; yc = yc+dyc) {
	        for (xc = 0.5+dxc/2; xc < 1.5; xc = xc+dxc) {
		    call malloc (data, nx*ny, TY_REAL)
		    call mkt_msi (data, nx, ny, xc, yc, flux, msi, nxm, nym,
		        radius, ar, pa, nxssub, nyssub)
		    call mkt_save (mkt2, data, nx, ny, xc, yc, 1., 0., 0., 0.,
			YES)
	        }
	    }
	    call msifree (msi)
	}

	see = mkt1
	star = mkt2
	return (star)
end


# MKT_OBJECT -- Set object profiles.

pointer procedure mkt_object (name)

char	name[ARB]	# Profile name or file

int	i, j, n, nxm, nym, fd
real	radius, r, dr, s, b, flux, der[2]
pointer	sym, mkt, prof, asi, msi, buf, im

real	c3, c4, c5, c6, c7

real	asieval()
double	uigamma()
int	open(), fscan(), nscan(), strncmp(), ctor()
pointer	immap(), imgs2r(), stfind(), stenter()
bool	streq()
errchk	open, immap, asifit, asieval, asider

include	"mktemplates.com"

begin
	# Check if previously defined.
	sym = stfind (stp, name)
	if (sym != NULL)
	    return (Memi[sym])

	# Select type of profile and set intensity profile array.
	# Compute the fraction of the total flux in the profile.

	prof = NULL
	msi = NULL
	if (strncmp (name, "sersic", 6) == 0) {
	    i = 7
	    if (ctor (name, i, s) == 0) {
		call eprintf ("WARNING: Bad sersic profile syntax (%s).\n")
		    call pargstr (name)
		return (NULL)
	    }
	    n = nint (2 * (s + 0.01))
	    s = n / 2.
	    if (n < 1 || n > 20) {
		call eprintf (
		    "WARNING: Sersic index out of allowed range (%.1f).\n")
		    call pargi (s)
		return (NULL)
	    }
	    nxm = NPROF
	    call malloc (prof, nxm, TY_REAL)
	    radius = log (dynrange) ** s
	    dr = radius / (nxm - 1)
	    do i = 0, nxm - 1 {
		r = (i * dr) ** (1/s)
		Memr[prof+i] = exp (-r)
	    }

	    flux = 1 - uigamma (n, r)
	    r = n - 1./3. + 4./(405.*s) + 46./(25515.*s*s)
	    radius = radius / r ** s
	} else if (streq (name, "expdisk")) {
	    s = 1.
	    n = nint (2 * s)

	    nxm = NPROF
	    call malloc (prof, nxm, TY_REAL)
	    radius = log (dynrange) ** s
	    dr = radius / (nxm - 1)
	    do i = 0, nxm - 1 {
		r = (i * dr) ** (1/s)
		Memr[prof+i] = exp (-r)
	    }

	    flux = 1 - uigamma (n, r)
	    r = n - 1./3. + 4./(405.*s) + 46./(25515.*s*s)
	    radius = radius / r ** s
	} else if (streq (name, "devauc")) {
	    s = 4.
	    n = nint (2 * s)
	    nxm = NPROF
	    call malloc (prof, nxm, TY_REAL)
	    radius = log (dynrange) ** s
	    dr = radius / (nxm - 1)
	    do i = 0, nxm - 1 {
		r = (i * dr) ** (1/s)
		Memr[prof+i] = exp (-r)
	    }

	    flux = 1 - uigamma (n, r)
	    r = n - 1./3. + 4./(405.*s) + 46./(25515.*s*s)
	    radius = radius / r ** s
	} else ifnoerr (im = immap (name, READ_ONLY, 0)) {
	    iferr {
		nxm = IM_LEN(im,1)
		nym = IM_LEN(im,2)
		buf = imgs2r (im, 1, nxm, 1, nym)
		call msiinit (msi, II_BILINEAR)
		call msifit (msi, Memr[buf], nxm, nym, nxm)
	    } then
		call erract (EA_WARN)
	    call imunmap (im)

	    flux = 1.
	    radius = 1.
	} else ifnoerr (fd = open (name, READ_ONLY, TEXT_FILE)) {
	    nxm = NPROF
	    call malloc (buf, nxm, TY_REAL)

	    j = 0
	    while (fscan (fd) != EOF) {
	 	call gargr (flux)
		if (nscan() < 1)
		    next
		if (j == nxm) {
		    nxm = nxm + NPROF
		    call realloc (buf, nxm, TY_REAL)
		}
		Memr[buf+j] = flux
		j = j + 1
	    }
	    call close (fd)
	    if (j == 0) {
		call mfree (buf, TY_REAL)
		nxm = 0
		call error (1, "PSF template not found")
	    }

	    nxm = NPROF
	    call malloc (prof, nxm, TY_REAL)
	    dr = 1. / (nxm - 1)
	    j = j - 1

	    call asiinit (asi, II_SPLINE3)
	    call asifit (asi, Memr[buf], j)
	    s = Memr[buf]
	    call mfree (buf, TY_REAL)

	    if (s == 0.) {
		do i = 1, nxm - 1 {
		    r = i * dr
		    call asider (asi, 1+j*r, der, 2)
		    Memr[prof+i] = max (0., der[2] / r)
		}
		Memr[prof] = max (0., 2 * Memr[prof+1] - Memr[prof+2])
	    } else {
		do i = 0, nxm - 1 {
		    r = i * dr
		    Memr[prof+i] = asieval (asi, 1+j*r)
		}
	    }
	    call asifree (asi)

	    flux = 1.
	    radius = 1.
	} else {
	    call eprintf ("WARNING: Object template %s not found.\n")
		call pargstr (name)
	    return (NULL)
	}

	# Create the template structure if a model is defined..
	if (prof == NULL && msi == NULL)
	    mkt = NULL
	else {
	    call calloc (mkt, LEN_MKT, TY_STRUCT)
	    MKT_PROF(mkt) = prof
	    MKT_MSI(mkt) = msi
	    MKT_NXM(mkt) = nxm
	    MKT_NYM(mkt) = nym
	    MKT_F(mkt) = flux
	    MKT_SCALE(mkt) = radius
#call eprintf ("flux = %g, radius = %g\n")
#call pargr (MKT_F(mkt))
#call pargr (MKT_SCALE(mkt))
#do i = 0, nxm {
#call eprintf ("%d: %g\n")
#call pargi (i)
#call pargr (Memr[prof+i])
#}
	}

	# Enter object model name in symbol table.
	sym = stenter (stp, name, 1)
	Memi[sym] = mkt
	return (mkt)
end


# MKT_GOBJECT -- Get image raster.
# The profile type is set by the template structure.

procedure mkt_gobject (mkt, data, nx, ny, x, y, z, r, ar, pa, save)

pointer	mkt		# Object template
pointer	data		# Data
int	nx, ny		# Size of returned data
real	x, y		# Position of object
real	z		# Flux of object
real	r		# Major axis scale (pixels)
real	ar		# Axial ratio (minor / major)
real	pa		# Position angle (radians)
int	save		# Use/save template?

real	xc, yc, radius
int	nprof
pointer	prof

include	"mktemplates.com"

begin
	data = NULL
	if (mkt == NULL)
	    return

	# Stars are predefined.  Return the nearest template center.
	# Other objects are computed with or without seeing.

	if (mkt == star)
	    call mkt_gstar (star, data, nx, ny, x, y, z)
	else {
	    xc = x + 0.5 - int (x + 0.5)
	    yc = y + 0.5 - int (y + 0.5)
	    call mkt_get (mkt, data, nx, ny, xc, yc, z, r, ar, pa, save)
	    if (data != NULL)
		return

	    radius = r * MKT_SCALE(mkt)
	    nx = max (3, 2 * nint (radius) + 1)
	    ny = nx
	    if (see != NULL) {
	        nx = nx + Memi[MKT_NX(see)] / 2
	        ny = ny + Memi[MKT_NY(see)] / 2
	    }
	    call malloc (data, nx * ny, TY_REAL)

	    if (MKT_PROF(mkt) != 0) {
	        nprof = 1 + nint (radius * nxgsub * nxgsub)
	        call malloc (prof, nprof, TY_REAL)
	        call mkt_binprof (Memr[MKT_PROF(mkt)], MKT_NXM(mkt),
		    Memr[prof], nprof, radius, nxgsub)
	        call mkt_profile (data, nx, ny, x, y, z*MKT_F(mkt), Memr[prof],
		    nprof, radius, ar, pa, nxgsub, nygsub)
	        call mfree (prof, TY_REAL)
	    } else {
	        call mkt_msi (data, nx, ny, x, y, z, MKT_MSI(mkt),
		    MKT_NXM(mkt), MKT_NYM(mkt), radius, ar, pa,
		    nxgsub, nygsub)
	    }

	    call mkt_save (mkt, data, nx, ny, xc, yc, z, r, ar, pa, save)
	}
end


# MKT_BINPROF -- Bin intensity profile into subpixels

procedure mkt_binprof (prof, nprof, prof1, nprof1, radius, nsub)

real	prof[nprof]		# Input intensity profile
int	nprof			# Number of input points
real	prof1[nprof]		# Output binned intensity profile
int	nprof1			# Number of output points
real	radius			# Radius of profile
int	nsub			# Maximum subsampling

int	i, j, k, k1, k2, l, dx
real	scale, dy, val

int	debug, open()
data	debug/0/

begin
	if (radius < 0.1) {
	    call amovkr (1., prof1, nprof1)
	    return
	} else
	    call aclrr (prof1, nprof1)

	# Set binning parameters
	scale = (nprof - 1.) / (nprof1 - 1.)
	dx = nint ((nprof1 - 1.) / nsub / radius / 2.)
	dy = dx / (NY - 1.)

	# Bin central pixels
	do i = -dx, 2*dx {
	    k = abs (i)
	    k1 = max (1, i - dx + 1) 
	    k2 = i + dx + 1
	    do j = 0, NY-1 {
		if (j == 0)
		    val = k
		else if (k == 0)
		    val = dy * j
		else
		    val = k * sqrt (1. + (dy * j / k) ** 2)
		l = nint (scale * val + 1)
		if (l > nprof)
		    next
		val = prof[l] / NY
		do l = k1, k2
		    prof1[l] = prof1[l] + val
	    }
	}

	# Now bin remainder of pixels more crudely
	do i = 2*dx+1, nprof1-1 {
	    k1 = i - dx + 1
	    k2 = min (nprof1, i + dx + 1)
	    val = prof[nint (scale * i + 1)]
	    do k = k1, k2
		prof1[k] = prof1[k] + val
	}

	if (debug == YES) {
	    j = open ("debug1.dat", APPEND, TEXT_FILE)
	    do i = 1, nprof {
		call fprintf (j, "%d %g\n")
		    call pargi (i)
		    call pargr (prof[i])
	    }
	    call close (j)

	    j = open ("debug2.dat", APPEND, TEXT_FILE)
	    do i = 1, nprof1 {
		call fprintf (j, "%d %g\n")
		    call pargi (i)
		    call pargr (prof1[i])
	    }
	    call close (j)
	}
end


# MKT_GSTAR -- Get the precomputed template with center nearest (x,y).

procedure mkt_gstar (mkt, data, nx, ny, x, y, z)

pointer	mkt		# Template
pointer	data		# Data
int	nx, ny		# Size of data
real	x, y		# Position of object
real	z		# Flux of object

int	i, j
real	f

include	"mktemplates.com"

begin
	i = (x + 0.5 - int (x + 0.5)) * nxc
	j = (y + 0.5 - int (y + 0.5)) * nyc
	i = j * nxc + i
	data = Memi[MKT_PTRS(mkt)+i]
	nx = Memi[MKT_NX(mkt)+i]
	ny = Memi[MKT_NY(mkt)+i]
	f = Memr[MKT_FLUX(mkt)+i]
	if (f != z) {
	    call amulkr (Memr[data], z/f, Memr[data], nx*ny)
	    Memr[MKT_FLUX(mkt)+i] = z
	}
end


# MKT_PROFILE -- Make template from profile.

procedure mkt_profile (data, nx, ny, xc, yc, flux, prof, npts, radius,
	ar, pa, nxsub, nysub)

pointer	data		# Pointer to data array
int	nx, ny		# Size of template
real	xc, yc		# Model center
real	flux		# Model flux
real	prof[npts]	# 1D profile
int	npts		# Number of points in profile
real	radius		# Major axis radius of profile (pixels)
real	ar		# Axial ratio (minor / major)
real	pa		# Position angle relative to major axis (radians)
int	nxsub, nysub	# Number of subpixel samples

int	i, n, nxs, nys, nxs2, nys2, xs1, xs2, ys1, ys2
int	x, y, x2, y2
real	a, b, c, r, s, t, z, sum, sum1, asumr()
real	dx, dy, dsub, dsub2
real	x1, y1, xc1, yc1
pointer	ptr, ptr1, ptr2, see1, see2

include	"mktemplates.com"

define	see_	99

begin
	# Switch on the size of the seeing templates.
	if (see != NULL)
	    if (Memi[MKT_NX(see)] * Memi[MKT_NY(see)] > 1)
	        goto see_

# NO SEEING:

	# If the radius is very small return delta function.
	if (radius < 0.1) {
	    call aclrr (Memr[data], nx*ny)
	    Memr[data+(ny/2)*nx+(nx/2)] = flux
	    return
	}

	# Compute elliptical scale factors for entry into profile array.
	r = ((npts - 1) / radius) ** 2
	t = ((npts - 1) / (ar * radius)) ** 2
	c = cos (pa)
	s = sin (pa)
	a = r * c * c + t * s * s
	b = 2 * (r - t) * c * s
	c = r * s * s + t * c * c

	# Subsample the profile and sum into each pixel.
	n = nx * ny
	xc1 = xc - int (xc + 0.5) + nx/2
	yc1 = yc - int (yc + 0.5) + ny/2

	sum1 = 0.
	ptr = data
	do y = 0, ny-1 {
	    t = (y - yc1) ** 2
	    do x = 0, nx-1 {
		i = sqrt ((x - xc1) ** 2 + t)
		dsub = 1. / max (1, nxsub - i)
		sum = 0.
		for (y1 = y-0.5+dsub/2; y1 < y+0.5; y1 = y1+dsub) {
		    dy = (y1 - yc1)
		    s = c * dy**2
		    for (x1=x-0.5+dsub/2; x1<x+0.5; x1=x1+dsub) {
			dx = (x1-xc1)
			i = sqrt (a * dx**2 + b * dx * dy + s) + 1.5
			if (i <= npts)
			    sum = sum + prof[i]
		    }
		}
		sum = sum * dsub ** 2
		sum1 = sum1 + sum
		Memr[ptr] = sum
		ptr = ptr + 1
	    }
	}

	# If the subsamples completely miss and signal is zero then return
	# delta function otherwise scale to requested flux.

	if (sum1 == 0.)
	    Memr[data+(ny/2)*nx+(nx/2)] = flux
	else
	    call amulkr (Memr[data], flux/sum1, Memr[data], n)
	return

# WITH SEEING:

see_	n = nx * ny
	call aclrr (Memr[data], n)
	sum = 0.

	nxs = Memi[MKT_NX(see)]
	nys = Memi[MKT_NY(see)]
	nxs2 = nxs/2
	nys2 = nys/2

	# If the profile is very small return full star image rather than
	# convolution with truncated seeing template.

	if (radius > 0.01) {
	    r = ((npts - 1) / radius) ** 2
	    t = ((npts - 1) / (ar * radius)) ** 2
	    c = cos (pa)
	    s = sin (pa)
	    a = r * c * c + t * s * s
	    b = 2 * (r - t) * c * s
	    c = r * s * s + t * c * c
    
	    xc1 = xc - int (xc + 0.5) + nx/2
	    yc1 = yc - int (yc + 0.5) + ny/2
	    ptr = data-nys2*nx-nxs2
	    do y = 0, ny-1 {
		t = (y - yc1) ** 2
		ys1 = max (0, nys2 - y)
		ys2 = min (nys-1, ny - y + nys2 - 1)
		do x = 0, nx-1 {
		    i = sqrt ((x - xc1) ** 2 + t)
		    dsub = 1. / max (1, nxsub - i)
		    dsub2 = dsub ** 2
		    xs1 = max (0, nxs2 - x)
		    xs2 = min (nxs-1, nx - x + nxs2 - 1)
		    ptr1 = ptr + xs1
		    for (y1=y-0.5+dsub/2; y1<y+0.5; y1=y1+dsub) {
			dy = (y1 - yc1)
			s = c * dy**2
			for (x1=x-0.5+dsub/2; x1<x+0.5; x1=x1+dsub) {
			    dx = (x1-xc1)
			    i = sqrt (a * dx**2 + b * dx * dy + s) + 1.5
			    if (i <= npts) {
				z = prof[i] * dsub2
				call mkt_gstar (see, see1, nxs, nys, x1, y1, 1.)
				see1 = see1 + xs1
				do y2 = ys1, ys2 {
				   see2 = see1+y2*nxs
				   ptr2 = ptr1+y2*nx
				   do x2 = xs1, xs2 {
				       Memr[ptr2] = Memr[ptr2] +
					   z * Memr[see2]
				       ptr2 = ptr2 + 1
				       see2 = see2 + 1
				    }
				}
			    }
			}
		    }
		    ptr = ptr + 1
		}
	    }
	    sum = asumr (Memr[data], n)
	}

	# If no flux is accumulated or radius is very small return star image.
	# Otherwise scale to requested flux.

	if (sum == 0.) {
	    call mkt_gstar (star, see1, nxs, nys, xc, yc, flux)
	    x = nx/2
	    y = ny/2
	    nxs2 = nxs / 2
	    nys2 = nys / 2
	    xs1 = max (0, nxs2 - x)
	    xs2 = min (nxs-1, nx - x + nxs2 - 1)
	    ys1 = max (0, nys2 - y)
	    ys2 = min (nys-1, ny - y + nys2 - 1)
	    ptr1 = data-nys2*nx-nxs2+(y*nx+x+xs1)
	    see1 = see1 + xs1
	    do y2 = ys1, ys2 {
	       see2 = see1+y2*nxs
	       ptr2 = ptr1+y2*nx
	       do x2 = xs1, xs2 {
		   Memr[ptr2] = Memr[ptr2] + Memr[see2]
		   ptr2 = ptr2 + 1
		   see2 = see2 + 1
		}
	    }
	} else
	    call amulkr (Memr[data], flux/sum, Memr[data], n)
end


# MKT_MSI -- Make template from image interpolation function.

procedure mkt_msi (data, nx, ny, xc, yc, flux, model, nxm, nym, radius,
	ar, pa, nxsub, nysub)

pointer	data		# Pointer to data array
int	nx, ny		# Size of template
real	xc, yc		# Model center
real	flux		# Model flux
pointer	model		# Surface interpolation pointer for image template
int	nxm, nym	# Number of points in image template
real	radius		# Major axis radius of profile (pixels)
real	ar		# Axial ratio (minor / major)
real	pa		# Position angle relative to major axis (radians)
int	nxsub, nysub	# Number of subpixel samples

int	i, n, nxs, nys, nxs2, nys2, xs1, xs2, ys1, ys2, x, y, x2, y2
real	a, b, c, s, xcm, ycm, x1, y1, xc1, yc1, dsub, sum, sum1
real	ac, as, bc, bs, acdx1, acdx2, bsdx1, bsdx2, bcdy1, bcdy2, asdy1, asdy2
real	val1, val2, val3, val4, minval, maxval, xm[5], ym[5]
real	asumr(), msigrl(), msisqgrl()
pointer	ptr, ptr1, ptr2, see1, see2

include	"mktemplates.com"

define	see_	99

begin
	# Switch on the size of the seeing templates.
	if (see != NULL)
	    if (Memi[MKT_NX(see)] * Memi[MKT_NY(see)] > 1)
	        goto see_

# NO SEEING:

	# If the radius is very small return delta function.
	if (radius < 0.1) {
	    call aclrr (Memr[data], nx*ny)
	    Memr[data+(ny/2)*nx+(nx/2)] = flux
	    return
	}

	a = (nxm / 2.) / radius
	b = (nym / 2.) / (ar * radius)
	c = cos (pa)
	s = sin (pa)
	ac = a * c
	as = a * s
	bc = b * c
	bs = b * s
	a = nxm
	b = nym
	xcm = nxm / 2 + 1.
	ycm = nym / 2 + 1.

	# Subsample the profile and sum into each pixel.
	n = nx * ny
	xc1 = xc - int (xc + 0.5) + nx/2
	yc1 = yc - int (yc + 0.5) + ny/2

	sum1 = 0.
	ptr = data
	do y = 0, ny-1 {
	    c = (y - yc1) ** 2
	    do x = 0, nx-1 {
		i = sqrt ((x - xc1) ** 2 + c)
		dsub = 1. / max (1, nxsub - i)
		sum = 0.
		for (y1 = y-0.5; y1 < y+0.5-dsub/2; y1 = y1+dsub) {
		    asdy1 = (y1 - yc1)
		    asdy2 = asdy1 + dsub
		    bcdy1 = bc * asdy1 + ycm
		    bcdy2 = bc * asdy2 + ycm
		    if (pa == 0.) {
			val3 = max (1., bcdy1)
			if (val3 >= nym)
			    next
			val4 = min (b, bcdy2)
			if (val3 >= val4)
			    next
		    }
		    asdy1 = as * asdy1
		    asdy2 = as * asdy2
		    for (x1=x-0.5; x1<x+0.5-dsub/2; x1=x1+dsub) {
			bsdx1 = (x1-xc1)
			bsdx2 = bsdx1 + dsub
			acdx1 = ac * bsdx1 + xcm
			acdx2 = ac * bsdx2 + xcm
			if (pa == 0.) {
			    val1 = max (1., acdx1)
			    if (val1 >= nxm)
				next
			    val2 = min (a, acdx2)
			    if (val1 >= val2)
				next
			    s = msisqgrl (model, val1, val2, val3, val4)
			} else {
			    bsdx1 = bs * bsdx1
			    bsdx2 = bs * bsdx2

			    val1 = acdx1 + asdy1
			    val2 = acdx2 + asdy1
			    val3 = acdx2 + asdy2
			    val4 = acdx1 + asdy2
			    minval = min (val1, val2, val3, val4)
			    maxval = max (val1, val2, val3, val4)
			    if (minval >= a || maxval <= 1.)
				next
			    xm[1] = max (1., min (a, val1))
			    xm[2] = max (1., min (a, val2))
			    xm[3] = max (1., min (a, val3))
			    xm[4] = max (1., min (a, val4))
			    xm[5] = xm[1]

			    val1 = bcdy1 - bsdx1
			    val2 = bcdy1 - bsdx2
			    val3 = bcdy2 - bsdx2
			    val4 = bcdy2 - bsdx1
			    minval = min (val1, val2, val3, val4)
			    maxval = max (val1, val2, val3, val4)
			    if (minval >= b || maxval <= 1.)
				next
			    ym[1] = max (1., min (b, val1))
			    ym[2] = max (1., min (b, val2))
			    ym[3] = max (1., min (b, val3))
			    ym[4] = max (1., min (b, val4))
			    ym[5] = ym[1]
			    y2 = 1
			    do x2 = 2, 4
				if (ym[x2] < ym[y2])
				    y2 = x2
			    switch (y2) {
			    case 2:
				xm[1] = xm[2]; ym[1] = ym[2]
				xm[2] = xm[3]; ym[2] = ym[3]
				xm[3] = xm[4]; ym[3] = ym[4]
				xm[4] = xm[5]; ym[4] = ym[5]
				xm[5] = xm[1]; ym[5] = ym[1]
			    case 3:
				xm[1] = xm[3]; ym[1] = ym[3]
				xm[3] = xm[5]; ym[3] = ym[5]
				xm[5] = xm[2]; ym[5] = ym[2]
				xm[2] = xm[4]; ym[2] = ym[4]
				xm[4] = xm[5]; ym[4] = ym[5]
				xm[5] = xm[1]; ym[5] = ym[1]
			    case 4:
				xm[5] = xm[4]; ym[5] = ym[4]
				xm[4] = xm[3]; ym[4] = ym[3]
				xm[3] = xm[2]; ym[3] = ym[2]
				xm[2] = xm[1]; ym[2] = ym[1]
				xm[1] = xm[5]; ym[1] = ym[5]
			    }

			    s = msigrl (model, xm, ym, 5)
			}
			sum = sum + s
		    }
		}
		sum1 = sum1 + sum
		Memr[ptr] = sum
		ptr = ptr + 1
	    }
	}

	call amulkr (Memr[data], flux/sum1, Memr[data], n)
	return

# SEEING:

see_	n = nx * ny
	call aclrr (Memr[data], n)
	sum = 0.

	nxs = Memi[MKT_NX(see)]
	nys = Memi[MKT_NY(see)]
	nxs2 = nxs/2
	nys2 = nys/2

	# If the profile is very small return full star image rather than
	# convolution with truncated seeing template.

	if (radius > 0.01) {
	    a = (nxm / 2.) / radius
	    b = (nym / 2.) / (ar * radius)
	    c = cos (pa)
	    s = sin (pa)
	    ac = a * c
	    as = a * s
	    bc = b * c
	    bs = b * s
	    a = nxm
	    b = nym
	    xcm = nxm / 2 + 1.
	    ycm = nym / 2 + 1.
    
	    xc1 = xc - int (xc + 0.5) + nx/2
	    yc1 = yc - int (yc + 0.5) + ny/2
	    ptr = data-nys2*nx-nxs2
	    do y = 0, ny-1 {
		c = (y - yc1) ** 2
		ys1 = max (0, nys2 - y)
		ys2 = min (nys-1, ny - y + nys2 - 1)
		do x = 0, nx-1 {
		    i = sqrt ((x - xc1) ** 2 + c)
		    dsub = 1. / max (1, nxsub - i)
		    xs1 = max (0, nxs2 - x)
		    xs2 = min (nxs-1, nx - x + nxs2 - 1)
		    ptr1 = ptr + xs1
		    for (y1=y-0.5; y1<y+0.5-dsub/2; y1=y1+dsub) {
			asdy1 = (y1 - yc1)
			asdy2 = asdy1 + dsub
			bcdy1 = bc * asdy1 + ycm
			bcdy2 = bc * asdy2 + ycm
		        if (pa == 0.) {
			    val3 = max (1., bcdy1)
			    if (val3 >= nym)
			        next
			    val4 = min (b, bcdy2)
			    if (val3 >= val4)
			        next
		        }
			asdy1 = as * asdy1
			asdy2 = as * asdy2
		        for (x1=x-0.5; x1<x+0.5-dsub/2; x1=x1+dsub) {
			    bsdx1 = (x1-xc1)
			    bsdx2 = bsdx1 + dsub
			    acdx1 = ac * bsdx1 + xcm
			    acdx2 = ac * bsdx2 + xcm
			    if (pa == 0.) {
			        val1 = max (1., acdx1)
			        if (val1 >= nxm)
				    next
			        val2 = min (a, acdx2)
			        if (val1 >= val2)
				    next
			        sum = msisqgrl (model, val1, val2, val3, val4)
			    } else {
			        bsdx1 = bs * bsdx1
			        bsdx2 = bs * bsdx2

			        val1 = acdx1 + asdy1
			        val2 = acdx2 + asdy1
			        val3 = acdx2 + asdy2
			        val4 = acdx1 + asdy2
			        minval = min (val1, val2, val3, val4)
			        maxval = max (val1, val2, val3, val4)
			        if (minval >= a || maxval <= 1.)
				    next
			        xm[1] = max (1., min (a, val1))
			        xm[2] = max (1., min (a, val2))
			        xm[3] = max (1., min (a, val3))
			        xm[4] = max (1., min (a, val4))
			        xm[5] = xm[1]

			        val1 = bcdy1 - bsdx1
			        val2 = bcdy1 - bsdx2
			        val3 = bcdy2 - bsdx2
			        val4 = bcdy2 - bsdx1
			        minval = min (val1, val2, val3, val4)
			        maxval = max (val1, val2, val3, val4)
			        if (minval >= b || maxval <= 1.)
				    next
			        ym[1] = max (1., min (b, val1))
			        ym[2] = max (1., min (b, val2))
			        ym[3] = max (1., min (b, val3))
			        ym[4] = max (1., min (b, val4))
			        ym[5] = ym[1]

# The following is put in to work around a bug in msigrl in V2.8.  When
# V2.8 is long gone we can take this stuff out since msigrl will do the
# rotating of the coordinates itself.

				minval = max (1., minval)
				y2 = 1
				do x2 = 2, 4
				    if (ym[x2] < ym[y2])
					y2 = x2
				switch (y2) {
				case 2:
				    xm[1] = xm[2]; ym[1] = ym[2]
				    xm[2] = xm[3]; ym[2] = ym[3]
				    xm[3] = xm[4]; ym[3] = ym[4]
				    xm[4] = xm[5]; ym[4] = ym[5]
				    xm[5] = xm[1]; ym[5] = ym[1]
				case 3:
				    xm[1] = xm[3]; ym[1] = ym[3]
				    xm[3] = xm[5]; ym[3] = ym[5]
				    xm[5] = xm[2]; ym[5] = ym[2]
				    xm[2] = xm[4]; ym[2] = ym[4]
				    xm[4] = xm[5]; ym[4] = ym[5]
				    xm[5] = xm[1]; ym[5] = ym[1]
				case 4:
				    xm[5] = xm[4]; ym[5] = ym[4]
				    xm[4] = xm[3]; ym[4] = ym[3]
				    xm[3] = xm[2]; ym[3] = ym[2]
				    xm[2] = xm[1]; ym[2] = ym[1]
				    xm[1] = xm[5]; ym[1] = ym[5]
				}

			        sum = msigrl (model, xm, ym, 5)
			    }
	    		    call mkt_gstar (see, see1, nxs, nys, x1, y1, 1.)
			    see1 = see1 + xs1
			    do y2 = ys1, ys2 {
				see2 = see1+y2*nxs
				ptr2 = ptr1+y2*nx
				do x2 = xs1, xs2 {
				    Memr[ptr2] = Memr[ptr2] + sum * Memr[see2]
				    ptr2 = ptr2 + 1
				    see2 = see2 + 1
				}
			    }
			}
		    }
		    ptr = ptr + 1
		}
	    }
	    sum = asumr (Memr[data], n)
	}

	# If no flux is accumulated or radius is very small return star image.
	# Otherwise scale to requested flux.

	if (sum == 0.) {
	    call mkt_gstar (star, see1, nxs, nys, xc, yc, flux)
	    x = nx/2
	    y = ny/2
	    nxs2 = nxs / 2
	    nys2 = nys / 2
	    xs1 = max (0, nxs2 - x)
	    xs2 = min (nxs-1, nx - x + nxs2 - 1)
	    ys1 = max (0, nys2 - y)
	    ys2 = min (nys-1, ny - y + nys2 - 1)
	    ptr1 = data-nys2*nx-nxs2+(y*nx+x+xs1)
	    see1 = see1 + xs1
	    do y2 = ys1, ys2 {
	       see2 = see1+y2*nxs
	       ptr2 = ptr1+y2*nx
	       do x2 = xs1, xs2 {
		   Memr[ptr2] = Memr[ptr2] + Memr[see2]
		   ptr2 = ptr2 + 1
		   see2 = see2 + 1
		}
	    }
	} else
	    call amulkr (Memr[data], flux/sum, Memr[data], n)
end


# UIGAMMA -- Upper Incomplete Gamma Function ratioed to Complete.
#
#	uigamma(n,x) = e^(x) sum (x^k/k!) for k=0, n-1

double procedure uigamma (n, x)

int	n		#I argument
real	x		#I argument

int	i
double	uigamma, numerator, denominator

begin
	numerator = exp(-x)
	denominator = 1
	uigamma = numerator / denominator
	do i = 1, n-1 {
	    numerator = numerator * x
	    denominator = denominator * i
	    uigamma = uigamma + numerator / denominator
	}
	return (uigamma)
end
