include	<error.h>
include <imhdr.h>
include	<imio.h>
include	<math/gsurfit.h>

# Data structure.
define	MGS_SZNAME	99		# Length of mgs name string
define	MGS_LEN		56		# Length of structure
define	MGS_GS		Memi[$1]	# GSURFIT pointer
define	MGS_X		Memi[$1+1]	# Pointer to line of x values
define	MGS_Y		Memi[$1+2]	# Pointer to line of y values
define	MGS_Z		Memi[$1+3]	# Pointer to line of z values
define	MGS_NC		Memi[$1+4]	# Number of columns
define	MGS_REFIM	Memi[$1+5]	# Reference image pointer
define	MGS_NAME	Memc[P2C($1+6)]	# Map name


# MGS_GLR -- Get a line of data.

pointer procedure mgs_glr (mgs, line)

pointer	mgs		#I Map pointer
int	line		#I Line

int	nc
pointer	x, y, z, gs

begin
	if (mgs == NULL)
	    call error (1, "Map is undefined")

	gs = MGS_GS(mgs)
	x = MGS_X(mgs)
	y = MGS_Y(mgs)
	z = MGS_Z(mgs)
	nc = MGS_NC(mgs)

	call amovkr (real(line), Memr[y], nc)
	call gsvector (gs, Memr[x], Memr[y], Memr[z], nc)

	return (z)
end


# MGS_OPEN -- Open mgs.

pointer procedure mgs_open (name, refim, gsin)

char	name[ARB]	#I Name
pointer	refim		#I Reference image
pointer	gsin		#I GSURFIT pointer
pointer	mgs		#O Map pointer returned

int	i, nc, nl
real	gsgetr()
pointer	gs
errchk	mgs_ggs

begin
	nc = IM_LEN(refim,1)
	nl = IM_LEN(refim,2)

	call calloc (mgs, MGS_LEN, TY_STRUCT)
	MGS_REFIM(mgs) = refim
	call strcpy (name, MGS_NAME(mgs), MGS_SZNAME)
	MGS_NC(mgs) = nc

	iferr {
	    gs = gsin
	    if (gs == NULL) {
		call mgs_ggs (refim, name, gs)
		MGS_GS(mgs) = gs
	    }

	    if (1 < gsgetr (gs, GSXMIN) || nc > gsgetr (gs, GSXMAX) ||
		1 < gsgetr (gs, GSYMIN) || nl > gsgetr (gs, GSYMAX))
		call error (2, "Map and data images have different sizes")

	    MGS_GS(mgs) = gs
	    call malloc (MGS_X(mgs), nc, TY_REAL)
	    call malloc (MGS_Y(mgs), nc, TY_REAL)
	    call malloc (MGS_Z(mgs), nc, TY_REAL)
	    do i = 1, nc
		Memr[MGS_X(mgs)+i-1] = i
	} then {
	    call mgs_close (mgs)
	    call erract (EA_ERROR)
	}

	return (mgs)
end


# MGS_CLOSE -- Close mgs.

procedure mgs_close (mgs)

pointer	mgs			#I Map pointer

begin
	if (mgs == NULL)
	    return

	if (MGS_GS(mgs) != NULL)
	    call gsfree (MGS_GS(mgs))
	call mfree (MGS_X(mgs), TY_REAL)
	call mfree (MGS_Y(mgs), TY_REAL)
	call mfree (MGS_Z(mgs), TY_REAL)
	call mfree (mgs, TY_STRUCT)
end


# MGS_GETS -- Get string parameter.

procedure mgs_gets (mgs, param, val, maxchar)

pointer	mgs		#I Map pointer
char	param[ARB]	#I Parameter
char	val[ARB]	#O Parameter string value 
int	maxchar		#I Maximum number of characters to return

begin
	call error (1, "mgs_gets: unknown parameter")
end


# MGS_SETS -- Set string parameter.

procedure mgs_sets (mgs, param, val)

pointer	mgs		#I Map pointer
char	param[ARB]	#I Parameter
char	val[ARB]	#O Parameter string value 

begin
	call error (1, "mgs_sets: unknown parameter")
end


# MGS_GETI -- Get integer parameter.

procedure mgs_geti (mgs, param, val)

pointer	mgs		#I Map pointer
char	param[ARB]	#I Parameter
int	val		#O Value

bool	streq()

begin
	if (streq (param, "gsurfit"))
	    val = MGS_GS(mgs)
	else
	    call error (1, "mgs_geti: unknown parameter")
end


# MGS_SETI -- Set integer parameter.

procedure mgs_seti (mgs, param, val)

pointer	mgs		#I Map pointer
char	param[ARB]	#I Parameter
int	val		#I Value

bool	streq()

begin
	if (streq (param, "gsurfit")) {
	    call mgs_pgs (MGS_REFIM(mgs), MGS_NAME(mgs), val)
	    call gsfree (MGS_GS(mgs))
	    MGS_GS(mgs) = val
	} else
	    call error (1, "mgs_seti: unknown parameter")
end


# MGS_GETR -- Get real parameter.

procedure mgs_getr (mgs, param, val)

pointer	mgs		#I Map pointer
char	param[ARB]	#I Parameter
real	val		#O Value

begin
	call error (1, "mgs_getr: unknown parameter")
end


# MGS_SETR -- Set real parameter.

procedure mgs_setr (mgs, param, val)

pointer	mgs		#I Map pointer
char	param[ARB]	#I Parameter
real	val		#I Value

begin
	call error (1, "mgs_setr: unknown parameter")
end


# MAP_PGS -- Put mgs surface fit.

procedure mgs_pgs (im, key, gs)

pointer	im		#I Image pointer
char	key[ARB]	#I Keyword root
pointer	gs		#I Surface fit pointer

int	i, nc, fd, gsgeti(), stropen()
pointer	sp, kw, card, coeffs, strbuf, cp, cp1, cp2

begin
	if (IM_SECTUSED(im) == YES)
	    return

	call smark (sp)
	call salloc (kw, 80, TY_CHAR)
	call salloc (card, 68, TY_CHAR)

	nc = gsgeti (gs, GSNSAVE)
	call salloc (coeffs, nc, TY_REAL)
	call gssave (gs, Memr[coeffs])

	# Convert coeffs to a string.  Last character will be space.
	call salloc (strbuf, 20*nc, TY_CHAR)
	call aclrc (Memc[strbuf], 20*nc)
	fd = stropen (Memc[strbuf], 20*nc, WRITE_ONLY)
	do i = 1, nc {
	    call fprintf (fd, "%g ")
		call pargr (Memr[coeffs+i-1])
	}
	call close (fd)

	i = 1
	cp1 = strbuf
	for (cp=cp1; Memc[cp] != EOS; cp=cp+1) {
	    if (Memc[cp] == ' ')
		cp2 = cp
	    if (cp - cp1 + 1 == 68) {
		call sprintf (Memc[kw], 8, "%.6s%02d")
		    call pargstr (key)
		    call pargi (i)
		i = i + 1
		Memc[cp2] = EOS
		call imastr (im, Memc[kw], Memc[cp1])
		cp1 = cp2 + 1
		cp = cp1 
	    }
	}
	if (cp - cp1 + 1 > 0) {
	    call sprintf (Memc[kw], 8, "%.6s%02d")
		call pargstr (key)
		call pargi (i)
	    i = i + 1
	    Memc[cp2] = EOS
	    call imastr (im, Memc[kw], Memc[cp1])
	}
	repeat {
	    call sprintf (Memc[kw], 8, "%.6s%02d")
		call pargstr (key)
		call pargi (i)
	    i = i + 1
	    iferr (call imdelf (im, Memc[kw]))
		break
	}

	call sfree (sp)
end


# MAP_GGS -- Get mgs surface fit.

procedure mgs_ggs (im, key, gs)

pointer	im		#I Image pointer
char	key[ARB]	#I Keyword root
pointer	gs		#O Surface fit pointer

int	i, j, nc, ctor()
pointer	sp, kw, card, coeffs

begin
	if (IM_SECTUSED(im) == YES)
	    call error (1, "No surface fit with an image section")

	call smark (sp)
	call salloc (kw, 8, TY_CHAR)
	call salloc (card, 68, TY_CHAR)

	call malloc (coeffs, 100, TY_REAL)
	iferr {
	    nc = 0
	    do i = 1, ARB {
		call sprintf (Memc[kw], 8, "%.6s%02d")
		    call pargstr (key)
		    call pargi (i)
		iferr (call imgstr (im, Memc[kw], Memc[card], 68))
		    break
		j = 1
		while (ctor (Memc[card], j, Memr[coeffs+nc]) != 0) {
		    nc = nc + 1
		    if (mod (nc, 100) == 0)
			call realloc (coeffs, nc+100, TY_REAL)
		}
	    }

	    if (nc == 0)
		call error (1, "Surface fit not found")

	    call gsrestore (gs, Memr[coeffs])
	    call mfree (coeffs, TY_REAL)
	} then {
	    call mfree (coeffs, TY_REAL)
	    call erract (EA_ERROR)
	}

	call sfree (sp)
end
