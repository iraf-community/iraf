include	<imhdr.h>
include	<imio.h>
include	<mwset.h>
include "rvpackage.h"
include "rvmwcs.h"

# RVMWCS.X - Procedure to manage the MWCS sub-structure.  Routiunes are:
#
#    Open the structure:	rv_mwcs_open (rv)
#    Close the structure:       rv_mwcs_close (rv)
#    Fill the struct:     	rv_mwcs (im, ap, dtype, w1, dw, nw, nspec)
#


# RV_MWCS_OPEN - Open the MWCS structure and return an allocated pointer.

procedure rv_mwcs_open (rv)

pointer	rv					#I RV struct pointer
pointer	mptr

begin
        iferr (call calloc (mptr, SZ_MWCS_STRUCT, TY_STRUCT))
            call error (0, "Error allocating MWCS sub-structure.")
        RV_MWCSP(rv) = mptr

        # Initlialize the values
	RVMW_AP(rv) = NULL
	RVMW_DTYPE(rv) = NULL
	RVMW_W1(rv) = NULL
	RVMW_DW(rv) = NULL
	RVMW_NW(rv) = NULL
end


# RV_MWCS_CLOSE - Close the MWCS structure and free all pointers.

procedure rv_mwcs_close (rv)

pointer	rv					#I RV structpointer

begin
	if (RV_MWCSP(rv) != NULL) {
            # Free the pointers
	    call mfree (RVMW_AP(rv), TY_INT)
	    call mfree (RVMW_DTYPE(rv), TY_INT)
	    call mfree (RVMW_W1(rv), TY_REAL)
	    call mfree (RVMW_DW(rv), TY_REAL)
	    call mfree (RVMW_NW(rv), TY_INT)

	    # Free the structure
	    call mfree (RV_MWCSP(rv), TY_STRUCT)
	}
	RV_MWCSP(rv) = NULL
end


# RV_MWCS -- Get aperture, dispersion type, and linear wavelength parameters.
# The MWCS for the image is opened by the ONEDSPEC smw_open procedure which
# converts all input formats to "multispec".  Then for each logical line
# in the spectrum the spectrum information is obtain and stored in dynamic
# arrays which are allocated by the procedure and returned.

procedure rv_mwcs (im, ap, dtype, w1, dw, nw, nspec)

pointer	im		#I IMIO pointer
pointer	ap		#O Aperture number array pointer
pointer	dtype		#O Dispersion type array pointer
pointer	w1		#O Starting wavelength array pointer
pointer	dw		#O Wavelength increment array pointer
pointer	nw		#O Valid number of pixels array pointer
int	nspec		#O Number of spectra

int	i, i1, i2, n, nc, nl
real	a, b, c, line, mw_c1tranr()
double	d
pointer	mw, ct_lp, ct_pl, ct_lw, str, smw_openim(), mw_sctran()
errchk	smw_openim, mw_sctran

begin
	# Open MWCS
	mw = smw_openim (im)
	ct_pl = mw_sctran (mw, "physical", "logical", 1)
	ct_lp = mw_sctran (mw, "logical", "physical", 2)
	ct_lw = mw_sctran (mw, "logical", "world", 3)

	# Allocate memory.  Initially NULL pointers are given and
	# memory is allocated.  Repeated calls with the same pointers
	# will reallocate memory.  It is up to the calling procedure
	# to initialize to NULL and ultimately  free memory.

	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	nspec = nl
	if (ap == NULL)
	    call malloc (ap, nspec, TY_INT)
	else
	    call realloc (ap, nspec, TY_INT)
	if (dtype == NULL)
	    call malloc (dtype, nspec, TY_INT)
	else
	    call realloc (dtype, nspec, TY_INT)
	if (w1 == NULL)
	    call malloc (w1, nspec, TY_REAL)
	else
	    call realloc (w1, nspec, TY_REAL)
	if (dw == NULL)
	    call malloc (dw, nspec, TY_REAL)
	else
	    call realloc (dw, nspec, TY_REAL)
	if (nw == NULL)
	    call malloc (nw, nspec, TY_INT)
	else
	    call realloc (nw, nspec, TY_INT)
	str = NULL

	# For each image logical line determine the physical line.  Get
	# the aperture number and dispersion type from the MWCS attribute.
	# Determine the logical linear coordinate parameters and the number
	# of valid pixels.

	do i = 0, nl-1 {
	    line = 1 + i
	    i1 = nint (mw_c1tranr (ct_lp, line))
	    #call shdr_gwattrs (mw, i1, Memi[ap+i], i2, Memi[dtype+i],
	    #	a, a, n, a, a, a, str)
	    ifnoerr (call shdr_gwattrs (mw, i1, Memi[ap+i], i2, Memi[dtype+i],
	    	d, d, n, d, d, d, str)) {

	        i1 = max (1, min (int (mw_c1tranr (ct_pl, 1.)), nc))
	        i2 = max (1, min (int (mw_c1tranr (ct_pl, real (n))), nc))
	        n = max (i1, i2)
	        call mw_c2tranr (ct_lw, 1., line, a, c)
	        call mw_c2tranr (ct_lw, real(n), line, b, c)

	        Memr[w1+i] = a
	        Memr[dw+i] = (b - a) / (n - 1)
	        Memi[nw+i] = n
	    } else {
	        Memi[ap+i] = line
	        Memi[dtype+i] = -1
	        Memr[w1+i] = 1.
	        Memr[dw+i] = 1.
	        Memi[nw+i] = n
	    }
	}

	call mw_close (mw)
	call mfree (str, TY_CHAR)
end

# ==== The following is taken from the ONEDSPEC package unchanged except =====
# 1. smw_1d called instead of smw_2d in smw_openim
# 2. smw_1d allows 2D image


# The following routines interface various WCS formats to the "multispec"
# system used in the ONEDSPEC package.

# SMW_OPENIM  -- Open MWCS
# SMW_SAVEIM  -- Save MWCS
# SMW_MS      -- Convert old multispec header to new format and set WCS.
# SMW_1D      -- Convert old onedspec header to new format and set WCS.
# SMW_2D      -- Fix up WCS


# SMW_OPENIM -- Open MWCS
# This routine returns a "multispec" WCS for various input formats.

pointer procedure smw_openim (im)

pointer	im		#I Image pointer
pointer	mw		#O MWCS pointer

pointer	sp, str, mw_openim()
bool	strne()
int	mw_stati()
errchk	mw_openim, smw_ms, smw_1d

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	mw = mw_openim (im)
	call mw_seti (mw, MW_USEAXMAP, NO)
	call mw_gwattrs (mw, 0, "system", Memc[str], SZ_FNAME)
	if (strne (Memc[str], "multispec")) {
	    ifnoerr (call imgstr (im, "APFORMAT", Memc[str], SZ_LINE)) {
		if (strne (Memc[str], "onedspec"))
		    call smw_ms (im, mw)
		else
		    call smw_1d (im, mw)
	    } else if (mw_stati (mw, MW_NDIM) == 1)
		call smw_1d (im, mw)
	    else
		call smw_2d (im, mw)
	}

	call sfree (sp)
	return (mw)
end


# SMW_SAVEIM -- Save WCS in image header
# This procedure converts multispec WCS which are linear and contain just
# one spectrum to a simple linear WCS.

procedure smw_saveim (mw, im)

pointer	mw			# Multispec MWCS pointer
pointer	im			# Image pointer

int	ap, beam, dtype, nw, imaccf()
double	w1, dw, z, aplow, aphigh
pointer	mw1, sp, str, axmap, lterm, mw_open()
errchk	imdelf

begin
	# This is to work around truncation of header if the image header
	# is later copied.
	IM_HDRLEN(im) = IM_LENHDRMEM(im)

	if (IM_NDIM(im) != 1) {
	    call mw_saveim (mw, im)
	    return
	}

	str = NULL
	call shdr_gwattrs (mw, 1, ap, beam, dtype, w1, dw, nw, z, aplow,
	    aphigh, str)
	call mfree (str, TY_CHAR)

	if (dtype == 2) {
            call smark (sp)
            call salloc (axmap, 4, TY_INT)
            Memi[axmap] = 1
            Memi[axmap+1] = 0
            Memi[axmap+2] = 0
            Memi[axmap+3] = 0
            call mw_saxmap (mw, Memi[axmap], Memi[axmap+2], 2)
            call mw_saveim (mw, im)
            call sfree (sp)
            return
	}

	if (nw < IM_LEN(im,1))
	    call imaddi (im, "NP2", nw)
	else if (imaccf (im, "NP2") == YES)
	    call imdelf (im, "NP2")
	call imaddi (im, "APNUM", ap)
	call imaddi (im, "BEAM-NUM", beam)
	if (dtype != -1)
	    call imaddi (im, "DC-FLAG", dtype)
	else if (imaccf (im, "DC-FLAG") == YES)
	    call imdelf (im, "DC-FLAG")
	if (z != 0.)
	    call imaddd (im, "DOPCOR", z)
	else if (imaccf (im, "DOPCOR") == YES)
	    call imdelf (im, "DOPCOR")
	if (!IS_INDEFD(aplow))
	    call imaddd (im, "APLOW", aplow)
	else if (imaccf (im, "APLOW") == YES)
	    call imdelf (im, "APLOW")
	if (!IS_INDEFD(aphigh))
	    call imaddd (im, "APHIGH", aphigh)
	else if (imaccf (im, "APHIGH") == YES)
 	    call imdelf (im, "APHIGH")

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (lterm, 6, TY_DOUBLE)

	mw1 = mw_open (NULL, 1)
	call mw_newsystem (mw1, "linear", 1)
	call mw_swtype (mw1, 1, 1, "linear", "")
	ifnoerr (call mw_gwattrs (mw, 1, "label", Memc[str], SZ_LINE))
	    call mw_swattrs (mw1, 1, "label", Memc[str])
	ifnoerr (call mw_gwattrs (mw, 1, "units", Memc[str], SZ_LINE))
	    call mw_swattrs (mw1, 1, "units", Memc[str])
	call mw_gltermd (mw, Memd[lterm], Memd[lterm+4], 2)
	call mw_sltermd (mw1, Memd[lterm], Memd[lterm+4], 1)
	call mw_swtermd (mw1, 1D0, w1, dw, 1)
	call mw_saveim (mw1, im)
	call mw_close (mw1)

	call sfree (sp)
end


# SMW_MS -- Convert old multispec header to new format and set WCS.

procedure smw_ms (im, mw)

pointer	im			# IMIO pointer
pointer	mw			# MWCS pointer

int	i, j, k, ap, beam, dtype, nw, axes[2]
double	w1, dw, aplow, aphigh, z
pointer	sp, key, str, lterm, lterm1
int	mw_stati(), imgeti(), ctoi(), ctod(), imofnlu(), imgnfn()
pointer	mw_open()
data	axes/1,2/

begin
        call smark (sp)
        call salloc (key, SZ_FNAME, TY_CHAR)
        call salloc (str, SZ_LINE, TY_CHAR)
        call salloc (lterm, 6, TY_DOUBLE)

        # Initialize WCS
        switch (mw_stati (mw, MW_NDIM)) {
        case 1:
            call mw_gltermd (mw, Memd[lterm], Memd[lterm+4], 1)
            Memd[lterm+1] = 0
            Memd[lterm+2] = 0
            Memd[lterm+3] = 1
            Memd[lterm+5] = 0
        case 2:
            call mw_gltermd (mw, Memd[lterm], Memd[lterm+4], 2)
        case 3:
            call salloc (lterm1, 9, TY_DOUBLE)
            call mw_gltermd (mw, Memd[lterm1], Memd[lterm+4], 3)
            Memd[lterm] = Memd[lterm1]
            Memd[lterm+1] = Memd[lterm1+1]
            Memd[lterm+2] = Memd[lterm1+3]
            Memd[lterm+3] = Memd[lterm1+4]
        }
        call mw_close (mw)
        mw = mw_open (NULL, 2)
        call mw_newsystem (mw, "multispec", 2)
        call mw_swtype (mw, axes, 2, "multispec", "")
        call mw_ssystem (mw, "multispec")
        call mw_sltermd (mw, Memd[lterm], Memd[lterm+4], 2)

        # Get old parameters and write as attributes
        iferr (dtype = imgeti (im, "DC-FLAG"))
            dtype = -1
        else {
            call mw_swattrs (mw, 1, "label", "Wavelength")
            call mw_swattrs (mw, 1, "units", "Angstroms")
        }
        for (i=1;;i=i+1) {
            call sprintf (Memc[key], SZ_FNAME, "APNUM%d")
                call pargi (i)
            iferr (call imgstr (im, Memc[key], Memc[str], SZ_LINE))
                break

            j = 1
            k = ctoi (Memc[str], j, ap)
            k = ctoi (Memc[str], j, beam)
            k = ctod (Memc[str], j, w1)
            k = ctod (Memc[str], j, dw)
            k = ctoi (Memc[str], j, nw)
            if (k == 0)
                call error (1, "Syntax error in APNUM parameter")
            if (ctod (Memc[str], j, aplow) == 0)
                aplow = INDEF
            if (ctod (Memc[str], j, aphigh) == 0)
                aphigh = INDEF
            z = 0.

            k = dtype
            if (k==1 && (abs(w1)>20. || abs(w1+(nw-1)*dw)>20.))
                k = 0
            call shdr_swattrs (mw, i, ap, beam, k, w1, dw, nw, z,
                aplow, aphigh, "")
        }

        # Delete old parameters
	i = imofnlu (im,
	    "DISPAXIS,APFORMAT,APNUM*,BEAM-NUM,DC-FLAG,W0,WPC,NP1,NP2")
        while (imgnfn (i, Memc[key], SZ_FNAME) != EOF)
            call imdelf (im, Memc[key])

        # Update new parameters
        call smw_saveim (mw, im)

        call sfree (sp)
end


# SMW_1D -- Convert old onedspec header to new format and set WCS.

procedure smw_1d (im, mw)

pointer	im			# IMIO pointer
pointer	mw			# MWCS pointer

int     i, ap, beam, dtype, nw, axes[2]
double  m, v, r, w1, dw, aplow, aphigh, z
pointer sp, coeff, lterm
double  imgetd()
int     imgeti(), imofnlu(), imgnfn()
pointer mw_openim(), mw_open()
data    axes/1,2/

begin
        call smark (sp)
        call salloc (coeff, SZ_FNAME, TY_CHAR)
        call salloc (lterm, 6, TY_DOUBLE)

        # Convert old W0/WPC keywords if needed.
        iferr (w1 = imgetd (im, "CRVAL1")) {
            ifnoerr (w1 = imgetd (im, "W0")) {
                dw = imgetd (im, "WPC")
                iferr (m = imgetd (im, "LTM1_1"))
                    m = 1
                iferr (v = imgetd (im, "LTV1"))
                    v = 0
                r = m + v
                dw = dw / m
                call imaddd (im, "CRPIX1", r)
                call imaddd (im, "CRVAL1", w1)
                call imaddd (im, "CD1_1", dw)
                call imaddd (im, "CDELT1", dw)
                call mw_close(mw)
                mw = mw_openim (im)
            }
        }

        # Get 1D WCS
        call mw_gltermd (mw, m, v, 1)
        call mw_gwtermd (mw, r, w1, dw, 1)
        call mw_close (mw)

        # Set MULTISPEC WCS
        mw = mw_open (NULL, 2)
        call mw_newsystem (mw, "multispec", 2)
        call mw_swtype (mw, axes, 2, "multispec", "")
        call mw_ssystem (mw, "multispec")
        Memd[lterm] = m
        Memd[lterm+1] = 0.
        Memd[lterm+2] = 0.
        Memd[lterm+3] = 1.
        Memd[lterm+4] = v
        Memd[lterm+5] = 0.
        call mw_sltermd (mw, Memd[lterm], Memd[lterm+4], 2)

        iferr (beam = imgeti (im, "BEAM-NUM"))
            beam = 1
        iferr (ap = imgeti (im, "APNUM"))
            ap = beam
        iferr (dtype = imgeti (im, "DC-FLAG"))
            dtype = -1
        iferr (nw = imgeti (im, "NP2"))
            nw = max ((1.-v)/m, (IM_LEN(im,1)-v)/m)
        iferr (z = imgetd (im, "DOPCOR"))
            z = 0.
        iferr (aplow = imgetd (im, "APLOW"))
            aplow = INDEF
        iferr (aphigh = imgetd (im, "APHIGH"))
            aphigh = INDEF

        # Set WCS attributes
        if (dtype != -1) {
            call mw_swattrs (mw, 1, "label", "Wavelength")
            call mw_swattrs (mw, 1, "units", "Angstroms")
        }
        w1 = w1 + dw * (1 - r)

        if (dtype==1 && (abs(w1)>20. || abs(w1+(nw-1)*dw)>20.))
            dtype = 0
        call shdr_swattrs (mw, 1, ap, beam, dtype, w1, dw, nw, z,
            aplow, aphigh, "")

        # Delete old parameters
	i = imofnlu (im,
	    "DISPAXIS,APFORMAT,APNUM*,BEAM-NUM,DC-FLAG,W0,WPC,NP1,NP2")
       while (imgnfn (i, Memc[coeff], SZ_FNAME) != EOF)
            call imdelf (im, Memc[coeff])
        call imcfnl (i)

        # Update new parameters
        call smw_saveim (mw, im)

        call sfree (sp)
end


# SMW_2D -- Fix up WCS

procedure smw_2d (im, mw)

pointer im                      # IMIO pointer
pointer mw                      # MWCS pointer

int     i, ndim, mw_stati()
double  val
pointer sp, r, w, cd

begin
        ndim = mw_stati (mw, MW_NDIM)

        call smark (sp)
        call salloc (r, ndim, TY_DOUBLE)
        call salloc (w, ndim, TY_DOUBLE)
        call salloc (cd, ndim*ndim, TY_DOUBLE)

        # Check cd terms.  Assume no rotation.
        call mw_gwtermd (mw, Memd[r], Memd[w], Memd[cd], ndim)
        do i = 0, ndim-1 {
            val = Memd[cd+i*(ndim+1)]
            if (val == 0D0)
                Memd[cd+i*(ndim+1)] = 1D0
        }
        call mw_swtermd (mw, Memd[r], Memd[w], Memd[cd], ndim)

        call sfree (sp)
end


# SHDR_GWATTRS -- Get spectrum attribute parameters

procedure shdr_gwattrs (mw, line, ap, beam, dtype, w1, dw, nw, z, aplow, aphigh,
	coeff)

pointer	mw				# MWCS pointer
int	line				# Physical line number
int	ap				# Aperture number
int	beam				# Beam number
int	dtype				# Dispersion type
double	w1				# Starting coordinate
double	dw				# Coordinate interval
int	nw				# Number of valid pixels
double	z				# Redshift factor
double	aplow, aphigh			# Aperture limits
pointer	coeff				# Nonlinear coeff string (input/output)

int	i, j, sz_coeff, strlen(), ctoi(), ctod()
pointer	sp, key
errchk	mw_gwattrs

data	sz_coeff /SZ_LINE/

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)

	if (coeff == NULL)
	    call malloc (coeff, sz_coeff, TY_CHAR)
	else
	    call realloc (coeff, sz_coeff, TY_CHAR)

	call sprintf (Memc[key], SZ_FNAME, "spec%d")
	    call pargi (line)

	call mw_gwattrs (mw, 2, Memc[key], Memc[coeff], sz_coeff)
	while (strlen (Memc[coeff]) == sz_coeff) {
	    sz_coeff = 2 * sz_coeff
	    call realloc (coeff, sz_coeff, TY_CHAR)
	    call mw_gwattrs (mw, 2, Memc[key], Memc[coeff], sz_coeff)
	}

	i = 1
	j = ctoi (Memc[coeff], i, ap)
	j = ctoi (Memc[coeff], i, beam)
	j = ctoi (Memc[coeff], i, dtype)
	j = ctod (Memc[coeff], i, w1)
	j = ctod (Memc[coeff], i, dw)
	j = ctoi (Memc[coeff], i, nw)
	j = ctod (Memc[coeff], i, z)
	j = ctod (Memc[coeff], i, aplow)
	j = ctod (Memc[coeff], i, aphigh)
	if (Memc[coeff+i-1] != EOS)
	    call strcpy (Memc[coeff+i], Memc[coeff], sz_coeff)
	else
	    Memc[coeff] = EOS

	if (j == 0)
	    call error (1, "Syntax error in spectrum attribute parameter")

	call sfree (sp)
end


# SHDR_SWATTRS -- Set spectrum attribute parameters

procedure shdr_swattrs (mw, line, ap, beam, dtype, w1, dw, nw, z, aplow, aphigh,
	coeff)

pointer	mw				# MWCS pointer
int	line				# Physical line number
int	ap				# Aperture number
int	beam				# Beam number
int	dtype				# Dispersion type
double	w1				# Starting coordinate
double	dw				# Coordinate interval
int	nw				# Number of valid pixels
double	z				# Redshift factor
double	aplow, aphigh			# Aperture limits
char	coeff[ARB]			# Nonlinear coeff string

int	sz_val, strlen()
pointer	sp, key, val

begin
	sz_val = strlen (coeff) + SZ_LINE

	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (val, sz_val, TY_CHAR)

	# We can't use SPRINTF for the whole string because it can only
	# handle a limited length and trucates long coefficient strings.
	# Use STRCAT instead.

	call sprintf (Memc[key], SZ_FNAME, "spec%d")
	    call pargi (line)
	call sprintf (Memc[val], sz_val, "%d %d %d %g %g %d %g %.2f %.2f")
	    call pargi (ap)
	    call pargi (beam)
	    call pargi (dtype)
	    call pargd (w1)
	    call pargd (dw)
	    call pargi (nw)
	    call pargd (z)
	    call pargd (aplow)
	    call pargd (aphigh)
	if (coeff[1] != EOS) {
	    call strcat (" ", Memc[val], sz_val)
	    call strcat (coeff, Memc[val], sz_val)
	}
	call mw_swattrs (mw, 2, Memc[key], Memc[val])

	call sfree (sp)
end
