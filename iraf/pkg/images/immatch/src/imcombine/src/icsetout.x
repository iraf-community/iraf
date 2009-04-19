include	<imhdr.h>
include	<imset.h>
include <mwset.h>

define	OFFTYPES	"|none|wcs|world|physical|grid|"
define	FILE		0
define	NONE		1
define	WCS		2
define	WORLD		3
define	PHYSICAL	4
define	GRID		5

# IC_SETOUT -- Set output image size and offsets of input images.

procedure ic_setout (in, out, offsets, nimages)

pointer	in[nimages]		# Input images
pointer	out[ARB]		# Output images
long	offsets[nimages,ARB]	# Offsets
int	nimages			# Number of images

size_t	sz_val
pointer	p_val
int	i, j, indim, outdim, mwdim, fd, offtype
long	a, b, amin, bmax
real	val
bool	proj, reloff, flip, streq(), fp_equald()
pointer	sp, str, fname
pointer	ltv, lref, wref, cd, ltm, coord, shift, axno, axval, section
pointer	mw, ct, mw_openim(), mw_sctran(), xt_immap()
int	open(), fscan(), nscan(), mw_stati(), strlen(), strdic()
long	lmod(), lnint(), ldnint()
errchk	mw_openim, mw_gwtermd, mw_gltermd, mw_gaxmap
errchk	mw_sctran, mw_ctrand, open, xt_immap

include	"icombine.com"
define	newscan_ 10

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (str, sz_val, TY_CHAR)
	call salloc (fname, sz_val, TY_CHAR)
	sz_val = IM_MAXDIM
	call salloc (ltv, sz_val, TY_DOUBLE)
	sz_val = IM_MAXDIM*IM_MAXDIM
	call salloc (ltm, sz_val, TY_DOUBLE)
	sz_val = IM_MAXDIM
	call salloc (lref, sz_val, TY_DOUBLE)
	call salloc (wref, sz_val, TY_DOUBLE)
	sz_val = IM_MAXDIM*IM_MAXDIM
	call salloc (cd, sz_val, TY_DOUBLE)
	sz_val = IM_MAXDIM
	call salloc (coord, sz_val, TY_DOUBLE)
	call salloc (shift, sz_val, TY_REAL)
	call salloc (axno, sz_val, TY_INT)
	call salloc (axval, sz_val, TY_INT)

	# Check and set the image dimensionality.
	indim = IM_NDIM(in[1])
	outdim = IM_NDIM(out[1])
	proj = (indim != outdim)
	if (!proj) {
	    do i = 1, nimages
		if (IM_NDIM(in[i]) != outdim) {
		    call sfree (sp)
		    call error (1, "Image dimensions are not the same")
		}
	}

	# Set the reference point to that of the first image.
	mw = mw_openim (in[1])
	call mw_seti (mw, MW_USEAXMAP, NO)
	mwdim = mw_stati (mw, MW_NPHYSDIM)
	call mw_gwtermd (mw, Memd[lref], Memd[wref], Memd[cd], mwdim)
	ct = mw_sctran (mw, "world", "logical", 0)
	call mw_ctrand (ct, Memd[wref], Memd[lref], mwdim)
	call mw_ctfree (ct)
	if (proj)
	    Memd[lref+outdim] = 1

	# Parse the user offset string.  If "none" then there are no offsets.
	# If "world" or "wcs" then set the offsets based on the world WCS.
	# If "physical" then set the offsets based on the physical WCS.
	# If "grid" then set the offsets based on the input grid parameters.
	# If a file scan it.

	call clgstr ("offsets", Memc[fname], SZ_FNAME)
	call sscan (Memc[fname])
	call gargwrd (Memc[fname], SZ_FNAME)
	if (nscan() == 0)
	    offtype = NONE
	else {
	    offtype = strdic (Memc[fname], Memc[str], SZ_FNAME, OFFTYPES)
	    if (offtype > 0 && !streq (Memc[fname], Memc[str]))
		offtype = 0
	}
	if (offtype == 0)
	    offtype = FILE

	switch (offtype) {
	case NONE:
	    sz_val = outdim*nimages
	    call aclrl (offsets, sz_val)
	    reloff = true
	case WORLD, WCS:
	    do j = 1, outdim
		offsets[1,j] = 0
	    if (proj) {
		ct = mw_sctran (mw, "world", "logical", 0)
		do i = 2, nimages {
		    Memd[wref+outdim] = i
		    call mw_ctrand (ct, Memd[wref], Memd[coord], indim)
		    do j = 1, outdim
			offsets[i,j] = ldnint(Memd[lref+j-1] - Memd[coord+j-1])
		}
		call mw_ctfree (ct)
		call mw_close (mw)
	    } else {
		ct = mw_sctran (mw, "world", "logical", 0)
		call mw_ctrand (ct, Memd[wref], Memd[lref], indim)
		do i = 2, nimages {
		    call mw_close (mw)
		    mw = mw_openim (in[i])
		    ct = mw_sctran (mw, "world", "logical", 0)
		    call mw_ctrand (ct, Memd[wref], Memd[coord], indim)
		    do j = 1, outdim
			offsets[i,j] = ldnint(Memd[lref+j-1] - Memd[coord+j-1])
		    call mw_ctfree (ct)
		}
	    }
	    reloff = true
	case PHYSICAL:
	    sz_val = SZ_FNAME
	    call salloc (section, sz_val, TY_CHAR)

	    call mw_gltermd (mw, Memd[ltm], Memd[coord], indim)
	    do i = 2, nimages {
		call mw_close (mw)
		mw = mw_openim (in[i])
		call mw_gltermd (mw, Memd[cd], Memd[coord], indim)
		call strcpy ("[", Memc[section], SZ_FNAME)
		flip = false
		do j = 0, indim*indim-1, indim+1 {
		    if (Memd[ltm+j] * Memd[cd+j] >= 0.)
			call strcat ("*,", Memc[section], SZ_FNAME)
		    else {
			call strcat ("-*,", Memc[section], SZ_FNAME)
			flip = true
		    }
		}
		Memc[section+strlen(Memc[section])-1] = ']'
		if (flip) {
		    call imstats (in[i], IM_IMAGENAME, Memc[fname], SZ_FNAME)
		    call strcat (Memc[section], Memc[fname], SZ_FNAME)
		    call xt_imunmap (in[i], i)
		    p_val = TY_CHAR
		    in[i] = xt_immap (Memc[fname], READ_ONLY, p_val, i) 
		    call mw_close (mw)
		    mw = mw_openim (in[i])
		    call mw_gltermd (mw, Memd[cd], Memd[coord], indim)
		    do j = 0, indim*indim-1
			if (!fp_equald (Memd[ltm+j], Memd[cd+j]))
			    call error (1,
				"Cannot match physical coordinates")
		}
	    }

	    call mw_close (mw)
	    mw = mw_openim (in[1])
	    ct = mw_sctran (mw, "logical", "physical", 0)
	    call mw_ctrand (ct, Memd[lref], Memd[ltv], indim)
	    call mw_ctfree (ct)
	    do j = 1, outdim
		offsets[1,j] = 0
	    if (proj) {
		ct = mw_sctran (mw, "physical", "logical", 0)
		do i = 2, nimages {
		    Memd[ltv+outdim] = i
		    call mw_ctrand (ct, Memd[ltv], Memd[coord], indim)
		    do j = 1, outdim
			offsets[i,j] = ldnint(Memd[lref+j-1] - Memd[coord+j-1])
		}
		call mw_ctfree (ct)
		call mw_close (mw)
	    } else {
		do i = 2, nimages {
		    call mw_close (mw)
		    mw = mw_openim (in[i])
		    ct = mw_sctran (mw, "physical", "logical", 0)
		    call mw_ctrand (ct, Memd[ltv], Memd[coord], indim)
		    do j = 1, outdim
			offsets[i,j] = ldnint(Memd[lref+j-1] - Memd[coord+j-1])
		    call mw_ctfree (ct)
		}
	    }
	    reloff = true
	case GRID:
	    amin = 1
	    do j = 1, outdim {
		call gargl (a)
		call gargl (b)
		if (nscan() < 1+2*j) {
		    a = 1
		    b = 0
		}
		do i = 1, nimages
		    offsets[i,j] = lmod((i-1)/amin, a) * b 
		amin = amin * a
	    }
	    reloff = true
	case FILE:
	    reloff = true
	    fd = open (Memc[fname], READ_ONLY, TEXT_FILE)
	    do i = 1, nimages {
newscan_	if (fscan (fd) == EOF)
		    call error (1, "IMCOMBINE: Offset list too short")
		call gargwrd (Memc[fname], SZ_FNAME)
		if (Memc[fname] == '#') {
		    call gargwrd (Memc[fname], SZ_FNAME)
		    call strlwr (Memc[fname])
		    if (streq (Memc[fname], "absolute"))
			reloff = false
		    else if (streq (Memc[fname], "relative"))
			reloff = true
		    goto newscan_
		}
		call reset_scan ()
		do j = 1, outdim {
		    call gargr (val)
		    offsets[i,j] = lnint(val)
		}
		if (nscan() < outdim)
		    call error (1, "IMCOMBINE: Error in offset list")
	    }
	    call close (fd)
	}

	# Set the output image size and the aligned flag 
	aligned = true
	do j = 1, outdim {
	    a = offsets[1,j]
	    b = IM_LEN(in[1],j) + a
	    amin = a
	    bmax = b
	    do i = 2, nimages {
		a = offsets[i,j]
		b = IM_LEN(in[i],j) + a
		if (a != amin || b != bmax || !reloff)
		    aligned = false
		amin = min (a, amin)
		bmax = max (b, bmax)
	    }
	    IM_LEN(out[1],j) = bmax
	    if (reloff || amin < 0) {
		do i = 1, nimages
		    offsets[i,j] = offsets[i,j] - amin
		IM_LEN(out[1],j) = IM_LEN(out[1],j) - amin
	    }
	}

	# Get the output limits.
	call clgstr ("outlimits", Memc[fname], SZ_FNAME)
	call sscan (Memc[fname])
	do j = 1, outdim {
	    call gargl (a)
	    call gargl (b)
	    if (nscan() < 2*j)
		break
	    if (!IS_INDEFL(a)) {
		do i = 1, nimages {
		    offsets[i,j] = offsets[i,j] - a + 1
		    if (offsets[i,j] != 0)
			aligned = false
		}
		IM_LEN(out[1],j) = IM_LEN(out[1],j) - a + 1
	    }
	    if (!IS_INDEFL(a) && !IS_INDEFL(b))
		IM_LEN(out[1],j) = min (IM_LEN(out[1],j), b - a + 1)
	}

	# Update the WCS.
	if (proj || !aligned || !reloff) {
	    call mw_close (mw)
	    mw = mw_openim (out[1])
	    mwdim = mw_stati (mw, MW_NPHYSDIM)
	    call mw_gaxmap (mw, Memi[axno], Memi[axval], mwdim)
	    if (!aligned || !reloff) {
		call mw_gltermd (mw, Memd[cd], Memd[lref], mwdim)
		do i = 1, mwdim {
		    j = Memi[axno+i-1]
		    if (j > 0 && j <= indim)
			Memd[lref+i-1] = Memd[lref+i-1] + offsets[1,j]
		}
		if (proj)
		    Memd[lref+mwdim-1] = 0.
		call mw_sltermd (mw, Memd[cd], Memd[lref], mwdim)
	    }
	    if (proj) {
		# Apply dimensional reduction.
		do i = 1, mwdim {
		    j = Memi[axno+i-1]
		    if (j <= outdim)
			next
		    else if (j > outdim+1)
			Memi[axno+i-1] = j - 1
		    else {
			Memi[axno+i-1] = 0
			Memi[axval+i-1] = 0
		    }
		}
		call mw_saxmap (mw, Memi[axno], Memi[axval], mwdim)
	    }

	    # Reset physical coordinates.
	    if (offtype == WCS || offtype == WORLD) {
		call mw_gltermd (mw, Memd[ltm], Memd[ltv], mwdim)
		call mw_gwtermd (mw, Memd[lref], Memd[wref], Memd[cd], mwdim)
		call mwvmuld (Memd[ltm], Memd[lref], Memd[lref], mwdim)
		sz_val = mwdim
		call aaddd (Memd[lref], Memd[ltv], Memd[lref], sz_val)
		call mwinvertd (Memd[ltm], Memd[ltm], mwdim)
		call mwmmuld (Memd[cd], Memd[ltm], Memd[cd], mwdim)
		call mw_swtermd (mw, Memd[lref], Memd[wref], Memd[cd], mwdim)
		sz_val = mwdim
		call aclrd (Memd[ltv], sz_val)
		sz_val = mwdim*mwdim
		call aclrd (Memd[ltm], sz_val)
		do i = 1, mwdim
		    Memd[ltm+(i-1)*(mwdim+1)] = 1.
		call mw_sltermd (mw, Memd[ltm], Memd[ltv], mwdim)
	    }
	    call mw_saveim (mw, out)
	}
	call mw_close (mw)

	call sfree (sp)
end
