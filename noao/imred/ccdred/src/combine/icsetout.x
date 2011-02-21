include	<imhdr.h>
include <mwset.h>

# IC_SETOUT -- Set output image size and offsets of input images.

procedure ic_setout (in, out, offsets, nimages)

pointer	in[nimages]		# Input images
pointer	out[ARB]		# Output images
int	offsets[nimages,ARB]	# Offsets
int	nimages			# Number of images

int	i, j, indim, outdim, mwdim, a, b, amin, bmax, fd
real	val
bool	reloff, streq()
pointer	sp, fname, lref, wref, cd, coord, shift, axno, axval
pointer	mw, ct, mw_openim(), mw_sctran()
int	open(), fscan(), nscan(), mw_stati()
errchk	mw_openim, mw_gwtermd, mw_gltermd, mw_gaxmap
errchk	mw_sctran, mw_ctrand, open

include	"icombine.com"
define	newscan_ 10

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (lref, IM_MAXDIM, TY_DOUBLE)
	call salloc (wref, IM_MAXDIM, TY_DOUBLE)
	call salloc (cd, IM_MAXDIM*IM_MAXDIM, TY_DOUBLE)
	call salloc (coord, IM_MAXDIM, TY_DOUBLE)
	call salloc (shift, IM_MAXDIM, TY_REAL)
	call salloc (axno, IM_MAXDIM, TY_INT)
	call salloc (axval, IM_MAXDIM, TY_INT)

	# Check and set the image dimensionality.
	indim = IM_NDIM(in[1])
	outdim = IM_NDIM(out[1])
	if (project) {
	    outdim = indim - 1
	    IM_NDIM(out[1]) = outdim
	} else {
	    do i = 1, nimages
		if (IM_NDIM(in[i]) != outdim) {
		    call sfree (sp)
		    call error (1, "Image dimensions are not the same")
		}
	}

	# Set the reference point to that of the first image.
	mw = mw_openim (in[1])
	mwdim = mw_stati (mw, MW_NPHYSDIM)
	call mw_gwtermd (mw, Memd[lref], Memd[wref], Memd[cd], mwdim)
	ct = mw_sctran (mw, "world", "logical", 0)
	call mw_ctrand (ct, Memd[wref], Memd[lref], mwdim)
	call mw_ctfree (ct)
	if (project)
	    Memd[lref+outdim] = 1

	# Parse the user offset string.  If "none" then there are no offsets.
	# If "wcs" then set the offsets based on the image WCS.
	# If "grid" then set the offsets based on the input grid parameters.
	# If a file scan it.

	call clgstr ("offsets", Memc[fname], SZ_FNAME)
	call sscan (Memc[fname])
	call gargwrd (Memc[fname], SZ_FNAME)
	if (nscan() == 0 || streq (Memc[fname], "none")) {
	    call aclri (offsets, outdim*nimages)
	    reloff = true
	} else if (streq (Memc[fname], "wcs")) {
	    do j = 1, outdim
		offsets[1,j] = 0
	    if (project) {
		ct = mw_sctran (mw, "world", "logical", 0)
		do i = 2, nimages {
		    Memd[wref+outdim] = i
		    call mw_ctrand (ct, Memd[wref], Memd[coord], indim)
		    do j = 1, outdim
			offsets[i,j] = nint (Memd[lref+j-1] - Memd[coord+j-1])
		}
		call mw_ctfree (ct)
		call mw_close (mw)
	    } else {
		do i = 2, nimages {
		    call mw_close (mw)
		    mw = mw_openim (in[i])
		    ct = mw_sctran (mw, "world", "logical", 0)
		    call mw_ctrand (ct, Memd[wref], Memd[coord], indim)
		    do j = 1, outdim
			offsets[i,j] = nint (Memd[lref+j-1] - Memd[coord+j-1])
		    call mw_ctfree (ct)
		}
	    }
	    reloff = true
	} else if (streq (Memc[fname], "grid")) {
	    amin = 1
	    do j = 1, outdim {
		call gargi (a)
		call gargi (b)
		if (nscan() < 1+2*j)
		    break
		do i = 1, nimages
		    offsets[i,j] = mod ((i-1)/amin, a) * b 
		amin = amin * a
	    }
	    reloff = true
	} else {
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
		    offsets[i,j] = nint (val)
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

	# Update the WCS.
	if (project || !aligned || !reloff) {
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
		call mw_sltermd (mw, Memd[cd], Memd[lref], mwdim)
	    }
	    if (project) {
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
	    call mw_saveim (mw, out)
	}
	call mw_close (mw)

	call sfree (sp)
end
