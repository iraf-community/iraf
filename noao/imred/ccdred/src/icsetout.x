include	<imhdr.h>

# IC_SETOUT -- Set output image size and offsets of input images.

procedure ic_setout (in, out, offsets, nimages)

pointer	in[nimages]		# Input images
pointer	out[3]			# Output images
int	offsets[nimages,ARB]	# Offsets
int	nimages			# Number of images

int	i, j, ndim, a, b, amin, bmax, fd
real	val
bool	reloff, streq()
pointer	sp, fname
int	open(), fscan(), nscan(), strncmp()
errchk	open

include	"icombine.com"
define	newscan_ 10

begin
	# Check and set image dimensionality.
	ndim = IM_NDIM(out[1])
	if (project) {
	    ndim = ndim - 1
	    IM_NDIM(out[1]) = ndim
	} else {
	    do i = 1, nimages
		if (IM_NDIM(in[i]) != ndim)
		    call error (1, "Image dimensions are not the same")
	}

	# Initialize
	aligned = true
	reloff = true
	call aclri (offsets, ndim*nimages)

	# Parse the user offset string.  If "grid" then set the offsets
	# based on the input grid parameters.  If a file scan it.

	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call clgstr ("offsets", Memc[fname], SZ_FNAME)
	call sscan (Memc[fname])
	call gargwrd (Memc[fname], SZ_FNAME)
	if (nscan() > 0) {
	    if (strncmp (Memc[fname], "none", 4) == 0)
		;
	    else if (strncmp (Memc[fname], "grid", 4) == 0) {
		amin = 1
		do j = 1, IM_MAXDIM {
		    call gargi (a)
		    call gargi (b)
		    if (nscan() < 1+2*j)
			break
		    do i = 1, nimages
			offsets[i,j] = mod ((i-1)/amin, a) * b 
		    amin = amin * a
		}
	    } else {
		fd = open (Memc[fname], READ_ONLY, TEXT_FILE)
		do i = 1, nimages {
newscan_	    if (fscan (fd) == EOF)
			call error (1, "ICOMBINE: Offset list too short")
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
		    do j = 1, ndim {
			call gargr (val)
			offsets[i,j] = nint (val)
		    }
		    if (nscan() < ndim)
			call error (1, "ICOMBINE: Error in offset list")
		}
		call close (fd)
	    }
	}

	# Set the output image size and the aligned flag 
	do j = 1, ndim {
	    a = offsets[1,j]
	    b = IM_LEN(in[1],j) + a
	    amin = a
	    bmax = b
	    do i = 2, nimages {
		a = offsets[i,j]
		b = IM_LEN(in[i],j) + a
		if (a != amin || b != bmax)
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

	call sfree (sp)
end
