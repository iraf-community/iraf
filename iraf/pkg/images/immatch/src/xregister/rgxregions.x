include <fset.h>
include <ctype.h>
include <imhdr.h>
include "xregister.h"

# RG_XREGIONS -- Decode the image sections into regions. If the sections string
# is NULL then the regions list is initially empty and depending on the mode
# of the task, XREGISTER will or will not complain.Otherwise the image
# sections specified in the sections string or file are decoded into a
# regions list.

int procedure rg_xregions (list, im, xc, rp)

int	list			#I pointer to the regions list
pointer	im			#I pointer to the reference image
pointer	xc			#I pointer to the cross-correlation structure
int	rp			#I index of the current region

int	fd, nregions
pointer	sp, fname, regions
int	rg_xgrid(), rg_xgregions(), rg_xrregions(), rg_xstati(), fntgfnb()
int	open()
errchk	fntgfnb(), open(), close()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (regions, SZ_LINE, TY_CHAR)

	call rg_xstats (xc, REGIONS, Memc[regions], SZ_LINE)
	if (rp < 1 || rp > MAX_NREGIONS || Memc[regions] == EOS) {
	    nregions = 0
	} else if (rg_xgrid (im, xc, rp, MAX_NREGIONS) > 0) {
	    nregions = rg_xstati (xc, NREGIONS)
	} else if (rg_xgregions (im, xc, rp, MAX_NREGIONS) > 0) {
	    nregions = rg_xstati (xc, NREGIONS)
	} else if (list != NULL) {
	    iferr {
		if (fntgfnb (list, Memc[fname], SZ_FNAME) != EOF) {
	            fd = open (Memc[fname], READ_ONLY, TEXT_FILE)
	            nregions= rg_xrregions (fd, im, xc, rp, MAX_NREGIONS)
	            call close (fd)
		}
	    } then
		nregions = 0
	} else
	    nregions = 0

	call sfree (sp)

	return (nregions)
end


# RG_XMKREGIONS -- Create a list of regions by marking image sections
# on the image display.

int procedure rg_xmkregions (im, xc, rp, max_nregions, regions, maxch)

pointer	im			#I pointer to the reference image
pointer	xc			#I pointer to the cross-correlation structure
int	rp			#I index of the current region
int	max_nregions		#I the maximum number of regions
char	regions[ARB]		#O the output regions string
int	maxch			#I maximum size of the output regions string

int	op, nregions, wcs, key
pointer	sp, region, section, cmd
real	xll, yll, xur, yur
int	rg_xstati(), clgcur(), gstrcpy()
pointer	rg_xstatp()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (region, SZ_LINE, TY_CHAR)
	call salloc (section, SZ_LINE, TY_CHAR)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Allocate the arrays to hold the regions information,
	call rg_xrealloc (xc, max_nregions)

	# Initialize.
	nregions = min (rp-1, rg_xstati (xc, NREGIONS))
	op = 1

	# Mark the sections on the display.
	while (nregions < max_nregions) {

	    call printf ("Mark lower left corner of region %d [q to quit].\n")
		call pargi (nregions + 1)
	    if (clgcur ("icommands", xll, yll, wcs, key, Memc[cmd],
	        SZ_LINE) == EOF)
		break
	    if (key == 'q')
		break

	    call printf ("Mark upper right corner of region %d [q to quit].\n")
		call pargi (nregions + 1)
	    if (clgcur ("icommands", xur, yur, wcs, key, Memc[cmd],
	        SZ_LINE) == EOF)
		break
	    if (key == 'q')
		break

	    if (xll < 1.0 || xur > IM_LEN(im,1) || yll < 1.0 || yur >
		IM_LEN(im,2))
		break

	    Memi[rg_xstatp(xc,RC1)+nregions] = nint (xll)
	    Memi[rg_xstatp(xc,RC2)+nregions] = nint (xur)
	    Memi[rg_xstatp(xc,RL1)+nregions] = nint (yll)
	    Memi[rg_xstatp(xc,RL2)+nregions] = nint (yur)
	    Memr[rg_xstatp(xc,RZERO)+nregions] = INDEFR
	    Memr[rg_xstatp(xc,RXSLOPE)+nregions] = INDEFR
	    Memr[rg_xstatp(xc,RYSLOPE)+nregions] = INDEFR
	    Memr[rg_xstatp(xc,XSHIFTS)+nregions] = INDEFR
	    Memr[rg_xstatp(xc,YSHIFTS)+nregions] = INDEFR
	    nregions = nregions + 1

	    # Write the first 9 regions into the regions string.
	    call sprintf (Memc[cmd], SZ_LINE, "[%d:%d,%d:%d] ")
		call pargi (nint (xll))
		call pargi (nint (xur))
		call pargi (nint (yll))
		call pargi (nint (yur))
	    op = op + gstrcpy (Memc[cmd], regions[op], maxch - op + 1)
	}
	call printf ("\n")

	# Reallocate the correct amount of space.
	call rg_xseti (xc, NREGIONS, nregions)
	if (nregions > 0)
	    call rg_xrealloc (xc, nregions)
	else 
	    call rg_xrfree (xc)

	call sfree (sp)

	return (nregions)
end


# RG_XGRID - Decode the regions from a grid specification.

int procedure rg_xgrid (im, xc, rp, max_nregions)

pointer	im			#I pointer to the reference image
pointer	xc			#I pointer to the cross-correlation structure
int	rp			#I index of the current region
int	max_nregions		#I the maximum number of regions

int	i, istart, iend, j, jstart, jend, ncols, nlines, nxsample, nysample
int	nxcols, nylines, nregions
pointer	sp, region, section
int	rg_xstati(), nscan(), strcmp()
pointer	rg_xstatp()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (region, SZ_LINE, TY_CHAR)
	call salloc (section, SZ_LINE, TY_CHAR)

	# Allocate the arrays to hold the regions information,
	call rg_xrealloc (xc, max_nregions)

	# Initialize.
	call rg_xstats (xc, REGIONS, Memc[region], SZ_LINE)
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	nregions = min (rp - 1, rg_xstati (xc, NREGIONS))

	# Decode the grid specification.
	call sscan (Memc[region])
	    call gargwrd (Memc[section], SZ_LINE)
	    call gargi (nxsample)
	    call gargi (nysample) 
	if ((nscan() != 3) || (strcmp (Memc[section], "grid") != 0)) {
	    call sfree (sp)
	    return (nregions)
	}

	# Decode the regions.
	if ((nxsample * nysample) > max_nregions) {
	    nxsample = nint (sqrt (real (max_nregions) * real (ncols) /
	        real (nlines)))
	    nysample = real (max_nregions) / real (nxsample)
	}
	nxcols = ncols / nxsample
	nylines = nlines / nysample
	jstart = 1 + (nlines - nysample * nylines) / 2
	jend = jstart + (nysample - 1) * nylines
	do j = jstart, jend, nylines {
	    istart = 1 + (ncols - nxsample * nxcols) / 2
	    iend = istart + (nxsample - 1) * nxcols
	    do i = istart, iend, nxcols {
		Memi[rg_xstatp(xc,RC1)+nregions] = i 
		Memi[rg_xstatp(xc,RC2)+nregions] = i + nxcols - 1
		Memi[rg_xstatp(xc,RL1)+nregions] = j
		Memi[rg_xstatp(xc,RL2)+nregions] = j + nylines - 1
		Memr[rg_xstatp(xc,RZERO)+nregions] = INDEFR
		Memr[rg_xstatp(xc,RXSLOPE)+nregions] = INDEFR
		Memr[rg_xstatp(xc,RYSLOPE)+nregions] = INDEFR
		Memr[rg_xstatp(xc,XSHIFTS)+nregions] = INDEFR
		Memr[rg_xstatp(xc,YSHIFTS)+nregions] = INDEFR
		nregions = nregions + 1
	    }
	}

	call rg_xseti (xc, NREGIONS, nregions)
	if (nregions > 0)
	    call rg_xrealloc (xc, nregions)
	else
	    call rg_xrfree (xc)
	call sfree (sp)

	return (nregions)
end


# RG_XRREGIONS -- Read and decode the regions from a file.

int procedure rg_xrregions (fd, im, xc, rp, max_nregions)

int	fd			#I regions file descriptor
pointer	im			#I pointer to the reference image
pointer	xc			#I pointer to the cross-correlation structure
int	rp			#I index of the current region
int	max_nregions		#I the maximum number of regions

int	ncols, nlines, nregions, x1, y1, x2, y2, step
pointer	sp, line, section
int	rg_xstati(), getline(), rg_xgsections()
pointer	rg_xstatp()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (section, SZ_LINE, TY_CHAR)

	# Allocate the arrays to hold the regions information,
	call rg_xrealloc (xc, max_nregions)

	# Initialize.
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	nregions = min (rp - 1, rg_xstati (xc, NREGIONS))

	# Decode the regions string.
	while ((getline (fd, Memc[line]) != EOF) &&  nregions < max_nregions) {
	    call sscan (Memc[line])
		call gargwrd (Memc[section], SZ_LINE)
	    while ((Memc[section] != EOS) && (nregions < max_nregions)) {
		if (rg_xgsections (Memc[section], x1, x2, step, y1, y2, step,
		    ncols, nlines) == OK) {
		    Memi[rg_xstatp(xc,RC1)+nregions] = x1
		    Memi[rg_xstatp(xc,RC2)+nregions] = x2
		    Memi[rg_xstatp(xc,RL1)+nregions] = y1
		    Memi[rg_xstatp(xc,RL2)+nregions] = y2
		    Memr[rg_xstatp(xc,RZERO)+nregions] = INDEFR
		    Memr[rg_xstatp(xc,RXSLOPE)+nregions] = INDEFR
		    Memr[rg_xstatp(xc,RYSLOPE)+nregions] = INDEFR
		    Memr[rg_xstatp(xc,XSHIFTS)+nregions] = INDEFR
		    Memr[rg_xstatp(xc,YSHIFTS)+nregions] = INDEFR
		    nregions = nregions + 1
		}
		call gargwrd (Memc[section], SZ_LINE)
	    }
	}

	# Reallocate the correct amount of space.
	call rg_xseti (xc, NREGIONS, nregions)
	if (nregions > 0)
	    call rg_xrealloc (xc, nregions)
	else
	    call rg_xrfree (xc)

	call sfree (sp)

	return (nregions)
end


# RG_XGREGIONS -- Decode a list of regions from a string containing
# a list of sections.

int procedure rg_xgregions (im, xc, rp, max_nregions)

pointer	im			#I pointer to the reference image
pointer	xc			#I pointer to cross-correlation structure
int	rp			#I the index of the current region
int	max_nregions		#I the maximum number of regions

int	ncols, nlines, nregions, x1, x2, y1, y2, step
pointer	sp, section, region
int	rg_xstati(), rg_xgsections()
pointer	rg_xstatp()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (region, SZ_LINE, TY_CHAR)
	call salloc (section, SZ_LINE, TY_CHAR)

	# Allocate the arrays to hold the regions information.
	call rg_xrealloc (xc, max_nregions)

	# Initialize.
	call rg_xstats (xc, REGIONS, Memc[region], SZ_LINE)
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	nregions = min (rp - 1, rg_xstati (xc, NREGIONS))

	# Decode the sections
	call sscan (Memc[region])
	    call gargwrd (Memc[section], SZ_LINE)
	while ((Memc[section] != EOS) && (nregions < max_nregions)) {
	    if (rg_xgsections (Memc[section], x1, x2, step, y1, y2, step,
		ncols, nlines) == OK) {
		Memi[rg_xstatp(xc,RC1)+nregions] = x1
		Memi[rg_xstatp(xc,RC2)+nregions] = x2
		Memi[rg_xstatp(xc,RL1)+nregions] = y1
		Memi[rg_xstatp(xc,RL2)+nregions] = y2
		Memr[rg_xstatp(xc,RZERO)+nregions] = INDEFR
		Memr[rg_xstatp(xc,RXSLOPE)+nregions] = INDEFR
		Memr[rg_xstatp(xc,RYSLOPE)+nregions] = INDEFR
		Memr[rg_xstatp(xc,XSHIFTS)+nregions] = INDEFR
		Memr[rg_xstatp(xc,YSHIFTS)+nregions] = INDEFR
		nregions = nregions + 1
	    }
	    call gargwrd (Memc[section], SZ_LINE)
	}


	# Reallocate the correct amount of space.
	call rg_xseti (xc, NREGIONS, nregions)
	if (nregions > 0)
	    call rg_xrealloc (xc, nregions)
	else
	    call rg_xrfree (xc)

	call sfree (sp)

	return (nregions)
end


# RG_XGSECTIONS -- Decode an image section into column and line limits
# and a step size. Sections which describe the whole image are decoded into
# a block ncols * nlines long.

int procedure rg_xgsections (section, x1, x2, xstep, y1, y2, ystep, ncols,
	nlines)

char	section[ARB]		#I the input section string
int	x1, x2			#O the output column section limits
int	xstep			#O the output column step size
int	y1, y2			#O the output line section limits
int	ystep			#O the output line step size
int	ncols, nlines		#I the maximum number of lines and columns

int	ip
int	rg_xgdim()

begin
	ip = 1
	if (rg_xgdim (section, ip, x1, x2, xstep, ncols) == ERR)
	    return (ERR)
	if (rg_xgdim (section, ip, y1, y2, ystep, nlines) == ERR)
	    return (ERR)

	return (OK)
end


# RG_XGDIM -- Decode a single subscript expression to produce the
# range of values for that subscript (X1:X2), and the sampling step size, STEP.
# Note that X1 may be less than, greater than, or equal to X2, and STEP may
# be a positive or negative nonzero integer.  Various shorthand notations are
# permitted, as is embedded whitespace.

int procedure rg_xgdim (section, ip, x1, x2, step, limit)

char	section[ARB]		#I the input image section
int	ip			#I/O pointer to the position in section string
int	x1			#O first limit of dimension
int	x2			#O second limit of dimension
int	step			#O step size of dimension
int	limit			#I maximum size of dimension

int	temp
int	ctoi()

begin
	x1 = 1
	x2 = limit
	step = 1

	while (IS_WHITE(section[ip]))
	    ip = ip + 1

	if (section[ip] =='[')
	    ip = ip + 1

	while (IS_WHITE(section[ip]))
	    ip = ip + 1

	# Get X1, X2.
	if (ctoi (section, ip, temp) > 0) {			# [x1
	    x1 = max (1, min (temp, limit))
	    if (section[ip] == ':') {	
		ip = ip + 1
		if (ctoi (section, ip, temp) == 0)		# [x1:x2
		    return (ERR)
		x2 = max (1, min (temp, limit))
	    } else
		x2 = x1

	} else if (section[ip] == '-') {
	    x1 = limit
	    x2 = 1
	    ip = ip + 1
	    if (section[ip] == '*')
		ip = ip + 1

	} else if (section[ip] == '*')				# [*
	    ip = ip + 1

	while (IS_WHITE(section[ip]))
	    ip = ip + 1

	# Get sample step size, if give.
	if (section[ip] == ':') {				# ..:step
	    ip = ip + 1
	    if (ctoi (section, ip, step) == 0)
		return (ERR)
	    else if (step == 0)
		return (ERR)
	}

	# Allow notation such as "-*:5", (or even "-:5") where the step
	# is obviously supposed to be negative.

	if (x1 > x2 && step > 0)
	    step = -step
	
	while (IS_WHITE(section[ip]))
	    ip = ip + 1

	if (section[ip] == ',') {
	    ip = ip + 1
	    return (OK)
	} else if (section[ip] == ']')
	    return (OK)
	else
	    return (ERR)
end
