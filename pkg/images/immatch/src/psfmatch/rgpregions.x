include <fset.h>
include <imhdr.h>
include "psfmatch.h"

# RG_PREGIONS -- Decoode the regions specification.  If the sections
# string is NULL then a default region dnx by dny pixels wide centered
# on the reference image is used. Otherwise the section centers are
# read from the regions string or from the objects list.

int procedure rg_pregions (list, im, pm, rp, reread)

int	list			#I pointer to regions file list
pointer	im			#I pointer to the image
pointer	pm			#I pointer to the psfmatch structure
int	rp			#I region pointer
int	reread			#I reread the current file

char	fname[SZ_FNAME]
int	nregions, fd
int	open(), rg_prregions(), rg_pgregions(), fntgfnb()
int	rg_pstati()
data	fname[1] /EOS/
errchk	open(), fntgfnb(), close()

begin
	if (rp < 1 || rp > MAX_NREGIONS) {
	    nregions = 0
	} else if (rg_pgregions (im, pm, rp, MAX_NREGIONS) > 0) {
	    nregions = rg_pstati (pm, NREGIONS)
	} else if (list != NULL) {
	    if (reread == NO) {
	        iferr {
		    if (fntgfnb (list, fname, SZ_FNAME) != EOF) {
	                fd = open (fname, READ_ONLY, TEXT_FILE)
	                nregions= rg_prregions (fd, im, pm, rp, MAX_NREGIONS)
	                call close (fd)
		    }
	        } then
		    nregions = 0
	    } else if (fname[1] != EOS) {
	        iferr {
	            fd = open (fname, READ_ONLY, TEXT_FILE)
	            nregions= rg_prregions (fd, im, pm, rp, MAX_NREGIONS)
	            call close (fd)
		} then
		    nregions = 0
	    }
	} else
	    nregions = 0

	return (nregions)
end


# RG_PMKREGIONS -- Create a list of psf objects by selecting objects with
# the image display cursor.

int procedure rg_pmkregions (fd, im, pm, rp, max_nregions)

int	fd			#I the output coordinates file descriptor
pointer	im			#I pointer to the image
pointer	pm			#I pointer to the psf matching structure
int	rp			#I pointer to current region
int	max_nregions		#I maximum number of regions

int	nregions, wcs, key, x1, x2, y1, y2
pointer	sp, region, cmd
real	x, y, xc, yc
int	clgcur(), rg_pstati()
pointer	rg_pstatp()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (region, SZ_FNAME, TY_CHAR)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Allocate the arrays to hold the regions information,
	call rg_prealloc (pm, max_nregions)

	nregions = min (rp-1, rg_pstati (pm, NREGIONS))
	while (nregions < max_nregions) {

	    # Identify the object.
	    call printf ("Mark object %d [any key=mark,q=quit]:\n")
		call pargi (nregions + 1)
	    if (clgcur ("icommands", x, y, wcs, key, Memc[cmd], SZ_LINE) == EOF)
		break
	    if (key == 'q')
		break

	    # Center the object.
	    if (rg_pstati (pm, CENTER) == YES) {
	        call rg_pcntr (im, x, y, max (rg_pstati(pm, PNX),
	            rg_pstati(pm, PNY)), xc, yc)
	    } else {
		xc = x
		yc = y
	    }

	    # Compute the data section.
	    x1 = xc - rg_pstati (pm, DNX) / 2
	    x2 = x1 + rg_pstati (pm, DNX) - 1
	    y1 = yc - rg_pstati (pm, DNY) / 2
	    y2 = y1 + rg_pstati (pm, DNY) - 1

	    # Make sure that the region is on the image.
	    if (x1 < 1 || x2 > IM_LEN(im,1) || y1 < 1 || y2 >
		IM_LEN(im,2))
		next

	    if (fd != NULL) {
	        call fprintf (fd, "%0.3f  %0.3f\n")
		    call pargr (xc)
		    call pargr (yc)
	    }

	    Memi[rg_pstatp(pm,RC1)+nregions] = x1
	    Memi[rg_pstatp(pm,RC2)+nregions] = x2
	    Memi[rg_pstatp(pm,RL1)+nregions] = y1
	    Memi[rg_pstatp(pm,RL2)+nregions] = y2
	    Memr[rg_pstatp(pm,RZERO)+nregions] = INDEFR
	    Memr[rg_pstatp(pm,RXSLOPE)+nregions] = INDEFR
	    Memr[rg_pstatp(pm,RYSLOPE)+nregions] = INDEFR
	    nregions = nregions + 1

	}

	# Reallocate the correct amount of space.
	call rg_pseti (pm, NREGIONS, nregions)
	if (nregions > 0) {
	    call rg_prealloc (pm, nregions)
	    if (fd != NULL) {
	        call fstats (fd, F_FILENAME, Memc[region], SZ_FNAME)
	        call rg_psets (pm, PSFDATA, Memc[region])
	    } else
	        call rg_psets (pm, PSFDATA, "")
	} else { 
	    call rg_prfree (pm)
	    call rg_psets (pm, PSFDATA, "")
	}

	call sfree (sp)
	return (nregions)
end


# RG_PRREGIONS -- Procedure to read the regions from a file.

int procedure rg_prregions (fd, im, pm, rp, max_nregions)

int	fd			#I regions file descriptor
pointer	im			#I pointer to the image
pointer	pm			#I pointer to psf matching structure
int	rp			#I pointer to current region
int	max_nregions		#I maximum number of regions

int	nregions, x1, y1, x2, y2
pointer	sp, line
real	x, y, xc, yc
int	rg_pstati(), getline()
pointer	rg_pstatp()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Allocate the arrays to hold the regions information,
	call rg_prealloc (pm, max_nregions)

	# Decode the regions string.
	nregions = min (rp - 1, rg_pstati (pm, NREGIONS))
	while (getline (fd, Memc[line]) != EOF) {

	    if (nregions >= max_nregions)
		break

	    call sscan (Memc[line])
		call gargr (x)
		call gargr (y)
	    if (rg_pstati (pm, CENTER) == YES) {
		call rg_pcntr (im, x, y, max (rg_pstati(pm, PNX),
		    rg_pstati(pm, PNY)), xc, yc)
	    } else {
		xc = x
		yc = y
	    }

	    # Compute the data section.
	    x1 = xc - rg_pstati (pm, DNX) / 2
	    x2 = x1 + rg_pstati (pm, DNX) - 1
	    if (IM_NDIM(im) == 1) {
		y1 = 1
		y2 = 1
	    } else {
	        y1 = yc - rg_pstati (pm, DNY) / 2
	        y2 = y1 + rg_pstati (pm, DNY) - 1
	    }

	    # Make sure that the region is on the image.
	    if (x1 < 1 || x2 > IM_LEN(im,1) || y1 < 1 || y2 >
		IM_LEN(im,2))
		next

	    # Add the new region to the list.
	    Memi[rg_pstatp(pm,RC1)+nregions] = x1
	    Memi[rg_pstatp(pm,RC2)+nregions] = x2
	    Memi[rg_pstatp(pm,RL1)+nregions] = y1
	    Memi[rg_pstatp(pm,RL2)+nregions] = y2
	    Memr[rg_pstatp(pm,RZERO)+nregions] = INDEFR
	    Memr[rg_pstatp(pm,RXSLOPE)+nregions] = INDEFR
	    Memr[rg_pstatp(pm,RYSLOPE)+nregions] = INDEFR
	    nregions = nregions + 1
	}

	call rg_pseti (pm, NREGIONS, nregions)
	if (nregions > 0)
	    call rg_prealloc (pm, nregions)
	else
	    call rg_prfree (pm)

	call sfree (sp)
	return (nregions)
end


# RG_PGREGIONS -- Procedure to compute the column and line limits given
# an x and y position and a default size.

int procedure rg_pgregions (im, pm, rp, max_nregions)

pointer	im			#I pointer to the image
pointer	pm			#I pointer to psf matching structure
int	rp			#I pointer to the current region
int	max_nregions		#I maximum number of regions

int	ncols, nlines, nregions
int	x1, x2, y1, y2
pointer	sp, region
real	x, y, xc, yc
int	rg_pstati(), nscan()
pointer	rg_pstatp()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (region, SZ_LINE, TY_CHAR)

	# Allocate the arrays to hold the regions information.
	call rg_prealloc (pm, max_nregions)

	# Get the constants.
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	# Decode the center.
	call rg_pstats (pm, PSFDATA, Memc[region], SZ_LINE)
	nregions = min (rp - 1, rg_pstati (pm, NREGIONS))
	call sscan (Memc[region])
	call gargr (x)
	call gargr (y)

	# Compute the data region.
	if (nscan() >= 2) {

	    # Compute a more accurate center.
	    if (rg_pstati (pm, CENTER) == YES) {
	        call rg_pcntr (im, x, y, max (rg_pstati(pm, PNX),
	            rg_pstati(pm, PNY)), xc, yc)
	    } else {
		xc = x
		yc = y
	    }

	    # Compute the data section.
	    x1 = xc - rg_pstati (pm, DNX) / 2
	    x2 = x1 + rg_pstati (pm, DNX) - 1
	    if (IM_NDIM(im) == 1) {
		y1 = 1
		y2 = 1
	    } else {
	        y1 = yc - rg_pstati (pm, DNY) / 2
	        y2 = y1 + rg_pstati (pm, DNY) - 1
	    }

	    # Make sure that the region is on the image.
	    if (x1 >= 1 && x2 <= IM_LEN(im,1) && y1 >= 1 &&
	        y2 <= IM_LEN(im,2)) {
	        Memi[rg_pstatp(pm,RC1)+nregions] = x1
	        Memi[rg_pstatp(pm,RC2)+nregions] = x2
	        Memi[rg_pstatp(pm,RL1)+nregions] = y1
	        Memi[rg_pstatp(pm,RL2)+nregions] = y2
	        Memr[rg_pstatp(pm,RZERO)+nregions] = INDEFR
	        Memr[rg_pstatp(pm,RXSLOPE)+nregions] = INDEFR
	        Memr[rg_pstatp(pm,RYSLOPE)+nregions] = INDEFR
	        nregions = nregions + 1
	    }
	}


	# Reallocate the correct amount of space.
	call rg_pseti (pm, NREGIONS, nregions)
	if (nregions > 0)
	    call rg_prealloc (pm, nregions)
	else
	    call rg_prfree (pm)

	call sfree (sp)

	return (nregions)
end


# RG_PCNTR -- Compute star center using MPC algorithm.

procedure rg_pcntr (im, xstart, ystart, boxsize, xcntr, ycntr)

pointer im				#I pointer to the input image
real    xstart, ystart			#I initial position
int     boxsize				#I width of the centering box
real    xcntr, ycntr			#O computed center

int     x1, x2, y1, y2, half_box
int     ncols, nrows, nx, ny, try
real    xinit, yinit
pointer bufptr, sp, x_vect, y_vect
int     imgs2r()

begin
	# Inialize.
        half_box = (boxsize - 1) / 2
        xinit = xstart
        ncols = IM_LEN (im, 1)
	if (IM_NDIM(im) == 1) {
	    yinit = 1
	    nrows = 1
	} else {
            yinit = ystart
            nrows = IM_LEN (im, 2)
	}
        try = 0

	# Iterate until pixel shifts are less than one.
        repeat {

	    # Define region to extract.
            x1 = max (xinit - half_box, 1.0) +0.5
            x2 = min (xinit + half_box, real(ncols)) +0.5
            y1 = max (yinit - half_box, 1.0) +0.5
            y2 = min (yinit + half_box, real(nrows)) +0.5
            nx = x2 - x1 + 1
            ny = y2 - y1 + 1

            # Extract region around center
            bufptr = imgs2r (im, x1, x2, y1, y2)

            # Compute the new center.
            call smark (sp)
	    if (IM_NDIM(im) == 1) {
                call salloc (x_vect, nx, TY_REAL)
 	        call aclrr (Memr[x_vect], nx)
                call rg_prowsum (Memr[bufptr], Memr[x_vect], nx, ny)
                call rg_pcenter (Memr[x_vect], nx, xcntr)
		ycntr = 1
	    } else {
                call salloc (x_vect, nx, TY_REAL)
                call salloc (y_vect, ny, TY_REAL)
 	        call aclrr (Memr[x_vect], nx)
                call aclrr (Memr[y_vect], ny)
                call rg_prowsum (Memr[bufptr], Memr[x_vect], nx, ny)
                call rg_pcolsum (Memr[bufptr], Memr[y_vect], nx, ny)
                call rg_pcenter (Memr[x_vect], nx, xcntr)
                call rg_pcenter (Memr[y_vect], ny, ycntr)
	    }
            call sfree (sp)

            # Check for INDEF centers.
            if (IS_INDEFR(xcntr) || IS_INDEFR(ycntr)) {
                xcntr = xinit
                ycntr = yinit
                break
            }

            # Add in offsets
            xcntr = xcntr + x1
            ycntr = ycntr + y1

            try = try + 1
            if (try == 1) {
                if ((abs(xcntr-xinit) > 1.0) || (abs(ycntr-yinit) > 1.0)) {
                    xinit = xcntr
                    yinit = ycntr
                }
            } else
                break
        }
end


# RG_PROWSUM -- Sum all rows in a raster.

procedure rg_prowsum (v, row, nx, ny)

real    v[nx,ny]			#I the input subraster
real    row[ARB]			#O the output row sum
int     nx, ny				#I the dimensions of the subraster

int     i, j

begin
        do i = 1, ny
            do j = 1, nx
                row[j] = row[j] + v[j,i]
end


# RG_PCOLSUM -- Sum all columns in a raster.

procedure rg_pcolsum (v, col, nx, ny)

real    v[nx,ny]			#I the input subraster
real    col[ARB]			#O the output column sum
int     nx, ny				#I the dimensions of the subraster

int     i, j

begin
        do i = 1, ny
            do j = 1, nx
                col[j] = col[j] + v[i,j]
end


# RG_PCENTER -- Compute center of gravity of array.

procedure rg_pcenter (v, nv, vc)

real    v[ARB]				#I the input vector
int     nv				#I the length of the vector
real    vc				#O the output center

int     i
real    sum1, sum2, sigma, cont

begin
        # Compute first moment
        sum1 = 0.0
        sum2 = 0.0

        call aavgr (v, nv, cont, sigma)

        do i = 1, nv
            if (v[i] > cont) {
                sum1 = sum1 + (i-1) * (v[i] - cont)
                sum2 = sum2 + (v[i] - cont)
            }

        # Determine center
        if (sum2 == 0.0)
            vc = INDEFR
        else
            vc = sum1 / sum2
end
