include <ctype.h>
include <fset.h>
include <imhdr.h>
include "linmatch.h"

# RG_LREGIONS -- Decode the input regions description. If the regions string
# is NULL then the regions list is empty. The regions are specified in section
# notation, grid notation, coordinate notation or are read
# from a file.

long procedure rg_lregions (list, im, ls, rp, reread)

pointer	list			#I pointer to the regions file list
pointer	im			#I pointer to the reference image
pointer	ls			#I pointer to the linscale structure
long	rp			#I region pointer
int	reread			#I reread the current file

size_t	sz_val
long	l_val
char	fname[SZ_FNAME]
size_t	max_nregions, nregions
int	fd
pointer	sp, regions
long	rg_lgrid(), rg_lgregions(), rg_lsregions(), rg_lrcoords(), rg_lstatl()
long	rg_lrsections()
int	fntgfnb(), open()
data	fname[1] /EOS/
errchk	fntgfnb(), seek(),  open(), close()

begin
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (regions, sz_val, TY_CHAR)

	call rg_lstats (ls, REGIONS, Memc[regions], SZ_LINE)
	max_nregions = rg_lstatl (ls, MAXNREGIONS)

	if (rp < 1 || rp > max_nregions || Memc[regions] == EOS) {
	    nregions = 0
	} else if (rg_lgrid (im, ls, rp, max_nregions) > 0) {
	    nregions = rg_lstatl (ls, NREGIONS)
	} else if (rg_lgregions (im, ls, rp, max_nregions) > 0) {
	    nregions = rg_lstatl (ls, NREGIONS)
	} else if (rg_lsregions (im, ls, rp, max_nregions) > 0) {
	    nregions = rg_lstatl (ls, NREGIONS)
	} else if (list != NULL) {
	    if (reread == NO) {
	        iferr {
		    if (fntgfnb (list, fname, SZ_FNAME) != EOF) {
	                fd = open (fname, READ_ONLY, TEXT_FILE)
	                nregions= rg_lrsections (fd, im, ls, rp, max_nregions)
		        if (nregions <= 0) {
			    l_val = BOF
			    call seek (fd, l_val)
	                    nregions= rg_lrcoords (fd, im, ls, rp, max_nregions)
		        }
	                call close (fd)
		    } else
		        nregions = 0
	        } then
	            nregions = 0
	    } else if (fname[1] != EOS) {
		iferr {
	            fd = open (fname, READ_ONLY, TEXT_FILE)
	            nregions= rg_lrsections (fd, im, ls, rp, max_nregions)
		    if (nregions <= 0) {
			l_val = BOF
			call seek (fd, l_val)
	                nregions= rg_lrcoords (fd, im, ls, rp, max_nregions)
		    }
	            call close (fd)
		} then
		    nregions = 0
	    }
	} else
	    nregions = 0

	call sfree (sp)

	return (nregions)
end


# RG_LGRID - Decode the regions from a grid specification.

long procedure rg_lgrid (im, ls, rp, max_nregions)

pointer im                      #I pointer to the reference image
pointer ls                      #I pointer to the linscale structure
long	rp                      #I index of the current region
size_t	max_nregions            #I the maximum number of regions

size_t	sz_val
long	i, istart, iend, j, jstart, jend
long	ncols, nlines, nxsample, nysample
long	nxcols, nylines, nregions
pointer sp, region, section
int	nscan(), strcmp()
long	rg_lstatl(), lnint()
pointer rg_lstatp()

begin
        # Allocate working space.
        call smark (sp)
        sz_val = SZ_LINE
        call salloc (region, sz_val, TY_CHAR)
        call salloc (section, sz_val, TY_CHAR)

        # Allocate the arrays to hold the regions information,
        call rg_lrealloc (ls, max_nregions)

        # Initialize.
        call rg_lstats (ls, REGIONS, Memc[region], SZ_LINE)
        ncols = IM_LEN(im,1)
        nlines = IM_LEN(im,2)
        nregions = min (rp - 1, rg_lstatl (ls, NREGIONS))

        # Decode the grid specification.
        call sscan (Memc[region])
            call gargwrd (Memc[section], SZ_LINE)
            call gargl (nxsample)
            call gargl (nysample)
        if ((nscan() != 3) || (strcmp (Memc[section], "grid") != 0)) {
            call sfree (sp)
            return (nregions)
        }

        # Decode the regions.
        if ((nxsample * nysample) > max_nregions) {
            nxsample = lnint(sqrt (real (max_nregions) * real (ncols) /
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
                Meml[rg_lstatp(ls,RC1)+nregions] = i
                Meml[rg_lstatp(ls,RC2)+nregions] = i + nxcols - 1
                Meml[rg_lstatp(ls,RL1)+nregions] = j
                Meml[rg_lstatp(ls,RL2)+nregions] = j + nylines - 1
                Meml[rg_lstatp(ls,RXSTEP)+nregions] = 1
                Meml[rg_lstatp(ls,RYSTEP)+nregions] = 1
		Memr[rg_lstatp(ls,RMEAN)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RMEDIAN)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RMODE)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RSIGMA)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RSKY)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RSKYERR)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RMAG)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RMAGERR)+nregions] = INDEFR
		Meml[rg_lstatp(ls,RNPTS)+nregions] = INDEFL
		Memr[rg_lstatp(ls,IMEAN)+nregions] = INDEFR
		Memr[rg_lstatp(ls,IMEDIAN)+nregions] = INDEFR
		Memr[rg_lstatp(ls,IMODE)+nregions] = INDEFR
		Memr[rg_lstatp(ls,ISIGMA)+nregions] = INDEFR
		Memr[rg_lstatp(ls,ISKY)+nregions] = INDEFR
		Memr[rg_lstatp(ls,ISKYERR)+nregions] = INDEFR
		Memr[rg_lstatp(ls,IMAG)+nregions] = INDEFR
		Memr[rg_lstatp(ls,IMAGERR)+nregions] = INDEFR
		Meml[rg_lstatp(ls,INPTS)+nregions] = INDEFL
		Memr[rg_lstatp(ls,RBSCALE)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RBSCALEERR)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RBZERO)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RBZEROERR)+nregions] = INDEFR
		Memi[rg_lstatp(ls,RDELETE)+nregions] = LS_NO
		Memr[rg_lstatp(ls,RCHI)+nregions] = INDEFR
                nregions = nregions + 1
            }
        }

        call rg_lsetl (ls, NREGIONS, nregions)
        if (nregions > 0)
            call rg_lrealloc (ls, nregions)
        else
            call rg_lrfree (ls)
        call sfree (sp)

        return (nregions)
end


# RG_LGREGIONS -- Compute the column and line limits givenan x and y
# coordinate and a default size.

long procedure rg_lgregions (im, ls, rp, max_nregions)

pointer im                      #I pointer to the image
pointer ls                      #I pointer to the linscale structure
long	rp                      #I pointer to the current region
size_t	max_nregions            #I maximum number of regions

size_t	sz_val
char	comma
size_t	nregions
long	ncols, nlines, x1, x2, y1, y2
pointer sp, region
real    x, y, xc, yc
int	nscan(), onscan()
#int	rg_lstati()
long	rg_lstatl()
pointer rg_lstatp()

begin
        # Allocate working space.
        call smark (sp)
        sz_val = SZ_LINE
        call salloc (region, sz_val, TY_CHAR)

        # Allocate the arrays to hold the regions information.
        call rg_lrealloc (ls, max_nregions)

        # Get the constants.
        ncols = IM_LEN(im,1)
        nlines = IM_LEN(im,2)

        # Decode the center.
        call rg_lstats (ls, REGIONS, Memc[region], SZ_LINE)
        nregions = min (rp - 1, rg_lstatl (ls, NREGIONS))
	onscan = 0
        call sscan (Memc[region])
            call gargr (x)
            call gargr (y)
	    call gargc (comma)

	# Compute the data region.
        while ((nscan() == onscan + 3) && (nregions < max_nregions)) {

	    # Check for the comma.
	    if (comma != ',')
		break

            # Compute a more accurate center.
            #if (rg_lstati (ls, CENTER) == YES) {
                #call rg_lcntr (im, x, y, DEF_CRADIUS, xc, yc)
            #} else {
                xc = x
                yc = y
            #}

            # Compute the data section.
            x1 = xc - rg_lstatl (ls, DNX) / 2
            x2 = x1 + rg_lstatl (ls, DNX) - 1
            if (IM_NDIM(im) == 1) {
                y1 = 1
                y2 = 1
            } else {
                y1 = yc - rg_lstatl (ls, DNY) / 2
                y2 = y1 + rg_lstatl (ls, DNY) - 1
            }

            # Make sure that the region is on the image.
            if (x1 >= 1 && x2 <= IM_LEN(im,1) && y1 >= 1 &&
                y2 <= IM_LEN(im,2)) {
                Meml[rg_lstatp(ls,RC1)+nregions] = x1
                Meml[rg_lstatp(ls,RC2)+nregions] = x2
                Meml[rg_lstatp(ls,RL1)+nregions] = y1
                Meml[rg_lstatp(ls,RL2)+nregions] = y2
                Meml[rg_lstatp(ls,RXSTEP)+nregions] = 1
                Meml[rg_lstatp(ls,RYSTEP)+nregions] = 1
		Memr[rg_lstatp(ls,RMEAN)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RMEDIAN)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RMODE)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RSIGMA)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RSKY)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RSKYERR)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RMAG)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RMAGERR)+nregions] = INDEFR
		Meml[rg_lstatp(ls,RNPTS)+nregions] = INDEFL
		Memr[rg_lstatp(ls,IMEAN)+nregions] = INDEFR
		Memr[rg_lstatp(ls,IMEDIAN)+nregions] = INDEFR
		Memr[rg_lstatp(ls,IMODE)+nregions] = INDEFR
		Memr[rg_lstatp(ls,ISIGMA)+nregions] = INDEFR
		Memr[rg_lstatp(ls,ISKY)+nregions] = INDEFR
		Memr[rg_lstatp(ls,ISKYERR)+nregions] = INDEFR
		Memr[rg_lstatp(ls,IMAG)+nregions] = INDEFR
		Memr[rg_lstatp(ls,IMAGERR)+nregions] = INDEFR
		Meml[rg_lstatp(ls,INPTS)+nregions] = INDEFL
		Memr[rg_lstatp(ls,RBSCALE)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RBSCALEERR)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RBZERO)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RBZEROERR)+nregions] = INDEFR
		Memi[rg_lstatp(ls,RDELETE)+nregions] = LS_NO
		Memr[rg_lstatp(ls,RCHI)+nregions] = INDEFR
                nregions = nregions + 1
            }

	    onscan = nscan()
            call gargr (x)
            call gargr (y)
	    call gargc (comma)
        }

        # Reallocate the correct amount of space.
        call rg_lsetl (ls, NREGIONS, nregions)
        if (nregions > 0)
            call rg_lrealloc (ls, nregions)
        else
            call rg_lrfree (ls)

        call sfree (sp)

        return (nregions)
end


# RG_LMKREGIONS -- Procedure to mark the sections on the image display.
# Sections are marked by pointing the image display cursor to the
# lower left and upper rights corners of the desired sections respectively.

long procedure rg_lmkregions (fd, im, ls, rp, max_nregions, regions, maxch)

int	fd			#I pointer to the output text file
pointer	im			#I pointer to the image
pointer	ls			#I pointer to the intensity scaling structure
long	rp			#I pointer to current region
size_t	max_nregions		#I maximum number of regions
char	regions[ARB]		#O the output regions string
int	maxch			#I the maximum size of the output string

size_t	sz_val
long	nregions
int	op, wcs, key
pointer	sp, cmd
real	xll, yll, xur, yur
int	clgcur(), gstrcpy()
long	rg_lstatl(), lnint()
pointer	rg_lstatp()

begin
	# Allocate working space.
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (cmd, sz_val, TY_CHAR)

	# Allocate the arrays to hold the regions information,
	call rg_lrealloc (ls, max_nregions)

	# Initialize.
	nregions = min (rp-1, rg_lstatl (ls, NREGIONS))
	op = 1
	regions[1] = EOS

	while (nregions < max_nregions) {

	    call printf ("Mark lower left corner of region %d [q to quit].\n")
		call pargl (nregions + 1)
	    if (clgcur ("icommands", xll, yll, wcs, key, Memc[cmd],
	        SZ_LINE) == EOF)
		break
	    if (key == 'q')
		break

	    call printf ("Mark upper right corner of region %d [q to quit].\n")
		call pargl (nregions + 1)
	    if (clgcur ("icommands", xur, yur, wcs, key, Memc[cmd],
	        SZ_LINE) == EOF)
		break
	    if (key == 'q')
		break

	    # Make sure that the region is on the image.
	    if (xll < 1.0 || xur > IM_LEN(im,1) || yll < 1.0 || yur >
		IM_LEN(im,2))
		next

	    Meml[rg_lstatp(ls,RC1)+nregions] = lnint(xll)
	    Meml[rg_lstatp(ls,RC2)+nregions] = lnint(xur)
	    Meml[rg_lstatp(ls,RXSTEP)+nregions] = 1
	    Meml[rg_lstatp(ls,RL1)+nregions] = lnint(yll)
	    Meml[rg_lstatp(ls,RL2)+nregions] = lnint(yur)
	    Meml[rg_lstatp(ls,RYSTEP)+nregions] = 1

	    Memr[rg_lstatp(ls,RMEAN)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RMEDIAN)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RMODE)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RSIGMA)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RSKY)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RSKYERR)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RMAG)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RMAGERR)+nregions] = INDEFR
	    Meml[rg_lstatp(ls,RNPTS)+nregions] = INDEFL

	    Memr[rg_lstatp(ls,IMEAN)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,IMEDIAN)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,IMODE)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,ISIGMA)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,ISKY)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,ISKYERR)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,IMAG)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,IMAGERR)+nregions] = INDEFR
	    Meml[rg_lstatp(ls,INPTS)+nregions] = INDEFL

	    Memr[rg_lstatp(ls,RBSCALE)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RBSCALEERR)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RBZERO)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RBZEROERR)+nregions] = INDEFR
	    Memi[rg_lstatp(ls,RDELETE)+nregions] = LS_NO
	    Memr[rg_lstatp(ls,RCHI)+nregions] = INDEFR
	    nregions = nregions + 1

	    # Write the regions string.
	    call sprintf (Memc[cmd], SZ_LINE, "[%d:%d,%d:%d] ")
		call pargl (lnint(xll))
		call pargl (lnint(xur))
		call pargl (lnint(yll))
		call pargl (lnint(yur))
	    op = op + gstrcpy (Memc[cmd], regions[op], maxch - op + 1)

	    # Write the output record.
	    if (fd != NULL) {
		call fprintf (fd, "[%d:%d,%d:%d]\n")
		    call pargl (lnint(xll)) 
		    call pargl (lnint(xur)) 
		    call pargl (lnint(yll)) 
		    call pargl (lnint(yur)) 
	    }
	}
	call printf ("\n")

	# Reallocate the correct amount of space.
	call rg_lsets (ls, REGIONS, regions)
	call rg_lsetl (ls, NREGIONS, nregions)

	if (nregions > 0) {
	    sz_val = nregions
	    call rg_lrealloc (ls, sz_val)
	} else {
	    call rg_lrfree (ls)
	}

	call sfree (sp)

	return (nregions)
end


# RG_LMKXY -- Create a list of objects by selecting objects with
# the image display cursor.

long procedure rg_lmkxy (fd, im, ls, rp, max_nregions)

int	fd                      #I the output coordinates file descriptor
pointer im                      #I pointer to the image
pointer	ls                      #I pointer to the psf matching structure
long	rp                      #I pointer to current region
size_t	max_nregions            #I maximum number of regions

size_t	sz_val
long	nregions
int	wcs, key
long	x1, x2, y1, y2
pointer sp, region, cmd
real    xc, yc
int	clgcur()
long	rg_lstatl()
pointer rg_lstatp()

begin
        # Allocate working space.
        call smark (sp)
        sz_val = SZ_FNAME
        call salloc (region, sz_val, TY_CHAR)
        sz_val = SZ_LINE
        call salloc (cmd, sz_val, TY_CHAR)

        # Allocate the arrays to hold the regions information,
        call rg_lrealloc (ls, max_nregions)

        nregions = min (rp-1, rg_lstatl (ls, NREGIONS))
        while (nregions < max_nregions) {

            # Identify the object.
            call printf ("Mark object %d [any key=mark,q=quit]:\n")
                call pargl (nregions + 1)
            if (clgcur ("icommands", xc, yc, wcs, key, Memc[cmd],
		SZ_LINE) == EOF)
                break
            if (key == 'q')
                break

            # Compute the data section.
            x1 = xc - rg_lstatl (ls, DNX) / 2
            x2 = x1 + rg_lstatl (ls, DNX) - 1
            y1 = yc - rg_lstatl (ls, DNY) / 2
            y2 = y1 + rg_lstatl (ls, DNY) - 1

            # Make sure that the region is on the image.
            if (x1 < 1 || x2 > IM_LEN(im,1) || y1 < 1 || y2 >
                IM_LEN(im,2))
                next

            if (fd != NULL) {
                call fprintf (fd, "%0.3f  %0.3f\n")
                    call pargr (xc)
                    call pargr (yc)
            }

	    Meml[rg_lstatp(ls,RC1)+nregions] = x1
	    Meml[rg_lstatp(ls,RC2)+nregions] = x2
	    Meml[rg_lstatp(ls,RXSTEP)+nregions] = 1
	    Meml[rg_lstatp(ls,RL1)+nregions] = y1
	    Meml[rg_lstatp(ls,RL2)+nregions] = y2
	    Meml[rg_lstatp(ls,RYSTEP)+nregions] = 1

	    Memr[rg_lstatp(ls,RMEAN)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RMEDIAN)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RMODE)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RSIGMA)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RSKY)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RSKYERR)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RMAG)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RMAGERR)+nregions] = INDEFR
	    Meml[rg_lstatp(ls,RNPTS)+nregions] = INDEFL

	    Memr[rg_lstatp(ls,IMEAN)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,IMEDIAN)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,IMODE)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,ISIGMA)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,ISKY)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,ISKYERR)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,IMAG)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,IMAGERR)+nregions] = INDEFR
	    Meml[rg_lstatp(ls,INPTS)+nregions] = INDEFL

	    Memr[rg_lstatp(ls,RBSCALE)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RBSCALEERR)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RBZERO)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RBZEROERR)+nregions] = INDEFR
	    Memi[rg_lstatp(ls,RDELETE)+nregions] = LS_NO
	    Memr[rg_lstatp(ls,RCHI)+nregions] = INDEFR

            nregions = nregions + 1

        }

	# Reallocate the correct amount of space.
        call rg_lsetl (ls, NREGIONS, nregions)
        if (nregions > 0) {
	    sz_val = nregions
            call rg_lrealloc (ls, sz_val)
            if (fd != NULL) {
                call fstats (fd, F_FILENAME, Memc[region], SZ_FNAME)
                call rg_lsets (ls, REGIONS, Memc[region])
            } else
                call rg_lsets (ls, REGIONS, "")
        } else {
            call rg_lrfree (ls)
            call rg_lsets (ls, REGIONS, "")
        }

        call sfree (sp)
        return (nregions)
end


# RG_LRSECTIONS -- Read the sections from a file.

long procedure rg_lrsections (fd, im, ls, rp, max_nregions)

int	fd			#I the regions file descriptor
pointer	im			#I pointer to the image
pointer	ls			#I pointer to the linscale structure
long	rp			#I pointer to current region
size_t	max_nregions		#I the  maximum number of regions

size_t	sz_val
long	c_2
int	stat
size_t	nregions
long	ncols, nlines, x1, y1, x2, y2, xstep, ystep
pointer	sp, section, line
int	getline(), rg_lgsections()
long	rg_lstatl(), lmod()
pointer	rg_lstatp()

begin
	c_2 = 2
	# Allocate working space.
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (line, sz_val, TY_CHAR)
	call salloc (section, sz_val, TY_CHAR)

	# Allocate the arrays to hold the regions information,
	call rg_lrealloc (ls, max_nregions)

	# Get the constants.
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	# Decode the regions string.
	nregions = min (rp - 1, rg_lstatl (ls, NREGIONS))
	while (getline (fd, Memc[line]) != EOF &&  nregions < max_nregions) {

	    call sscan (Memc[line])
		call gargwrd (Memc[section], SZ_LINE)

	    while (Memc[section] != EOS && nregions < max_nregions) {
		stat = rg_lgsections (Memc[section], x1, x2, xstep, y1, y2,
		    ystep, ncols, nlines)

		# Check for even dimensioned regions.
		if (stat == OK) {
		    if (lmod(x2 - x1 + 1, c_2) == 2) {
			x2 = x2 + 1
			if (x2 > ncols)
			    x2 = x2 - 2
			if (x2 < 1)
			    stat = ERR
		    }
		    if (lmod(y2 - y1 + 1, c_2) == 2) {
			y2 = y2 + 1
			if (y2 > nlines)
			    y2 = y2 - 2
			if (y2 < 1)
			    stat = ERR
		    }
		} else
		    stat = ERR

		# Add the new region to the list.
		if (stat == OK) {
		    Meml[rg_lstatp(ls,RC1)+nregions] = x1
		    Meml[rg_lstatp(ls,RC2)+nregions] = x2
		    Meml[rg_lstatp(ls,RL1)+nregions] = y1
		    Meml[rg_lstatp(ls,RL2)+nregions] = y2
		    Meml[rg_lstatp(ls,RXSTEP)+nregions] = xstep
		    Meml[rg_lstatp(ls,RYSTEP)+nregions] = ystep
		    Memr[rg_lstatp(ls,RMEAN)+nregions] = INDEFR
		    Memr[rg_lstatp(ls,RMEDIAN)+nregions] = INDEFR
		    Memr[rg_lstatp(ls,RMODE)+nregions] = INDEFR
		    Memr[rg_lstatp(ls,RSIGMA)+nregions] = INDEFR
		    Memr[rg_lstatp(ls,RSKY)+nregions] = INDEFR
		    Memr[rg_lstatp(ls,RSKYERR)+nregions] = INDEFR
		    Memr[rg_lstatp(ls,RMAG)+nregions] = INDEFR
		    Memr[rg_lstatp(ls,RMAGERR)+nregions] = INDEFR
		    Meml[rg_lstatp(ls,RNPTS)+nregions] = INDEFL
		    Memr[rg_lstatp(ls,IMEAN)+nregions] = INDEFR
		    Memr[rg_lstatp(ls,IMEDIAN)+nregions] = INDEFR
		    Memr[rg_lstatp(ls,IMODE)+nregions] = INDEFR
		    Memr[rg_lstatp(ls,ISIGMA)+nregions] = INDEFR
		    Memr[rg_lstatp(ls,ISKY)+nregions] = INDEFR
		    Memr[rg_lstatp(ls,ISKYERR)+nregions] = INDEFR
		    Memr[rg_lstatp(ls,IMAG)+nregions] = INDEFR
		    Memr[rg_lstatp(ls,IMAGERR)+nregions] = INDEFR
		    Meml[rg_lstatp(ls,INPTS)+nregions] = INDEFL
		    Memr[rg_lstatp(ls,RBSCALE)+nregions] = INDEFR
		    Memr[rg_lstatp(ls,RBSCALEERR)+nregions] = INDEFR
		    Memr[rg_lstatp(ls,RBZERO)+nregions] = INDEFR
		    Memr[rg_lstatp(ls,RBZEROERR)+nregions] = INDEFR
		    Memi[rg_lstatp(ls,RDELETE)+nregions] = LS_NO
		    Memr[rg_lstatp(ls,RCHI)+nregions] = INDEFR
		    nregions = nregions + 1
		}

		call gargwrd (Memc[section], SZ_LINE)
	    }
	}

	call rg_lsetl (ls, NREGIONS, nregions)
	if (nregions > 0)
	    call rg_lrealloc (ls, nregions)
	else
	    call rg_lrfree (ls)

	call sfree (sp)
	return (nregions)
end


# RG_LRCOORDS -- Read the coordinates from a file.

long procedure rg_lrcoords (fd, im, ls, rp, max_nregions)

int	fd			#I the regions file descriptor
pointer	im			#I pointer to the image
pointer	ls			#I pointer to the linscale structure
long	rp			#I pointer to current region
size_t	max_nregions		#I the  maximum number of regions

size_t	sz_val
size_t	nregions
long	ncols, nlines, x1, x2, y1, y2
pointer	sp, line
real	x, y, xc, yc
int	getline(), nscan()
#int	rg_lstati()
long	rg_lstatl()
pointer	rg_lstatp()

begin
	# Allocate working space.
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (line, sz_val, TY_CHAR)

	# Allocate the arrays to hold the regions information,
	call rg_lrealloc (ls, max_nregions)

	# Get the constants.
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	# Decode the regions string.
	nregions = min (rp - 1, rg_lstatl (ls, NREGIONS))
	while (getline (fd, Memc[line]) != EOF &&  nregions < max_nregions) {

	    call sscan (Memc[line])
		call gargr (x)
		call gargr (y)
	    if (nscan() != 2)
		next

            # Compute a more accurate center.
            #if (rg_lstati (ls, CENTER) == YES) {
                #call rg_lcntr (im, x, y, DEF_CRADIUS, xc, yc)
            #} else {
                xc = x
                yc = y
            #}

            # Compute the data section.
            x1 = xc - rg_lstatl (ls, DNX) / 2
            x2 = x1 + rg_lstatl (ls, DNX) - 1
            if (IM_NDIM(im) == 1) {
                y1 = 1
                y2 = 1
            } else {
                y1 = yc - rg_lstatl (ls, DNY) / 2
                y2 = y1 + rg_lstatl (ls, DNY) - 1
            }

            # Make sure that the region is on the image.
            if (x1 >= 1 && x2 <= IM_LEN(im,1) && y1 >= 1 && y2 <=
	        IM_LEN(im,2)) {
                Meml[rg_lstatp(ls,RC1)+nregions] = x1
                Meml[rg_lstatp(ls,RC2)+nregions] = x2
                Meml[rg_lstatp(ls,RL1)+nregions] = y1
                Meml[rg_lstatp(ls,RL2)+nregions] = y2
                Meml[rg_lstatp(ls,RXSTEP)+nregions] = 1
                Meml[rg_lstatp(ls,RYSTEP)+nregions] = 1
		Memr[rg_lstatp(ls,RMEAN)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RMEDIAN)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RMODE)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RSIGMA)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RSKY)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RSKYERR)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RMAG)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RMAGERR)+nregions] = INDEFR
		Meml[rg_lstatp(ls,RNPTS)+nregions] = INDEFL
		Memr[rg_lstatp(ls,IMEAN)+nregions] = INDEFR
		Memr[rg_lstatp(ls,IMEDIAN)+nregions] = INDEFR
		Memr[rg_lstatp(ls,IMODE)+nregions] = INDEFR
		Memr[rg_lstatp(ls,ISIGMA)+nregions] = INDEFR
		Memr[rg_lstatp(ls,ISKY)+nregions] = INDEFR
		Memr[rg_lstatp(ls,ISKYERR)+nregions] = INDEFR
		Memr[rg_lstatp(ls,IMAG)+nregions] = INDEFR
		Memr[rg_lstatp(ls,IMAGERR)+nregions] = INDEFR
		Meml[rg_lstatp(ls,INPTS)+nregions] = INDEFL
		Memr[rg_lstatp(ls,RBSCALE)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RBSCALEERR)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RBZERO)+nregions] = INDEFR
		Memr[rg_lstatp(ls,RBZEROERR)+nregions] = INDEFR
		Memi[rg_lstatp(ls,RDELETE)+nregions] = LS_NO
		Memr[rg_lstatp(ls,RCHI)+nregions] = INDEFR
                nregions = nregions + 1
            }
	}

	call rg_lsetl (ls, NREGIONS, nregions)
	if (nregions > 0)
	    call rg_lrealloc (ls, nregions)
	else
	    call rg_lrfree (ls)

	call sfree (sp)
	return (nregions)
end


# RG_LRPHOT -- Read the photometry from a file.

long procedure rg_lrphot (fd, ls, rp, max_nregions, refimage)

int	fd			#I the regions file descriptor
pointer	ls			#I pointer to the linscale structure
long	rp			#I pointer to current region
size_t	max_nregions		#I the  maximum number of regions
int	refimage		#I is the photometry for the reference image

size_t	sz_val
size_t	nregions, maxnr
pointer	sp, line
real	sky, skyerr, mag, magerr
long	rg_lstatl()
int	getline(), nscan()
pointer	rg_lstatp()

begin
	# Allocate working space.
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (line, sz_val, TY_CHAR)

	# Allocate the space to hold the arrays.
	if (refimage == YES) {
	    call rg_lrealloc (ls, max_nregions)
	    nregions = min (rp - 1, rg_lstatl (ls, NREGIONS))
	    maxnr = max_nregions
	} else {
	    nregions = 0
	    maxnr = rg_lstatl (ls, NREGIONS)
	}

	while (getline (fd, Memc[line]) != EOF &&  nregions < maxnr) {

	    call sscan (Memc[line])
		call gargr (sky)
		call gargr (skyerr)
		call gargr (mag)
		call gargr (magerr)
	    if (nscan() != 4)
		next

            Meml[rg_lstatp(ls,RC1)+nregions] = INDEFL
            Meml[rg_lstatp(ls,RC2)+nregions] = INDEFL
            Meml[rg_lstatp(ls,RL1)+nregions] = INDEFL
            Meml[rg_lstatp(ls,RL2)+nregions] = INDEFL
            Meml[rg_lstatp(ls,RXSTEP)+nregions] = INDEFL
            Meml[rg_lstatp(ls,RYSTEP)+nregions] = INDEFL

	    Memr[rg_lstatp(ls,RMEAN)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RMEDIAN)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RMODE)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RSIGMA)+nregions] = INDEFR
	    Meml[rg_lstatp(ls,RNPTS)+nregions] = INDEFL
	    if (refimage == YES) {
	        Memr[rg_lstatp(ls,RSKY)+nregions] = sky
	        Memr[rg_lstatp(ls,RSKYERR)+nregions] = skyerr
	        Memr[rg_lstatp(ls,RMAG)+nregions] = mag
	        Memr[rg_lstatp(ls,RMAGERR)+nregions] = magerr
	        Memr[rg_lstatp(ls,ISKY)+nregions] = INDEFR
	        Memr[rg_lstatp(ls,ISKYERR)+nregions] = INDEFR
	        Memr[rg_lstatp(ls,IMAG)+nregions] = INDEFR
	        Memr[rg_lstatp(ls,IMAGERR)+nregions] = INDEFR
	    }

	    Memr[rg_lstatp(ls,IMEAN)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,IMEDIAN)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,IMODE)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,ISIGMA)+nregions] = INDEFR
	    Meml[rg_lstatp(ls,INPTS)+nregions] = INDEFL
	    if (refimage == NO) {
	        Memr[rg_lstatp(ls,ISKY)+nregions] = sky
	        Memr[rg_lstatp(ls,ISKYERR)+nregions] = skyerr
	        Memr[rg_lstatp(ls,IMAG)+nregions] = mag
	        Memr[rg_lstatp(ls,IMAGERR)+nregions] = magerr
	    }

	    Memr[rg_lstatp(ls,RBSCALE)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RBSCALEERR)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RBZERO)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RBZEROERR)+nregions] = INDEFR
	    Memi[rg_lstatp(ls,RDELETE)+nregions] = LS_NO
	    Memr[rg_lstatp(ls,RCHI)+nregions] = INDEFR
            nregions = nregions + 1
	}

	if (refimage == YES) {
	    call rg_lsetl (ls, NREGIONS, nregions)
	    if (nregions > 0)
	        call rg_lrealloc (ls, nregions)
	    else
	        call rg_lrfree (ls)
	} else if (nregions < rg_lstatl (ls,NREGIONS)) {
	    call rg_lsetl (ls, NREGIONS, nregions)
	}

	call sfree (sp)
	return (nregions)
end


# RG_LSREGIONS -- Procedure to compute the column and line limits given
# an image section. If the section is the null string then the region list
# is empty.

long procedure rg_lsregions (im, ls, rp, max_nregions)

pointer	im			#I pointer to the image
pointer	ls			#I pointer to the linscale structure
long	rp			#I pointer to the current region
size_t	max_nregions		#I maximum number of regions

size_t	sz_val
size_t	nregions
long	ncols, nlines
long	x1, x2, y1, y2, xstep, ystep
pointer	sp, section, region
long	rg_lstatl()
int	rg_lgsections()
pointer	rg_lstatp()

begin
	# Allocate working space.
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (region, sz_val, TY_CHAR)
	call salloc (section, sz_val, TY_CHAR)
	call rg_lstats (ls, REGIONS, Memc[region], SZ_LINE)

	# Allocate the arrays to hold the regions information.
	call rg_lrealloc (ls, max_nregions)

	# Get the constants.
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	if (Memc[region] != EOS) {

	    call sscan (Memc[region])
	        call gargwrd (Memc[section], SZ_LINE)

	    nregions = min (rp - 1, rg_lstatl (ls, NREGIONS))
	    while (Memc[section] != EOS && nregions < max_nregions) {

		# Check for even dimensioned regions.
		if (rg_lgsections (Memc[section], x1, x2, xstep, y1, y2, ystep,
		    ncols, nlines) == OK) {
		    Meml[rg_lstatp(ls,RC1)+nregions] = x1
		    Meml[rg_lstatp(ls,RC2)+nregions] = x2
		    Meml[rg_lstatp(ls,RL1)+nregions] = y1
		    Meml[rg_lstatp(ls,RL2)+nregions] = y2
		    Meml[rg_lstatp(ls,RXSTEP)+nregions] = xstep
		    Meml[rg_lstatp(ls,RYSTEP)+nregions] = ystep
	    	    Memr[rg_lstatp(ls,RMEAN)+nregions] = INDEFR
	    	    Memr[rg_lstatp(ls,RMEDIAN)+nregions] = INDEFR
	    	    Memr[rg_lstatp(ls,RMODE)+nregions] = INDEFR
	    	    Memr[rg_lstatp(ls,RSIGMA)+nregions] = INDEFR
	    	    Memr[rg_lstatp(ls,RSKY)+nregions] = INDEFR
	    	    Memr[rg_lstatp(ls,RSKYERR)+nregions] = INDEFR
	    	    Memr[rg_lstatp(ls,RMAG)+nregions] = INDEFR
	    	    Memr[rg_lstatp(ls,RMAGERR)+nregions] = INDEFR
	    	    Meml[rg_lstatp(ls,RNPTS)+nregions] = INDEFL
	    	    Memr[rg_lstatp(ls,IMEAN)+nregions] = INDEFR
	    	    Memr[rg_lstatp(ls,IMEDIAN)+nregions] = INDEFR
	    	    Memr[rg_lstatp(ls,IMODE)+nregions] = INDEFR
	    	    Memr[rg_lstatp(ls,ISIGMA)+nregions] = INDEFR
	    	    Memr[rg_lstatp(ls,ISKY)+nregions] = INDEFR
	    	    Memr[rg_lstatp(ls,ISKYERR)+nregions] = INDEFR
	    	    Memr[rg_lstatp(ls,IMAG)+nregions] = INDEFR
	    	    Memr[rg_lstatp(ls,IMAGERR)+nregions] = INDEFR
	    	    Meml[rg_lstatp(ls,INPTS)+nregions] = INDEFL
	    	    Memr[rg_lstatp(ls,RBSCALE)+nregions] = INDEFR
	    	    Memr[rg_lstatp(ls,RBSCALEERR)+nregions] = INDEFR
	    	    Memr[rg_lstatp(ls,RBZERO)+nregions] = INDEFR
	    	    Memr[rg_lstatp(ls,RBZEROERR)+nregions] = INDEFR
	    	    Memi[rg_lstatp(ls,RDELETE)+nregions] = LS_NO
	    	    Memr[rg_lstatp(ls,RCHI)+nregions] = INDEFR
		    nregions = nregions + 1
		}
	        call gargwrd (Memc[section], SZ_LINE)
	    }

	} else {
	    Meml[rg_lstatp(ls,RC1)+nregions] = 1
	    Meml[rg_lstatp(ls,RC2)+nregions] = ncols
	    Meml[rg_lstatp(ls,RL1)+nregions] = 1
	    Meml[rg_lstatp(ls,RL2)+nregions] = nlines
	    Meml[rg_lstatp(ls,RXSTEP)+nregions] = 1
	    Meml[rg_lstatp(ls,RYSTEP)+nregions] = 1
	    Memr[rg_lstatp(ls,RMEAN)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RMEDIAN)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RMODE)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RSIGMA)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RSKY)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RSKYERR)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RMAG)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RMAGERR)+nregions] = INDEFR
	    Meml[rg_lstatp(ls,RNPTS)+nregions] = INDEFL
	    Memr[rg_lstatp(ls,IMEAN)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,IMEDIAN)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,IMODE)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,ISIGMA)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,ISKY)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,ISKYERR)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,IMAG)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,IMAGERR)+nregions] = INDEFR
	    Meml[rg_lstatp(ls,INPTS)+nregions] = INDEFL
	    Memr[rg_lstatp(ls,RBSCALE)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RBSCALEERR)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RBZERO)+nregions] = INDEFR
	    Memr[rg_lstatp(ls,RBZEROERR)+nregions] = INDEFR
	    Memi[rg_lstatp(ls,RDELETE)+nregions] = LS_NO
	    Memr[rg_lstatp(ls,RCHI)+nregions] = INDEFR
	    nregions = 1
	}


	# Reallocate the correct amount of space.
	call rg_lsetl (ls, NREGIONS, nregions)
	if (nregions > 0)
	    call rg_lrealloc (ls, nregions)
	else
	    call rg_lrfree (ls)

	call sfree (sp)
	return (nregions)
end


# RG_LGSECTIONS -- Decode an image section into column and line limits
# and a step size. Sections which describe the whole image are decoded into
# a block ncols * nlines long.

int procedure rg_lgsections (section, x1, x2, xstep, y1, y2, ystep, ncols,
        nlines)

char    section[ARB]            #I the input section string
long	x1, x2                  #O the output column section limits
long	xstep                   #O the output column step size
long	y1, y2                  #O the output line section limits
long	ystep                   #O the output line step size
long	ncols, nlines           #I the maximum number of lines and columns

int	ip
int	rg_lgdim()

begin
        ip = 1
        if (rg_lgdim (section, ip, x1, x2, xstep, ncols) == ERR)
            return (ERR)
        if (rg_lgdim (section, ip, y1, y2, ystep, nlines) == ERR)
            return (ERR)

        return (OK)
end


# RG_LGDIM -- Decode a single subscript expression to produce the
# range of values for that subscript (X1:X2), and the sampling step size, STEP.
# Note that X1 may be less than, greater than, or equal to X2, and STEP may
# be a positive or negative nonzero integer.  Various shorthand notations are
# permitted, as is embedded whitespace.

int procedure rg_lgdim (section, ip, x1, x2, step, limit)

char    section[ARB]            #I the input image section
int	ip                      #I/O pointer to the position in section string
long	x1                      #O first limit of dimension
long	x2                      #O second limit of dimension
long	step                    #O step size of dimension
long	limit                   #I maximum size of dimension

long	temp
int	ctol()

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
        if (ctol (section, ip, temp) > 0) {                     # [x1
            x1 = max (1, min (temp, limit))
            if (section[ip] == ':') {
                ip = ip + 1
                if (ctol (section, ip, temp) == 0)              # [x1:x2
                    return (ERR)
                x2 = max (1, min (temp, limit))
            } else {
                x2 = x1
	    }

        } else if (section[ip] == '-') {
            x1 = limit
            x2 = 1
            ip = ip + 1
            if (section[ip] == '*')
                ip = ip + 1

        } else if (section[ip] == '*')                          # [*
            ip = ip + 1

        while (IS_WHITE(section[ip]))
            ip = ip + 1

        # Get sample step size, if give.
        if (section[ip] == ':') {                               # ..:step
            ip = ip + 1
            if (ctol (section, ip, step) == 0)
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



