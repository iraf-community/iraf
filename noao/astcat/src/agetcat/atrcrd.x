include <fset.h>
include <imhdr.h>
include <mwset.h>
include <pkg/skywcs.h>
include "../../lib/astrom.h"
include "../../lib/acatalog.h"

# AT_RCLIST -- Create a list of field centers.

int procedure at_rclist (at, rcsource)

pointer	at			#I the astrometry descriptor
char	rcsource[ARB]		#I the source of the regions list

pointer	sp, symname, st, sym
int	nfields, fd, imlist
double	at_statd()
pointer	at_statp(), stopen(), stenter()
int	at_stati(), access(), open(), imtopen(), at_rcread(), at_rcwcsim()
bool	streq()

begin
	# Store the rcsource name in the data structure.
	call at_sets (at, RCSOURCE, rcsource)

	# Check that the field center pointer is defined.
	if (at_statp (at, PRCENTER) == NULL)
	    return (0)

	call smark (sp)
	call salloc (symname, SZ_FNAME, TY_CHAR)

	# Open the symbol table.
	if (at_statp (at, RCST) != NULL)
	    call stclose (at_statp(at, RCST))
	st = stopen ("fclist", 2 * DEF_LEN_RCST, DEF_LEN_RCST,
	    10 * DEF_LEN_RCST) 
	call at_setp (at, RCST, st)

	# The source is the fcpars parameter set.
	if (streq (rcsource, "pars")) {

	    if (at_statd (at, RCRA) < 0.0d0 || at_statd(at, RCRA) > 360.0d0)
		nfields = 0
	    else if (at_statd (at, RCDEC) < -90.0d0 || at_statd(at, RCDEC) >
	        90.0d0)
		nfields = 0
	    else if (at_statd (at, RCRAWIDTH) / 60.0d0 <= 0.0d0 ||
	        at_statd(at, RCRAWIDTH) / 60.0d0 > 360.0d0)
		nfields = 0
	    else if (at_statd (at, RCDECWIDTH) / 60.0d0 <= 0.0d0 ||
	        at_statd(at, RCDECWIDTH) / 60.0d0 > 180.0d0)
		nfields = 0
	    else {
		call sprintf (Memc[symname], SZ_FNAME, "%s1")
		    call pargstr (DEF_RCST_ROOTNAME)
	        sym = stenter (st, Memc[symname], LEN_RCST_STRUCT)
	        AT_RCSTRA(sym) = at_statd (at, RCRA) 
	        AT_RCSTDEC(sym) = at_statd (at, RCDEC) 
	        AT_RCSTRAWIDTH(sym) = at_statd (at, RCRAWIDTH) 
	        AT_RCSTDECWIDTH(sym) = at_statd (at, RCDECWIDTH) 
	        AT_RCSTRAUNITS(sym) = at_stati (at, RCRAUNITS) 
	        AT_RCSTDECUNITS(sym) = at_stati (at, RCDECUNITS) 
	        call at_stats (at, RCSYSTEM, Memc[symname], SZ_FNAME)
	        call strcpy (Memc[symname], AT_RCSTSYSTEM(sym), SZ_FNAME)
		call strcpy ("pars", AT_RCSTSOURCE(sym), SZ_FNAME)
		call strcpy ("", AT_RCSTNAME(sym), SZ_FNAME)
	        nfields = 1
	    }

	# The source is a text file.
	} else if (access (rcsource, READ_ONLY, TEXT_FILE) == YES) {

	    fd = open (rcsource, READ_ONLY, TEXT_FILE)
	    nfields = at_rcread (fd, at, st)
	    call close (fd)

	# The field center source is a list of images. Assume for now that
	# images with celestial coordinate systems have a wcs system name
	# of "image". This is true of images with a standard FITS wcs and
	# for images with a wcs created by the core IRAF tasks.
	} else {
	    imlist = imtopen (rcsource)
	    nfields = at_rcwcsim (imlist, at, st)
	    call imtclose (imlist)
	}

	call sfree (sp)

	return (nfields)
end


# AT_RCREAD -- Read in the field center information from a text file.

int procedure at_rcread (fd, at, st)

int	fd			#I the field center file descriptor
pointer	at			#I the astrometry descriptor
pointer	st			#I the field center symbol table descriptor.

double	ra, dec, rawidth, decwidth
pointer	sp, symname, sym
int	nfields
pointer	stenter()
int	fscan(), nscan(), at_stati(), strdic()

begin
	call smark (sp)
	call salloc (symname, SZ_FNAME, TY_CHAR)

	nfields = 0
	while (fscan(fd) != EOF) {

	    # Get the minimum number of fields.
	    call gargd (ra)
	    call gargd (dec)
	    call gargd (rawidth)
	    call gargd (decwidth)
	    if (nscan() < 4)
	        next
	    if (ra < 0.0d0 || ra > 360.0d0)
		next
	    if (dec < -90.0d0 || dec > 90.0d0)
		next
	    if (rawidth / 60.0d0 <= 0.0d0 || rawidth / 60.0d0 > 360.0d0)
		next
	    if (decwidth / 60.0d0 <= 0.0d0 || decwidth / 60.0d0 > 180.0d0)
		next

	    # Get the next symbols.
	    nfields = nfields + 1
	    call sprintf (Memc[symname], SZ_FNAME, "%s%d")
		call pargstr (DEF_RCST_ROOTNAME)
	        call pargi (nfields)
	    sym = stenter (st, Memc[symname], LEN_RCST_STRUCT)

	    AT_RCSTRA(sym) = ra
	    AT_RCSTDEC(sym) = dec
	    AT_RCSTRAWIDTH(sym) = rawidth
	    AT_RCSTDECWIDTH(sym) = decwidth

	    # Set the source and source name.
	    call strcpy ("file", AT_RCSTSOURCE(sym), SZ_FNAME)
	    call fstats (fd, F_FILENAME, Memc[symname], SZ_FNAME)
	    call strcpy (Memc[symname], AT_RCSTNAME(sym), SZ_FNAME)

	    # Decode the units.
	    call gargwrd (Memc[symname], SZ_FNAME)
	    if (nscan() < 5) {
	    	AT_RCSTRAUNITS(sym) = at_stati (at, RCRAUNITS) 
	        AT_RCSTDECUNITS(sym) = at_stati (at, RCDECUNITS) 
	        call at_stats (at, RCSYSTEM, Memc[symname], SZ_FNAME)
	        call strcpy (Memc[symname], AT_RCSTSYSTEM(sym), SZ_FNAME)
		next
	    } else
		AT_RCSTRAUNITS(sym) = strdic (Memc[symname], Memc[symname],
		    SZ_FNAME, AT_RA_UNITS)
	    call gargwrd (Memc[symname], SZ_FNAME)
	    if (nscan() < 6) {
	        AT_RCSTDECUNITS(sym) = at_stati (at, RCDECUNITS) 
	        call at_stats (at, RCSYSTEM, Memc[symname], SZ_FNAME)
	        call strcpy (Memc[symname], AT_RCSTSYSTEM(sym), SZ_FNAME)
		next
	    } else
	        AT_RCSTDECUNITS(sym) = strdic (Memc[symname], Memc[symname],
	            SZ_FNAME, AT_DEC_UNITS)

	    # Decode the coordinate system.
	    call gargstr (Memc[symname], SZ_FNAME)
	    if (Memc[symname] == EOS || nscan() < 7) { 
	        call at_stats (at, RCSYSTEM, Memc[symname], SZ_FNAME)
	        call strcpy (Memc[symname], AT_RCSTSYSTEM(sym), SZ_FNAME)
	    } else
	        call strcpy (Memc[symname], AT_RCSTSYSTEM(sym), SZ_FNAME)

	}

	call sfree (sp)

	return (nfields)
end


# AT_RCWCSIM -- Read in the field center information from a list of images.

int procedure at_rcwcsim (imlist, at, st)

int	imlist			#I the image list descriptor
pointer	at			#I the astrometry descriptor
pointer	st			#I the field center symbol table descriptor.

double	ra, dec, width
pointer	sp, image, symname, im, mw, coo, sym, ct
int	nfields
pointer	immap(), mw_sctran(), stenter()
int	imtgetim(), sk_decim(), sk_stati()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (symname, SZ_FNAME, TY_CHAR)

	nfields = 0
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # The image must be 2D.
	    im = immap (Memc[image], READ_ONLY, 0)
	    if (IM_NDIM(im) != 2) {
		call imunmap (im)
		next
	    }

	    # The image must have a FITS celestial coordinate system.
	    if (sk_decim (im, "world", mw, coo) == ERR) {
		if (mw != NULL)
		    call mw_close (mw)
		call sk_close (coo)
		call imunmap (im)
		next
	    }

	    # Find the center of the image.
	    ct = mw_sctran (mw, "logical", "world", 03B)
	    call mw_c2trand (ct, double((1.0d0 + IM_LEN(im,1)) / 2.0d0),
	        double((1.0d0 + IM_LEN(im,2)) / 2.0d0), ra, dec)
	    if (ra < 0.0d0 || ra > 360.0d0)
		next
	    if (dec < -90.0d0 || dec > 90.0d0)
		next

	    # Find the width of the field.
	    call at_gfwidth (im, mw, sk_stati(coo, S_PLNGAX),
	        sk_stati(coo, S_PLATAX), width) 

	    # Get the next symol.
	    nfields = nfields + 1
	    call sprintf (Memc[symname], SZ_FNAME, "%s%d")
		call pargstr (DEF_RCST_ROOTNAME)
	        call pargi (nfields)
	    sym = stenter (st, Memc[symname], LEN_RCST_STRUCT)

	    AT_RCSTRA(sym) = ra
	    AT_RCSTDEC(sym) = dec
	    AT_RCSTRAWIDTH(sym) = width
	    AT_RCSTDECWIDTH(sym) = width
	    AT_RCSTRAUNITS(sym) = AT_DEGREES
	    AT_RCSTDECUNITS(sym) = AT_DEGREES
	    call sk_enwcs (coo, AT_RCSTSYSTEM(sym), SZ_FNAME)

	    call strcpy ("image", AT_RCSTSOURCE(sym), SZ_FNAME)
	    call strcpy (Memc[image], AT_RCSTNAME(sym), SZ_FNAME)

	    # Cleanup.
	    call sk_close (coo)
	    call mw_close (mw)
	    call imunmap (im)
	}

	call sfree (sp)

	return (nfields)
end


define  NEWCD     Memd[ncd+(($2)-1)*ndim+($1)-1]

# AT_GFWIDTH -- Estimate the field width in arcminutes from the size of the
# image and the image wcs.

procedure at_gfwidth (im, mw, lngax, latax, width)

pointer	im			#I the input image desciptor
pointer	mw			#I the input wcs descriptor
int	lngax			#I the longitude axis
int	latax			#I the latitude axis
double	width			#O the output field width in minutes of arc

double	scale
pointer	r, cd, ltm, iltm, ncd
int	ndim
int	mw_stati()

begin
	# Get the dimension of the wcs.
	ndim = mw_stati (mw, MW_NPHYSDIM)

        # Allocate working memory.
        call malloc (r, ndim * ndim, TY_DOUBLE)
        call malloc (cd, ndim * ndim, TY_DOUBLE)
        call malloc (ltm, ndim * ndim, TY_DOUBLE)
        call malloc (iltm, ndim * ndim, TY_DOUBLE)
        call malloc (ncd, ndim * ndim, TY_DOUBLE)

        # Compute the original world to logical transformation.
        call mw_gwtermd (mw, Memd[r], Memd[r], Memd[cd], ndim)
        call mw_gltermd (mw, Memd[ltm], Memd[r], ndim)
        call mwinvertd (Memd[ltm], Memd[iltm], ndim)
        call mwmmuld (Memd[cd], Memd[iltm], Memd[ncd], ndim)

	# Estimate the scale.
	scale = max (sqrt (NEWCD(lngax,lngax)**2 + NEWCD(lngax,latax)**2),
	    sqrt (NEWCD(latax,lngax)**2 + NEWCD(latax,latax)**2))

	# Compute the width
	width = 60.0d0 * scale * max (IM_LEN(im,1), IM_LEN(im,2)) 

	# Free the space.
        call mfree (r, TY_DOUBLE)
        call mfree (cd, TY_DOUBLE)
        call mfree (ncd, TY_DOUBLE)
        call mfree (ltm, TY_DOUBLE)
        call mfree (iltm, TY_DOUBLE)
end
