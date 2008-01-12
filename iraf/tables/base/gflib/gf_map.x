include <syserr.h>
include	<imhdr.h>
include <imio.h>
include "gf.h"

#* HISTORY *
#* B.Simon	30-Sep-98	Original code
#* B.Simon	15-Dec-98	Fixes for imcalc problems
#* B.Simon	17-Dec-98	Fixes for geis files with group sections
#* B.Simon	04-Nov-99       Create primary header and set pixtype for fits
#* B.Simon	15-Nov-99       Made adding primary header condtional on EXTEND
#* B.Simon	19-Nov-99	added extension header fixup code
#* B.Simon	08-Nov-00	Do not close and reopen template
#* B.Simon	20-Nov-00	get default extension with gf_getext

# GF_MAP -- Run immap transparently on images of all types

pointer procedure gf_map (image, acmode, oldim)

char	image[ARB]	# i: image name
int	acmode		# i: access mode
pointer	oldim		# i: old image descriptor
#--
int	code, extend, inherit, oldcode, gn, hist
pointer	sp, db, im, extra, oldname, fullname

int	gf_filetype(), gf_imtype()
pointer	immap()
errchk	immap

int gfhist()

begin
	call smark (sp)
	call salloc (extra, SZ_SHORTSTR, TY_CHAR)
	call salloc (oldname, SZ_FNAME, TY_CHAR)
	call salloc (fullname, SZ_FNAME, TY_CHAR)

	# Set default characteristics of new image name

	gn = -1

	# Determine the file type

	code = gf_filetype (image, acmode)

	# Build a database of extension header keywords

	hist = -1  # Assume no history spool required


	if (acmode != NEW_COPY) {
	    im = NULL
	    db = NULL
	    extend = NO
	    inherit = NO

	    if (code == FITS_FMT) {
		call gf_extend (image, acmode, im, extend, inherit)

		if (extend == YES)
		    call gf_build_db (image, im, db)
	    }

	    # Get hist from extension#0 before mapping the real extension.
	    if ((code == FITS_FMT) && (inherit == 1)) {
		hist = gfhist(image)
	    }
	} else {
	    if (oldim == NULL)
		call imerr (image, SYS_IMMAGNCPY)

	    # Save template image characterisrics

	    oldcode = gf_imtype (oldim)
	    call strcpy (IM_HDRFILE(oldim), Memc[oldname], SZ_FNAME)

	    # See if the new image should have extensions
	    # Set inherit to the value in the old fits image  

	    call gf_extend (image, acmode, oldim, extend, inherit)

	    # Copy primary header from template image

	    db = NULL
	    if (extend == YES) {
		call gf_primary (image, oldim)

		# Create extension keyword database

		if (code == FITS_FMT || oldcode == FITS_FMT)
		    call gf_build_db (Memc[oldname], oldim, db)
	    }
	}

	# Construct the name of the  image
	if (extend == NO && code == FITS_FMT)
	    gn = 0

	if (inherit == YES) {
	    call strcpy ("inherit", Memc[extra], SZ_SHORTSTR)
	} else {
	    Memc[extra] = EOS
	}

	call gf_imname (image, Memc[extra], acmode, gn, 
			Memc[fullname], SZ_FNAME)

	# Open the image using the reconstituted name
	im = immap (Memc[fullname], acmode, oldim)

	# Set the pixel type to that of the old image

	if (acmode == NEW_COPY && code == FITS_FMT)
	    IM_PIXTYPE(im) = IM_PIXTYPE(oldim)

	# Set up the group parameter block in geis format images

	if (code == GEIS_FMT && oldim != NULL && db != NULL)
	    call gf_merge_gpb (im, oldim, db)

	# Add the database to the cache

	if (code == FITS_FMT) {
	    gn = max (gn, 1)
	    call gf_setup_db (im, db, gn, extend, inherit, hist)

	} else if (db != NULL) {
	    call mfree (db, TY_CHAR)
	}

	call sfree (sp)
	return (im)
end


# gfhist scans a FITS file for history records, and returns a spool file 
# containing them.

int procedure gfhist(imname)
char    imname[ARB]

#--

pointer im
int     imua, hist, ua, ua_len, gn, gcount

char    filename[SZ_FNAME], hname[SZ_FNAME]
char    record[SZ_LINE]
char    cluster[SZ_FNAME], ksection[SZ_SHORTSTR], section[SZ_SHORTSTR]

int strncmp(), stropen(), strlen(), getline(), open()
pointer immap()

include "gfdb.com"

begin
	call imparse(imname, cluster, SZ_FNAME, ksection, SZ_SHORTSTR, section, SZ_SHORTSTR, gn, gcount)

	call strcpy(cluster, filename, SZ_FNAME)
	call strcat("[0]", filename, SZ_FNAME)

	im = immap(filename, READ_ONLY, NULL)

	ua = IM_USERAREA(im)
 	ua_len = strlen(Memc[ua])
	imua = stropen(Memc[ua], ua_len, READ_ONLY)

	# Generate a unique name for the history spool file.
	call sprintf(hname, SZ_FNAME, "h%d")
	call pargi(hnum)
	hnum = hnum + 1           # from gfdb.com common block

	hist = open(hname, READ_WRITE, SPOOL_FILE)

	while(getline(imua, record) != EOF) {
		if (strncmp(record,"HISTORY", 7) == 0) {
			call putline(hist, record)
		}
	}

	call imunmap(im)
	call strclose(imua)
	return hist
end
