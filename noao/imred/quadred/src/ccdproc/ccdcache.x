include	<imhdr.h>
include	<imset.h>
include	<mach.h>
include	"ccdcache.h"

.help ccdcache Jun87
.nf ---------------------------------------------------------------------
The purpose of the CCD image caching package is to minimize image mapping
time, to prevent multiple mapping of the same image, and to keep entire
calibration images in memory for extended periods to minimize disk
I/O.  It is selected by specifying a maximum caching size based on the
available memory.  When there is not enough memory for caching (or by
setting the size to 0) then standard IMIO is used.  When there is
enough memory then as many images as will fit into the specified cache
size are kept in memory.  Images are also kept mapped until explicitly
flushed or the entire package is closed.

This is a special purpose interface intended only for the CCDRED package.
It has the following restrictions.

    1.  Images must be processed to be cached.
    2.  Images must be 2 dimensional to be cached
    3.  Images must be real or short to be cached.
    4.  Images must be read_only to be cached.
    5.  Cached images remain in memory until they are displaced,
	flushed, or the package is closed.

The package consists of the following procedures.

	      ccd_open ()
	 im = ccd_cache (image)
	ptr = ccd_glr (im, col1, col2, line)
	ptr = ccd_gls (im, col1, col2, line)
	      ccd_unmap (im)
	      ccd_flush (im)
	      ccd_close ()


CCD_OPEN:   Initialize the image cache.  Called at the beginning.
CCD_CLOSE:  Flush the image cache and restore memory.  Called at the end.

CCD_CACHE:  Open an image and save the IMIO pointer.  If the image has been
opened previously it need not be opened again.  If image data caching
is specified the image data may be read it into memory.  In order for
image data caching to occur the the image has to have been processed,
be two dimensional, be real or short, and the total cache memory not
be exceeded.  If an error occurs in reading the image into memory
the data is not cached.

CCD_UNMAP:  The image access number is decremented but the image
is not closed against the event it will be used again.

CCD_FLUSH:  The image is closed and flushed from the cache.

CCD_GLR, CCD_GLS: Get a real or short image line.  If the image data is cached
then a pointer to the line is quickly returned.  If the data is not cached then
IMIO is used to get the pointer.
.endhelp ---------------------------------------------------------------------



# CCD_CACHE -- Open an image and possibly cache it in memory.

pointer procedure ccd_cache (image, ccdtype)

char	image[ARB]		# Image to be opened
int	ccdtype			# Image type

int	i, nc, nl, nbytes
pointer	sp, str, pcache, pcache1, im

int	sizeof()
pointer	immap(), imgs2r(), imgs2s()
bool	streq(), ccdcheck()
errchk	immap, imgs2r, imgs2s

include	"ccdcache.com"

define	done_		99

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Check if the image is cached.
	for (i=1; i<=ccd_ncache; i=i+1) {
	    pcache = Memi[ccd_pcache+i-1]
	    im = CCD_IM(pcache)
	    call imstats (im, IM_IMAGENAME, Memc[str], SZ_LINE)
	    if (streq (image, Memc[str]))
		break
	}

	# If the image is not cached open it and allocate memory.
	if (i > ccd_ncache) {
	    im = immap (image, READ_ONLY, 0)
	    ccd_ncache = i
	    call realloc (ccd_pcache, ccd_ncache, TY_INT)
	    call malloc (pcache, CCD_LENCACHE, TY_STRUCT)
	    Memi[ccd_pcache+i-1] = pcache
	    CCD_IM(pcache) = im
	    CCD_NACCESS(pcache) = 0
	    CCD_SZDATA(pcache) = 0
	    CCD_DATA(pcache) = NULL
	    CCD_BUFR(pcache) = NULL
	    CCD_BUFS(pcache) = NULL
	}

	# If not caching the image data or if the image data has already
	# been cached we are done.
	if ((ccd_maxcache == 0) || (CCD_SZDATA(pcache) > 0))
	    goto done_

	# Don't cache unprocessed calibration image data.
	# This is the only really CCDRED specific code.
	if (ccdcheck (im, ccdtype))
	    goto done_

	# Check image is 2D and a supported pixel type.
	if (IM_NDIM(im) != 2)
	    goto done_
	if ((IM_PIXTYPE(im) != TY_REAL) && (IM_PIXTYPE(im) !=TY_SHORT))
	    goto done_

	# Compute the size of the image data.
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	nbytes = nc * nl * sizeof (IM_PIXTYPE(im)) * SZB_CHAR

	# Free memory not in use.
	if (ccd_szcache + nbytes > ccd_maxcache) {
	    for (i=1; i<=ccd_ncache; i=i+1) {
	        pcache1 = Memi[ccd_pcache+i-1]
	        if (CCD_NACCESS(pcache1) == 0) {
		    if (CCD_SZDATA(pcache1) > 0) {
		        ccd_szcache = ccd_szcache - CCD_SZDATA(pcache1)
		        CCD_SZDATA(pcache1) = 0
		        CCD_DATA(pcache1) = NULL
			call mfree (CCD_BUFR(pcache1), TY_REAL)
			call mfree (CCD_BUFS(pcache1), TY_SHORT)
		        call imseti (CCD_IM(pcache1), IM_CANCEL, YES)
		        if (ccd_szcache + nbytes > ccd_maxcache)
		            break
		    }
	        }
	    }
	}
	if (ccd_szcache + nbytes > ccd_maxcache)
	    goto done_

	# Cache the image data
	iferr {
	    switch (IM_PIXTYPE (im)) {
	    case TY_SHORT:
	        CCD_DATA(pcache) = imgs2s (im, 1, nc, 1, nl)
	    case TY_REAL:
	        CCD_DATA(pcache) = imgs2r (im, 1, nc, 1, nl)
	    }
	    ccd_szcache = ccd_szcache + nbytes
	    CCD_SZDATA(pcache) = nbytes
	} then {
	    call imunmap (im)
	    im = immap (image, READ_ONLY, 0)
	    CCD_IM(pcache) = im
	    CCD_SZDATA(pcache) = 0
	}

done_
	CCD_NACCESS(pcache) = CCD_NACCESS(pcache) + 1
	call sfree (sp)
	return (im)
end


# CCD_OPEN -- Initialize the CCD image cache.

procedure ccd_open (max_cache)

int	max_cache	# Maximum cache size in bytes

int	max_size, begmem()
include	"ccdcache.com"

begin
	ccd_ncache = 0
	ccd_maxcache = max_cache
	ccd_szcache = 0
	call malloc (ccd_pcache, 1, TY_INT)

	# Ask for the maximum physical memory.
	if (ccd_maxcache > 0) {
	    ccd_oldsize = begmem (0, ccd_oldsize, max_size)
	    call fixmem (max_size)
	}
end


# CCD_UNMAP -- Unmap an image.
# Don't actually unmap the image since it may be opened again.

procedure ccd_unmap (im)

pointer	im			# IMIO pointer

int	i
pointer	pcache
include	"ccdcache.com"

begin
	for (i=1; i<=ccd_ncache; i=i+1) {
	    pcache = Memi[ccd_pcache+i-1]
	    if (CCD_IM(pcache) == im) {
		CCD_NACCESS(pcache) = CCD_NACCESS(pcache) - 1
		return
	    }
	}

	call imunmap (im)
end


# CCD_FLUSH -- Close image and flush from cache.

procedure ccd_flush (im)

pointer	im			# IMIO pointer

int	i
pointer	pcache
include "ccdcache.com"

begin
	for (i=1; i<=ccd_ncache; i=i+1) {
	    pcache = Memi[ccd_pcache+i-1]
	    if (CCD_IM(pcache) == im) {
		ccd_ncache = ccd_ncache - 1
	        ccd_szcache = ccd_szcache - CCD_SZDATA(pcache)
	        call mfree (CCD_BUFR(pcache), TY_REAL)
	        call mfree (CCD_BUFS(pcache), TY_SHORT)
		call mfree (pcache, TY_STRUCT)
		for (; i<=ccd_ncache; i=i+1)
		    Memi[ccd_pcache+i-1] = Memi[ccd_pcache+i]
		break
	    }
	}

	call imunmap (im)
end


# CCD_CLOSE -- Close the image cache.

procedure ccd_close ()

int	i
pointer	pcache
include "ccdcache.com"

begin
	for (i=1; i<=ccd_ncache; i=i+1) {
	    pcache = Memi[ccd_pcache+i-1]
	    call imunmap (CCD_IM(pcache))
	    call mfree (CCD_BUFR(pcache), TY_REAL)
	    call mfree (CCD_BUFS(pcache), TY_SHORT)
	    call mfree (pcache, TY_STRUCT)
	}
	call mfree (ccd_pcache, TY_INT)

	# Restore memory.
	call fixmem (ccd_oldsize)
end


# CCD_GLR -- Get a line of real data from the image.
# If the image data is cached this is fast (particularly if the datatype
# matches).  If the image data is not cached then use IMIO.

pointer procedure ccd_glr (im, col1, col2, line)

pointer	im			# IMIO pointer
int	col1, col2		# Columns
int	line			# Line

int	i
pointer	pcache, data, bufr, imgs2r()
errchk	malloc
include "ccdcache.com"

begin
	# Quick test for cached data.
	if (ccd_maxcache == 0)
	    return (imgs2r (im, col1, col2, line, line))

	# Return cached data.
	if (IM_PIXTYPE(im) == TY_REAL) {
	    for (i=1; i<=ccd_ncache; i=i+1) {
		pcache = Memi[ccd_pcache+i-1]
		if (CCD_IM(pcache) == im) {
		    if (CCD_SZDATA(pcache) > 0)
		        return (CCD_DATA(pcache)+(line-1)*IM_LEN(im,1)+col1-1)
		    else
			break
		}
	    }
	} else {
	    for (i=1; i<=ccd_ncache; i=i+1) {
		pcache = Memi[ccd_pcache+i-1]
		if (CCD_IM(pcache) == im) {
		    if (CCD_SZDATA(pcache) > 0) {
		        data = CCD_DATA(pcache)+(line-1)*IM_LEN(im,1)+col1-1
		        bufr = CCD_BUFR(pcache)
		        if (bufr == NULL) {
			    call malloc (bufr, IM_LEN(im,1), TY_REAL)
			    CCD_BUFR(pcache) = bufr
			}
		        call achtsr (Mems[data], Memr[bufr], IM_LEN(im,1))
		        return (bufr)
		    } else
			break
		}
	    }
	}

	# Return uncached data.
	return (imgs2r (im, col1, col2, line, line))
end


# CCD_GLS -- Get a line of short data from the image.
# If the image data is cached this is fast (particularly if the datatype
# matches).  If the image data is not cached then use IMIO.

pointer procedure ccd_gls (im, col1, col2, line)

pointer	im			# IMIO pointer
int	col1, col2		# Columns
int	line			# Line

int	i
pointer	pcache, data, bufs, imgs2s()
errchk	malloc
include "ccdcache.com"

begin
	# Quick test for cached data.
	if (ccd_maxcache == 0)
	    return (imgs2s (im, col1, col2, line, line))

	# Return cached data.
	if (IM_PIXTYPE(im) == TY_SHORT) {
	    for (i=1; i<=ccd_ncache; i=i+1) {
		pcache = Memi[ccd_pcache+i-1]
		if (CCD_IM(pcache) == im) {
		    if (CCD_SZDATA(pcache) > 0)
		        return (CCD_DATA(pcache)+(line-1)*IM_LEN(im,1)+col1-1)
		    else
			break
		}
	    }
	} else {
	    for (i=1; i<=ccd_ncache; i=i+1) {
		pcache = Memi[ccd_pcache+i-1]
		if (CCD_IM(pcache) == im) {
		    if (CCD_SZDATA(pcache) > 0) {
		        data = CCD_DATA(pcache)+(line-1)*IM_LEN(im,1)+col1-1
		        bufs = CCD_BUFS(pcache)
		        if (bufs == NULL) {
			    call malloc (bufs, IM_LEN(im,1), TY_SHORT)
			    CCD_BUFS(pcache) = bufs
		        }
		        call achtrs (Memr[data], Mems[bufs], IM_LEN(im,1))
		        return (bufs)
		    } else
			break
		}
	    }
	}

	# Return uncached data.
	return (imgs2s (im, col1, col2, line, line))
end
