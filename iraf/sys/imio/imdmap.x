# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<imset.h>
include	<imhdr.h>
include	<imio.h>

# IMDMAP -- Map an image display frame as an imagefile.  Equivalent to
# the ordinary immap, except that the pixel storage file is the image
# frame buffer.  The special pixel storage file is pre-opened with
# IMDOPEN.  Upon the first pixel access, IMIO normally opens the pixfile.
# In this case, it sees that the file has already been opened (as a
# special device as it turns out), and simply uses it.

pointer procedure imdmap (device, access_mode, imdopen)

char	device[ARB]		# graphcap name of display device to be opened
int	access_mode		# display access mode
extern	imdopen()		# device FIO open procedure
int	imdopen()

int	pfd, pixel_mode
pointer	sp, devinfo, devname, im, tty

bool	streq(), ttygetb()
pointer	immap(), ttygdes()
int	ttygeti(), ttygets(), envgets(), btoi()
errchk	imdopen, immap, syserrs

begin
	call smark (sp)
	call salloc (devinfo, SZ_LINE,  TY_CHAR)
	call salloc (devname, SZ_FNAME, TY_CHAR)

	# Determine the display access mode.  Write permission is always
	# required, even to read from a display device.  Write only mode
	# is however desirable for the display, to avoid unnecessary i/o
	# when faulting the file buffer.

	switch (access_mode) {
	case READ_ONLY:
	    pixel_mode = READ_WRITE
	case READ_WRITE:
	    pixel_mode = READ_WRITE
	case WRITE_ONLY:
	    pixel_mode = WRITE_ONLY
	default:
	    # Cannot create an image on a special device.
	    call syserrs (SYS_IMDEVOPN, device)
	}

	# Open an image header for the special device.
	im = immap ("dev$null", NEW_IMAGE, 0)

	# Read the graphcap entry for the device and fetch the device
	# parameters.

	if (streq (device, "stdimage")) {
	    if (envgets ("stdimage", Memc[devname], SZ_FNAME) <= 0) {
		call imunmap (im)
		call syserrs (SYS_IMDEVOPN, device)
	    }
	} else
	    call strcpy (device, Memc[devname], SZ_FNAME)
		
	iferr (tty = ttygdes (Memc[devname])) {
	    call imunmap (im)
	    call erract (EA_ERROR)
	}

	if (ttygets (tty, "DD", Memc[devinfo], SZ_LINE) <= 0) {
	    call imunmap (im)
	    call ttycdes (tty)
	    call syserrs (SYS_IMDEVOPN, device)
	}

	IM_PIXTYPE(im)  = TY_SHORT
	IM_LEN(im,1)    = ttygeti (tty, "xr")
	IM_LEN(im,2)    = ttygeti (tty, "yr")
	IM_LEN(im,3)    = ttygeti (tty, "cn")
	IM_LEN(im,4)    = btoi(ttygetb (tty, "LC"))
	IM_NDIM(im)     = 2
	IM_MIN(im)      = real (ttygeti (tty, "z0"))
	IM_MAX(im)      = real (ttygeti (tty, "zr") - 1.) + IM_MIN(im)
	IM_LIMTIME(im)  = IM_MTIME(im) + 1
	IM_PIXOFF(im)   = 1	
	IM_HGMOFF(im)   = NULL
	IM_BLIST(im)    = NULL
	IM_NPHYSDIM(im) = 2

	call amovl (IM_LEN(im,1), IM_PHYSLEN(im,1), IM_MAXDIM)
	call amovl (IM_LEN(im,1), IM_SVLEN(im,1),   IM_MAXDIM)

	# Open the display device.
	pfd = imdopen (Memc[devinfo], pixel_mode)
	if (pfd == ERR) {
	    call imunmap (im)
	    call syserrs (SYS_IMDEVOPN, device)
	}

	call imseti (im, IM_PIXFD, pfd)
	call imseti (im, IM_WHEADER, NO)
	call imsetbuf (pfd, im)

	call ttycdes (tty)
	call sfree (sp)

	return (im)
end
