include <imhdr.h>
include <ctype.h>
include <mach.h>
include "i2sun.h"


# I2SUN -- IRAF to Sun Rasterfile:  convert either a list of IRAF images 
# or all slices from a specified axis of a dimension>2 image into a series
# of Sun rasterfiles.  This format-specific task is primarily used to make
# movies in the absence of a portable movie/filmloop utility, if a
# Sun-specific movie task is available.
# ** The format of the output Sun rasterfiles is hard-coded into this task,
# ** and thus could diverge from a future Sun format; we do not want to link
# ** with Sun libraries, as this task should be runnable on other machines.

procedure t_i2sun

pointer	sp, tr, input, im, rfnames, clutfile, transform, cur_rf
pointer	ulutfile, ulut, colormap, pk_colormap, lut
int	list, lfd, rfd, nslices, stat, nimages
int	rheader[RAS_HDR_INTS], ras_maptype, ras_maplength, frame, slice, i, j
short	lut1, lut2
bool	use_clut, make_map

pointer immap()
int	open(), access(), clgeti(), imtopenp(), imtlen(), imtgetim(), read()
real	clgetr()
bool	streq(), clgetb()

errchk	open()

begin
	call smark (sp)
	call salloc (tr, LEN_TR, TY_STRUCT)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (rfnames, SZ_FNAME, TY_CHAR)
	call salloc (clutfile, SZ_FNAME, TY_CHAR)
	call salloc (cur_rf, SZ_FNAME, TY_CHAR)
	call salloc (transform, SZ_LINE, TY_CHAR)
	call salloc (pk_colormap, NGREY*3, TY_CHAR)
	call salloc (colormap, NGREY*3, TY_SHORT)
	lut = NULL
	im = NULL

	# Input parameters.
	list = imtopenp ("input")
	call clgstr ("output", Memc[rfnames], SZ_FNAME)
	call clgstr ("clutfile", Memc[clutfile], SZ_FNAME)
	call clgstr ("ztrans", Memc[transform], SZ_LINE)
	TR_Z1(tr) = clgetr ("z1")
	TR_Z2(tr) = clgetr ("z2")
	TR_ZTRANS(tr) = Z_LINEAR
	if (streq (Memc[transform], "log"))
	    TR_ZTRANS(tr) = Z_LOG
	else if (streq (Memc[transform], "none"))
	    TR_ZTRANS(tr) = Z_UNITARY
	else if (streq (Memc[transform], "user")) {

	    # Get user-specified transfer lookup table.
	    TR_ZTRANS(tr) = Z_USER
	    call salloc (ulutfile, SZ_FNAME, TY_CHAR)
	    call clgstr ("ulutfile", Memc[ulutfile], SZ_FNAME)

	    # Borrowed from DISPLAY; mallocs storage for ulut:
	    call tr_ulut (Memc[ulutfile], TR_Z1(tr), TR_Z2(tr), ulut)
	}
	TR_XSIZE(tr) = clgeti ("xsize")
	TR_YSIZE(tr) = clgeti ("ysize")
	TR_ORDER(tr) = clgeti ("order")
	TR_XMAG(tr)  = clgetr ("xmag")
	TR_YMAG(tr)  = clgetr ("ymag")

	# Get input image axes to map to output frames.  At present we
	# can only traverse one slice axis.
	TR_SLICEAXIS(tr) = clgeti ("sliceaxis")

	# Swap bytes in output rasterfile?  (useful when I2SUN run on VAX etc.)
	TR_SWAPBYTES(tr) = clgetb ("swap")

	# Check if there are no images.
	nimages = imtlen (list)
	if (nimages == 0) {
	    call eprintf (0, "No input images to convert")
	    goto wrapup_
	}

	# Open color lookup table file (an existing Sun rasterfile at present)
	if (access (Memc[clutfile], READ_ONLY, BINARY_FILE) == YES) {
	    lfd = open (Memc[clutfile], READ_ONLY, BINARY_FILE)
	    use_clut = true
	} else
	    use_clut = false

	# Read color lookup table.
	make_map = false
	if (use_clut) {
	    # Only the color table is used from the rasterfile; ignore all else.
	    stat = read (lfd, rheader, RAS_HDR_INTS * SZB_CHAR)
	    if (stat != RAS_HDR_INTS * SZB_CHAR) {
		call eprintf ("Error reading header from file `%s'\n")
		    call pargstr (Memc[clutfile])
		goto wrapup_
	    }
	    if (rheader[1] != RAS_MAGIC) {
		call eprintf ("File `%s' not a valid Sun rasterfile\n")
		    call pargstr (Memc[clutfile])
		goto wrapup_
	    }
	    ras_maptype = rheader[7]
	    ras_maplength = rheader[8]
	    if (ras_maptype != RMT_NONE && ras_maplength > 0) {
		stat = read (lfd, Memc[colormap], ras_maplength / SZB_CHAR)
		if (stat != ras_maplength / SZB_CHAR) {
		    call eprintf ("Error reading colormap from %s\n")
			call pargstr (Memc[clutfile])
		    goto wrapup_
		}
		# Colormap was already packed on disk.
		call achtsc (Mems[colormap], Memc[pk_colormap], ras_maplength)
	    } else {
		make_map = true
		call eprintf ("Invalid colormap in %s; using greyscale\n")
		    call pargstr (Memc[clutfile])
	    }
	} else
	    make_map = true

	if (make_map) {
	    # Construct a greyscale colormap of same range as IMTOOL.
	    ras_maptype = RMT_EQUAL_RGB
	    ras_maplength = NGREY * 3
	    do i = 1, 3 {
		Mems[colormap+(i-1)*NGREY] = WHITE
		do j = COLORSTART+1, COLOREND
		    Mems[colormap+j-1+(i-1)*NGREY] = j * (WHITE+1) /
			NGREY
		Mems[colormap+COLOREND-1+1+(i-1)*NGREY] = WHITE
		do j = COLOREND+2, NGREY
		    Mems[colormap+j-1+(i-1)*NGREY] = BLACK
	    }
	    call achtsc (Mems[colormap], Memc[pk_colormap], ras_maplength)

	    # Pack to byte stream.
	    call chrpak (Memc[pk_colormap], 1, Memc[pk_colormap], 1,
		ras_maplength)
	}

	# For each IRAF image or band, construct and dispose of a rasterfile.
	frame = 0
	while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {
	    im = immap (Memc[input], READ_ONLY, 0)
	    if (IM_NDIM(im) > 2 && TR_SLICEAXIS(tr) > IM_NDIM(im)) {
		call eprintf ("Specified slice axis invalid for image %s\n")
		    call pargstr (Memc[input])
		goto wrapup_
	    }
	    nslices = IM_LEN(im, TR_SLICEAXIS(tr))
	    if (nslices < 1)
		nslices = 1

	    # Set up spatial transformation (technically, could be different
	    # for each input image).
	    call tr_setup (im, tr)

	    # We assume that if any n>2 images are present, the user wants
	    # all bands dumped out.
	    do slice = 1, nslices {

		# Construct next rasterfile name and open file; works in
		# 'append' mode, next higher available frame number.
		call sprintf (Memc[cur_rf], SZ_FNAME, Memc[rfnames])
		    call pargi (frame)
		while (access (Memc[cur_rf], READ_ONLY, BINARY_FILE) == YES) {
		    frame = frame + 1
		    call sprintf (Memc[cur_rf], SZ_FNAME, Memc[rfnames])
			call pargi (frame)
		}
		iferr (rfd = open (Memc[cur_rf], NEW_FILE, BINARY_FILE)) {
		    call eprintf ("Cannot open output rasterfile `%s'\n")
			call pargstr (Memc[cur_rf])
		    goto wrapup_
		}
		frame = frame + 1

		# Write header to rasterfile:
		rheader[1] = RAS_MAGIC
		rheader[2] = TR_XE(tr) - TR_XS(tr) + 1
		rheader[3] = TR_YE(tr) - TR_YS(tr) + 1
		rheader[4] = NBITS_FB
		rheader[5] = rheader[2] * rheader[3]
		rheader[6] = RMT_STANDARD
		rheader[7] = ras_maptype
		rheader[8] = ras_maplength
		if (TR_SWAPBYTES(tr))
		    call bswap4 (rheader, 1, rheader, 1, RAS_HDR_INTS*4)
		call write (rfd, rheader, RAS_HDR_INTS * SZB_CHAR)

		# Write colormap to rasterfile.
		call write (rfd, Memc[pk_colormap], ras_maplength / SZB_CHAR)

		# Verify user-specified transfer function parameters.
		if (TR_ZTRANS(tr) == Z_USER) {
		    call alims (Mems[ulut], U_MAXPTS, lut1, lut2)
		    if (lut2 < short(DSP_MIN) || lut1 > short(DSP_MAX)) {
			call eprintf ("User specified greyscales <> range\n")
			call eprintf ("ulut1=%D, dmin=%D; ulut2=%D, dmax=%D\n")
			    call pargi (lut1)
			    call pargi (DSP_MIN)
			    call pargi (lut2)
			    call pargi (DSP_MAX)
		    }
		    if (!IS_INDEF(TR_Z1(tr)) && !IS_INDEF(TR_Z2(tr)) &&
			TR_Z2(tr) < IM_MIN(im) || TR_Z1(tr) > IM_MAX(im)) {
			call eprintf ("User specified intensities <> range\n")
			call eprintf ("z1=%g, im_min=%g; z2=%g, im_max=%g\n")
			    call pargr (TR_Z1(tr))
			    call pargr (IM_MIN(im))
			    call pargr (TR_Z2(tr))
			    call pargr (IM_MAX(im))
			call eprintf ("continuing anyway.\n")
		    }
		}

		# Read image pixels and write to rasterfile.
		call cnv_image (im, slice, tr, ulut, rfd)

		call close (rfd)
	    }
	    call imunmap (im)
	}

wrapup_
	if (im != NULL)
	    call imunmap(im)
	call imtclose (list)
	call close (rfd)
	call sfree (sp)
	if (ulut != NULL)
	    call mfree (ulut, TY_SHORT)
end
