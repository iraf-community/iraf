include <fset.h>
include <gset.h>
include <imhdr.h>
include "tvmark.h"

# T_TVMARK -- Mark dots circles and squares on the image in the image display
# with optional numbering.

procedure t_tvmark ()

pointer	image	               	# pointer to name of the image
pointer	outimage 		# pointer to output image
pointer	coords			# pointer to coordinate file
pointer	logfile			# pointer to the log file
pointer	font			# pointer to the font
int	autolog			# automatically log commands
int	interactive		# interactive mode

pointer	sp, mk, im, iw, outim, cfilename
int	cl, log, ft, frame, ltid, wcs_status

bool	clgetb()
int	btoi(), clgeti()
pointer	immap(), open(), imd_mapframe(), iw_open()

begin
	# Set standard output to flush on newline.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate working space.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (coords, SZ_FNAME, TY_CHAR)
	call salloc (outimage, SZ_FNAME, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)
	call salloc (font, SZ_FNAME, TY_CHAR)
	call salloc  (cfilename, SZ_FNAME, TY_CHAR)

	frame = clgeti ("frame")
	call clgstr ("coords", Memc[coords], SZ_FNAME)
	call clgstr ("outimage", Memc[outimage], SZ_FNAME)
	call clgstr ("logfile", Memc[logfile], SZ_FNAME)
	call clgstr ("font", Memc[font], SZ_FNAME)
	call clgstr ("commands.p_filename", Memc[cfilename], SZ_FNAME)
	autolog = btoi (clgetb ("autolog"))
	interactive = btoi (clgetb ("interactive"))

	# Fetch the marking parameters.
	call mk_gpars (mk)
	
	# Open the frame as an image.
	im = imd_mapframe (frame, READ_WRITE, YES)
	iw = iw_open (im, frame, Memc[image], SZ_FNAME, wcs_status)
	call mk_sets (mk, IMAGE, Memc[image], SZ_FNAME)
	call mk_seti (mk, FRAME, frame)

	# Open the coordinate file.
	if (Memc[coords] != EOS) {
	    iferr (cl = open (Memc[coords], READ_WRITE, TEXT_FILE)) {
		cl = open (Memc[coords], NEW_FILE, TEXT_FILE)
		call close (cl)
		cl = open (Memc[coords], READ_WRITE, TEXT_FILE)
	    }
	} else
	    cl = NULL
	call mk_sets (mk, COORDS, Memc[coords])

	# Save the output mage name
	call mk_sets (mk, OUTIMAGE, Memc[outimage])

	# Open the font file.
	#if (Memc[font] != EOS)
	    #ft = open (Memc[font], READ_ONLY, TEXT_FILE)
	#else
	    ft = NULL
	call mk_sets (mk, FONT, Memc[font])

	# Mark the image frame.
	if (interactive == NO) {
	    if (Memc[cfilename] != EOS)
		call mk_mark (mk, im, iw, cl, NULL, ft, autolog, NO)
	    else {

	    	# Open the output image.
		if (Memc[outimage] != EOS)
	    	    outim = immap (Memc[outimage], NEW_COPY, im)
		else
	    	    outim = NULL

		# Do the marking.
		ltid = 0
		if (cl != NULL)
	            call mk_bmark (mk, im, iw, cl, ltid, ft)

		# Copy / close image.
	        if (outim != NULL) {
		    call mk_imcopy (im, outim)
	            call imunmap (outim)
		}
	    }

	} else {

	    # Open the log file.
	    if (Memc[logfile] != EOS)
	        log = open (Memc[logfile], NEW_FILE, TEXT_FILE)
	    else
	        log = NULL
	    call mk_sets (mk, LOGFILE, Memc[logfile])
	    call mk_seti (mk, AUTOLOG, autolog)

	    call mk_mark (mk, im, iw, cl, log, ft, autolog, YES)

	    if (log != NULL)
	        call close (log)
	}

	# Close up the file lists and free memory.
	call iw_close (iw)
	call imunmap (im)
	if (cl != NULL)
	    call close (cl)
	if (ft != NULL)
	    call close (ft)

	# Free immark structure.
	call mkfree (mk)

	call sfree (sp)
end


# MK_IMCOPY -- Make a snap of the frame buffer.

procedure mk_imcopy (in, out)

pointer	in		# pointer to the input image
pointer	out		# pointe to the output image

int	i, ncols, nlines
pointer	sp, vin, vout, inbuf, outbuf
pointer	imgnls(), impnls()
errchk	imgnls(), impnls()

begin
	call smark (sp)
	call salloc (vin, IM_MAXDIM, TY_LONG)
	call salloc (vout, IM_MAXDIM, TY_LONG)

	ncols = IM_LEN(in, 1)
	nlines = IM_LEN(in, 2)
	call amovkl (long(1), Meml[vin], IM_MAXDIM)
	call amovkl (long(1), Meml[vout], IM_MAXDIM)

	do i = 1, nlines {
	    if (impnls (out, outbuf, Meml[vout]) == EOF)
	        call error (0, "Error writing output image.\n")
	    if (imgnls (in, inbuf, Meml[vin]) == EOF)
	        call error (0, "Error reading frame buffer.\n")
	    call amovs (Mems[inbuf], Mems[outbuf], ncols)
	}

	call imflush (out)
	call sfree (sp)
end


# MK_IMSECTION -- Restore a section of an image to an image of the same
# size.

procedure mk_imsection (in, out, x1, x2, y1, y2) 

pointer	in		# input image
pointer	out		# output image
int	x1, x2		# column limits
int	y1, y2		# line limits

int	i, ix1, ix2, iy1, iy2, ncols, nlines
pointer	ibuf, obuf
pointer	imps2s(), imgs2s()

begin
	ncols = IM_LEN(in,1)
	nlines = IM_LEN(in,2)

	ix1 = min (x1, x2)
	ix2 = max (x1, x2)
	ix1 = max (1, min (ncols, ix1)) 
	ix2 = min (ncols, max (1, ix2))

	iy1 = min (y1, y2)
	iy2 = max (y1, y2)
	iy1 = max (1, min (ncols, iy1)) 
	iy2 = min (ncols, max (1, iy2))

	do i = iy1, iy2 {
	    obuf = imps2s (out, ix1, ix2, i, i)
	    ibuf = imgs2s (in, ix1, ix2, i, i)
	    call amovs (Mems[ibuf], Mems[obuf], ix2 - ix1 + 1)
	}

	call imflush (out)
end
