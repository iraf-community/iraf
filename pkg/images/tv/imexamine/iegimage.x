# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	"imexam.h"

# IE_GIMAGE -- Get input image name and return IMIO pointer.
# If examining a list of images access the indexed image, displaying it if
# not already displayed.  Otherwise the image loaded into the current display
# frame is displayed, if it can be accessed, or the image frame buffer itself
# is examined.  If there is neither a list of images nor display access the
# user is queried for the name of the image to be examined.
# This procedure uses a prototype display interface (IMD/IW).
 
pointer procedure ie_gimage (ie, select)
 
pointer	ie			#I IMEXAM pointer
int	select			#I select frame?

char	errstr[SZ_FNAME]
int	frame, i, j, k
pointer	sp, image, dimage, imname, im

int	imtrgetim(), fnldir(), errget()
bool	strne(), streq()
pointer	imd_mapframe(), immap()
errchk	imd_mapframe, immap, ie_display, ie_mwinit

 
begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (dimage, SZ_FNAME, TY_CHAR)

	# Get image name, and display image if using display.  If we are
	# examining a list of images, the list and the current index into
	# the list determine the image to be examined.  If there is no list
	# we examine the currently displayed images, if any, else the
	# contents of the image display frame buffers are examined as images.

	if (IE_LIST(ie) != NULL) {
	    # Get image name.
	    IE_INDEX(ie) = max(1, min(IE_LISTLEN(ie), IE_INDEX(ie)))
	    if (imtrgetim (IE_LIST(ie), IE_INDEX(ie), Memc[image],
		SZ_FNAME) == EOF)
		call error (1, "Reference outside of image list")

	    # Display image.
	    if (IE_USEDISPLAY(ie) == YES) {
		# Is named image currently loaded into the image display?
		frame = 0
		if (streq (Memc[image], IE_IMAGE(ie)))
		    frame = IE_MAPFRAME(ie)
		else {
		    if (IE_DS(ie) == NULL)
			IE_DS(ie) = imd_mapframe (max (1, IE_NEWFRAME(ie)/100),
			    READ_WRITE, NO)

		    do i = 1, IE_NFRAMES(ie) {
			if (i == IE_MAPFRAME(ie)/100)
			    next
			do j = 1, 99 {
			   k = i * 100 + j
			   iferr (call ie_imname (IE_DS(ie), k,
			       Memc[dimage], SZ_FNAME))
			       break
			    if (streq (Memc[image], Memc[dimage])) {
				frame = k
				break
			    }
			}
			if (frame != 0)
			    break
		    }
		}

		# Load image into display frame if not already loaded.
		# If the allframes option is specified cycle through the
		# available display frames, otherwise resuse the same frame.

		if (frame == 0) {
		    if (IE_DS(ie) != NULL) {
			if (IE_IM(ie) == IE_DS(ie))
			    IE_IM(ie) = NULL
			call imunmap (IE_DS(ie))
		    }

		    frame = 100 * max (1, IE_DFRAME(ie) / 100) + 1
		    call ie_display (ie, Memc[image], frame/100)

		    IE_MAPFRAME(ie) = 0
		    if (IE_ALLFRAMES(ie) == YES) {
			IE_DFRAME(ie) = frame + 100
			if (IE_DFRAME(ie)/100 > IE_NFRAMES(ie))
			    IE_DFRAME(ie) = 101
		    }
		}

		# Map and display-select the frame.
		if (frame != IE_MAPFRAME(ie) || frame != IE_NEWFRAME(ie)) {
		    if (IE_DS(ie) != NULL) {
			if (IE_IM(ie) == IE_DS(ie))
			    IE_IM(ie) = NULL
			call imunmap (IE_DS(ie))
		    }
		    IE_DS(ie) = imd_mapframe (frame/100, READ_WRITE, select)
		    IE_MAPFRAME(ie) = frame
		    IE_NEWFRAME(ie) = frame
		}
	    }

	} else if (IE_USEDISPLAY(ie) == YES) {
	    # Map the new display frame.
	    if (IE_NEWFRAME(ie) != IE_MAPFRAME(ie)) {
		if (IE_NEWFRAME(ie)/100 != IE_MAPFRAME(ie)/100) {
		    if (IE_DS(ie) != NULL) {
			if (IE_IM(ie) == IE_DS(ie))
			    IE_IM(ie) = NULL
			call imunmap (IE_DS(ie))
		    }
		    IE_DS(ie) = imd_mapframe (IE_NEWFRAME(ie)/100, READ_WRITE,
			select)
		}
		IE_MAPFRAME(ie) = IE_NEWFRAME(ie)
	    }

	    # Get the image name.
	    call ie_imname (IE_DS(ie), IE_MAPFRAME(ie), Memc[image], SZ_FNAME)

	} else
	    call clgstr ("image", Memc[image], SZ_FNAME)

	# Check if the image has not been mapped and if so map it.
	# Possibly log any change of image.  Always map the physical image,
	# not a section, since we do everything in image coordinates.

	if (IE_IM(ie) == NULL || strne (Memc[image], IE_IMAGE(ie))) {

	    # Strip the path.
	    call imgcluster (Memc[image], Memc[imname], SZ_FNAME)
	    i = fnldir (Memc[imname], Memc[imname], SZ_FNAME)
	    call strcpy (Memc[image+i], IE_IMNAME(ie), IE_SZFNAME)

	    # Map the image.
	    iferr (im = immap (Memc[image], READ_ONLY, 0)) { 
		# Warn user once.
		i = errget (Memc[imname], SZ_FNAME)
		if (strne (Memc[imname], errstr)) {
		    call erract (EA_WARN)
		    call strcpy (Memc[imname], errstr, SZ_FNAME)
		}

		# Access the display frame buffer as the data image.
		if (IE_USEDISPLAY(ie) == YES && IE_LIST(ie) == NULL) {
		    if (IE_IM(ie) != NULL && IE_IM(ie) != IE_DS(ie))
			iferr (call imunmap (IE_IM(ie)))
			    ;
		    IE_IM(ie) = IE_DS(ie)
		    call sprintf (IE_IMAGE(ie), IE_SZFNAME, "Frame.%d(%s)")
			call pargi (IE_MAPFRAME(ie))
			call pargstr (IE_IMNAME(ie))
		    call strcpy ("Contents of raw image frame buffer\n",
			IM_TITLE(IE_IM(ie)), SZ_IMTITLE)
		} else
		    call erract (EA_WARN)
		    
	    } else {
		# Adjust image sections.
		call ie_gimage1 (im, Memc[image], Memc[imname], SZ_FNAME)
		if (strne (Memc[image], Memc[imname])) {
		   call imunmap (im)
		   im = immap (Memc[imname], READ_ONLY, 0)
		}

		# Make the new image the current one.
		errstr[1] = EOS
		call strcpy (Memc[image], IE_IMAGE(ie), IE_SZFNAME)
		if (IE_IM(ie) != NULL && IE_IM(ie) != IE_DS(ie))
		    iferr (call imunmap (IE_IM(ie)))
			;
		if (IE_MW(ie) != NULL)
		    call mw_close (IE_MW(ie))
		IE_IM(ie) = im
		if (IE_LOGFD(ie) != NULL) {
		    call fprintf (IE_LOGFD(ie), "# [%d] %s - %s\n")
			call pargi (IE_INDEX(ie))
			call pargstr (IE_IMNAME(ie))
			call pargstr (IM_TITLE(IE_IM(ie)))
		}
	    }
	}

	call ie_mwinit (ie)
	
	call sfree (sp)
	return (IE_IM(ie))
end


# IE_GIMAGE1 -- Convert input image section name to a 2D physical image section.

procedure ie_gimage1 (im, input, output, maxchar)

pointer	im			#I IMIO pointer
char	input[ARB]		#I Input image name
char	output[maxchar]		#O Output image name
int	maxchar			#I Maximum characters in output name.

int	i, fd
pointer	sp, section, lv, pv1, pv2

int	stropen(), strlen()
bool	streq()

begin
	call smark (sp)
	call salloc (section, SZ_FNAME, TY_CHAR)
	call salloc (lv, IM_MAXDIM, TY_LONG)
	call salloc (pv1, IM_MAXDIM, TY_LONG)
	call salloc (pv2, IM_MAXDIM, TY_LONG)

	# Get endpoint coordinates in original image.
	call amovkl (long(1), Meml[lv], IM_MAXDIM)
	call aclrl (Meml[pv1], IM_MAXDIM)
	call imaplv (im, Meml[lv], Meml[pv1], 2)
	call amovl (IM_LEN(im,1), Meml[lv], IM_NDIM(im))
	call aclrl (Meml[pv2], IM_MAXDIM)
	call imaplv (im, Meml[lv], Meml[pv2], 2)

	# Set image section.
	fd = stropen (Memc[section], SZ_FNAME, NEW_FILE)
	call fprintf (fd, "[")
	do i = 1, IM_MAXDIM {
	    if (Meml[pv1+i-1] != Meml[pv2+i-1])
		call fprintf (fd, "*")
	    else if (Meml[pv1+i-1] != 0) {
		call fprintf (fd, "%d")
		call pargi (Meml[pv1+i-1])
	    } else
		break
	    call fprintf (fd, ",")
	}
	call close (fd)
	i = strlen (Memc[section])
	Memc[section+i-1] = ']'

	if (streq ("[*,*]", Memc[section]))
	    Memc[section] = EOS

	# Strip existing image section and add new section.
	call imgimage (input, output, maxchar)
	call strcat (Memc[section], output, maxchar)
	
#	if (Memc[section] == EOS)
#	    call imgimage (input, output, maxchar)
#	else
#	    call strcpy (input, output, maxchar)

	call sfree (sp)
end
