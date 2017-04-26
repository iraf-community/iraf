# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <syserr.h>
include <imhdr.h>
include "imx.h"


# IMX_FILETYPE -- Determine the file type.

int procedure imx_filetype (fname)

char	fname[ARB]				#i file name

char	img[SZ_FNAME], name[SZ_FNAME], buf[SZ_LINE]

int	i, nchars, fd
bool	is_http_list

int	errcode(), open(), read(), access(), imaccess()
int	strncmp(), strsearch(), isdirectory()
pointer	im, immap()

begin
	# Check for a URL.
	if (strncmp ("http://", fname, 7) == 0) 
	    return (IMT_URL)

	call aclrc (name, SZ_FNAME)
	if (fname[1] == '@')
	    call strcpy (fname[2], name, SZ_FNAME)
	else
	    call strcpy (fname, name, SZ_FNAME)

 	# See if it is a directory.
	if (isdirectory (name, buf, SZ_LINE) > 0) 
	    return (IMT_DIR)

	# Check for concatenated strings.
	if (strsearch (fname, "//") > 0) {
	    if (isdirectory (fname, buf, SZ_LINE) > 0) 
	        return (IMT_DIR)
	    else
	        return (IMT_FILE)
	}

	call aclrc (img, SZ_FNAME)				# PHU
	call sprintf (img, SZ_FNAME, "%s[0]")
	    call pargstr (name)

	# Get a peek at the file.
	call aclrc (buf, SZ_LINE)
	if (imaccess (name, READ_ONLY) == YES ||
	    imaccess (img, READ_ONLY) == YES) {
	        return (IMT_IMAGE);
	} else if (access (name, 0, 0) == YES) {
	    fd = open (name, READ_ONLY, TEXT_FILE)
	    nchars = read (fd, buf, SZ_LINE)
	    call strupr (buf)
	    call close (fd)
	}

	# See if it might be an image of some kind.
	if (strncmp (buf, "SIMPLE", 6) == 0) {

	    ifnoerr (im = immap (name, READ_ONLY, 0)) {	# SIF, OIF, etc
	        call imunmap (im)
	        return (IMT_IMAGE)
	    }

	    do i = 0, 1 {					# MEF
	        call aclrc (img, SZ_FNAME)
	        call sprintf (img, SZ_FNAME, "%s[%d]")
	            call pargstr (name)
	            call pargi (i)

	        iferr (im = immap (img, READ_ONLY, 0)) {
                    switch (errcode()) {
                    case SYS_FXFRFEOF:
                        break
                    case SYS_IKIEXTN:
                        next
                    case SYS_IKIOPEN:
		        if (i == 0)
                            next
                        break
                    default:
                        call erract (EA_ERROR)
                    }
	        } else {
		    call imunmap (im)
		    return (IMT_IMAGE)
		}
	    }

	} else {
	
	    # If we get this far, we have a file of some kind.  See if it is a
 	    # list of URLs, a VOTable, or a plain file.
	    is_http_list = FALSE
	    fd = open (name, READ_ONLY, TEXT_FILE)
	    do i = 1, 10 {
	        call aclrc (buf, SZ_LINE)
	        nchars = read (fd, buf, SZ_LINE)
	        call strupr (buf)
	        if (strsearch (buf, "VOTABLE") > 0) {
	    	    call close (fd)
		    return (IMT_VOTABLE)
	        } else if (strncmp (buf, "http://", 7) == 0)
	    	    is_http_list = TRUE
	    }
	    call close (fd)
	}

	if (is_http_list)
	    return (IMT_TABLE)
	else
	    return (IMT_FILE)
end
