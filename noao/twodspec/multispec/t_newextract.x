include <imhdr.h>
include "ms.h"

# T_NEW_EXTRACTION -- Create a new extraction database.
#
# This is the first step in using the MULTISPEC package.  The new database
# may be created from scratch or intialized from an template image.

procedure t_new_extraction ()

# Task parameters:
char	image[SZ_FNAME]			# Multi-spectra image
char	template[SZ_FNAME]		# Template image
int	samples[3, MAX_RANGES]		# Sample line range array

char	comment[SZ_LINE]
char	database[SZ_FNAME], old_database[SZ_FNAME]
pointer	im, ms

bool	strne()
int	clgranges(), expand_ranges()
pointer	immap(), msmap()

begin
	# Get database and image name.  Map the image and check that
	# it is two dimensional.
	call clgstr ("image", image, SZ_FNAME)
	im = immap (image, READ_ONLY, 0)
	if (IM_NDIM(im) != 2)
	    call error (MS_ERROR, "Image file must be two dimensional.")

	# Get the template image name.
	call clgstr ("template", template, SZ_FNAME)

	if (strne (template, "")) {
	    # If a template is given then map the template and check
	    # that the new image dimensions agree with the old dimensions.

	    ms = msmap (template, READ_ONLY, 0)
	    if ((MS_LEN(ms, 1) != IM_LEN(im, 1)) ||
		(MS_LEN(ms, 2) != IM_LEN(im, 2)))
		call error (MS_ERROR,
		    "New image size does not agree with the old image size.")
	    call msunmap (ms)

	    # Copy the old database.  Map the new database and clear the
	    # the old comments before adding a new comment.

	    call sprintf (database, SZ_FNAME, "%s.db")
		call pargstr (image)
	    call sprintf (old_database, SZ_FNAME, "%s.db")
		call pargstr (template)
	    call fcopy (old_database, database)

	    ms = msmap (image, READ_WRITE, 0)
	    COMMENT(ms, 1) = EOS
	    call sprintf (comment, SZ_LINE,
		"Database initialized from the template image %s.")
		call pargstr (template)
	    call history (ms, comment)

	} else {
	    # For a new database initialize the  header parameters.
	    ms = msmap (image, NEW_FILE, MS_DB_ENTRIES)
	    MS_LEN(ms, 1) = IM_LEN(im, 1)
	    MS_LEN(ms, 2) = IM_LEN(im, 2)
	    MS_NSPECTRA(ms) = 0

	    # Get the sample line ranges and set the number of sample lines
	    # in the database header.
	    MS_NSAMPLES(ms) = clgranges ("sample_lines", 1, MS_LEN (ms, 2),
		samples, MAX_RANGES)

	    # Make an entry in the database for the sample lines and then
	    # access the entry in order to allocate memory for the sample
	    # line array.
	    call dbenter (MS_DB(ms), NAME(ms, SAMPLE), MS_NSAMPLES(ms)*SZ_INT,1)
	    call msgsample (ms)

	    # Expand the sample line range array into the sample line array.
	    # Then put the sample line array in the database.
	    MS_NSAMPLES(ms) = expand_ranges (samples, LINE(ms, 1),
		MS_NSAMPLES(ms))
	    call mspsample (ms)

	    # Add a history line.
	    call history (ms, "New MULTISPEC database created.")
	}

	# Set the image name and image title in the database.
	call strcpy (image, MS_IMAGE(ms), SZ_MS_IMAGE)
	call strcpy (IM_TITLE(im), MS_TITLE(ms), SZ_MS_TITLE)

	# Close image and database.  Write the database header record before
	# closing the database.
	call imunmap (im)
	call msphdr (ms)
	call msunmap (ms)
end
