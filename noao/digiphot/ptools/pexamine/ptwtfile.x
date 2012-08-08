include "../../lib/ptkeysdef.h"
include "pexamine.h"

# PT_WTFILE -- Write out the catalogs of selected and rejected objects.

procedure pt_wtfile (apd, key, apout, aprej, deleted, nstars)

int	apd		# input catalog file descriptor
pointer	key		# key structure for textfiles
int	apout		# output catalog file descriptor
int	aprej		# rejections catalog file descriptor
int	deleted[ARB]	# deletions array
int	nstars		# number of stars in the catalog

int	i, nselect, ndelete
int	pt_selrej()

begin
	# ST tables format.
	if (key == NULL) {

	    # Write out the good data.
	    if (apout != NULL) {
		call tbtcre (apout)
		call tbhcal (apd, apout)
		nselect = 0
		do i = 1, nstars {
		    if (deleted[i] == PX_DELETE)
			next
		    nselect = nselect + 1
		    call tbrcpy (apd, apout, i, nselect)
		}
	    }

	    # Write out the deletions.
	    if (aprej != NULL) {
		call tbtcre (aprej)
		call tbhcal (apd, aprej)
	        ndelete = 0
	        do i = 1, nstars {
		    if (deleted[i] != PX_DELETE)
			next
		    ndelete = ndelete + 1
		    call tbrcpy (apd, aprej, i, ndelete)
		}
	    }

	# Write out a text file.
	} else  {
	    if (pt_selrej (apd, apout, aprej, deleted, nstars) < nstars)
		;
	}
end


define	LEN_LONGLINE	10

# PT_SELREJ -- Select and/or reject records based on evaluating a logical
# expression.

int procedure pt_selrej (tp_in, tp_out, tp_rej, deleted, nstars)

int	tp_in		# the input catalog file descriptor
int	tp_out		# the output catalog file descriptor
int	tp_rej		# the rejections catalog file descriptor
int	deleted[ARB]	# the deletions array
int	nstars		# maximum number of stars

int	record, nchars, buflen, lenrecord
pointer	line, lline
int	getline()

begin
	# Check that output has been requested.
	if (tp_out == NULL &&  tp_rej == NULL)
	    return (0)

	# Rewind the input file.
	call seek (tp_in, BOF)

	# Initialize the file read.
	record = 0
	lenrecord = 0
	buflen = LEN_LONGLINE * SZ_LINE
	call malloc (line, SZ_LINE, TY_CHAR)
	call malloc (lline, buflen, TY_CHAR)

	# Loop over the text file records.
	repeat  {

	    # Read in a line of the text file.
	    nchars = getline (tp_in, Memc[line])
	    if (nchars == EOF)
		break

	    # Determine the type of record.
	    if (Memc[line] ==  KY_CHAR_POUND || Memc[line] == KY_CHAR_NEWLINE) {

		if (tp_out != NULL)
		    call putline (tp_out, Memc[line])
		if (tp_rej != NULL)
		    call putline (tp_rej, Memc[line])

	    } else {

		# Reallocate the temporary record space.
		if (lenrecord > buflen) {
		    buflen = buflen + SZ_LINE
		    call realloc (lline, buflen, TY_CHAR)
		}

		# Store the record.
		call amovc (Memc[line], Memc[lline+lenrecord], nchars)
		lenrecord = lenrecord + nchars
		Memc[lline+lenrecord] = EOS

	        # Do the record bookkeeping.
	        if (Memc[line+nchars-2] != KY_CHAR_CONT) {

		    # Increment the record counter.
		    record = record + 1

		    # Write out the expression.
		    if ((tp_out != NULL) && (deleted[record] != PX_DELETE))
			call putline (tp_out, Memc[lline])
		    if ((tp_rej != NULL) && (deleted[record] == PX_DELETE))
			call putline (tp_rej, Memc[lline])
		    if (record >= nstars)
			break

		    # Reinitialize the record read.
		    lenrecord = 0
	        }
	    }

	}

	# Cleanup.
	call mfree (line, TY_CHAR)
	call mfree (lline, TY_CHAR)

	return (record)
end
