include <imhdr.h>
include	<imio.h>
include "gf.h"

#* HISTORY *
#* B.Simon	19-Nov-99	Original code
#* B.Simon	13-Nov-00	Handle keywords both in primary and extension
#* B.Simon	 6-Dec-00	Use hash mark routines to track keywords
#* B.Simon	30-Jan-01	Added gf_putxtn

# GF_SPLIT_UA -- Split the user area of an image into primary and extension 

procedure gf_split_ua (im, db, prim, ext)

pointer	im	# i: image descriptor
pointer	db	# i: database of extension keywords
int	prim	# o: primary header spool file descriptor
int	ext	# o: primary header spool file descriptor

#--
bool	has_extend
int	in, fd, oldfd, hist
pointer	ua, sp, record, keyword, both

int	open(), stropen(), getline(), strncmp()
int	gf_imtype(), gf_find_db(), gf_findhash(), gf_getmkhash()

begin
	# Only update fits images with extensions that have been written to
	
	if (IM_ACMODE(im) == READ_ONLY || gf_imtype (im) != FITS_FMT || 
	    gf_find_db (im, PARAM_EXTEND) == NO) {

	    prim = NULL
	    ext = NULL
	    return
	}

	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (record, SZ_LINE, TY_CHAR)
	call salloc (keyword, SZ_KEYWORD, TY_CHAR)

	if (gf_find_db (im, PARAM_UPDATE) == NO) {
	    prim = NULL
	} else {
	    prim = open ("primary", READ_WRITE, SPOOL_FILE)
	}

	ext = open ("extension", READ_WRITE, SPOOL_FILE)

	ua = IM_USERAREA(im)
	in = stropen (Memc[ua], ARB, READ_ONLY)

	if (db == NULL) {
	    # No database means copy all keywords to primary header
	    call fcopyo (in, prim)
	} else {
	    # Create a hash of keywords that are 
	    # in the user area more than once

	    call gf_both (im, both)

	    oldfd = ext
	    has_extend = false

	    hist = gf_find_db(im, PARAM_HIST)
	    if (hist != -1) {
	       call seek(hist, EOF) # Append new HISTORY to existing spool.
	    }

            call seek(in, BOF)

	    while (getline (in, Memc[record]) != EOF) {
		# Check for presence of extend card

		if (strncmp (Memc[record], "EXTEND  ", 8) == 0)
		    has_extend = true

		# Determine whether keyword is in primary 
		# or extension header

		switch (gf_findhash (db, Memc[record])) {
		case NOT_FOUND:
		    fd = prim

		case IS_FOUND:
		    # If a keyword is in the extension, check to see
		    # if it is also in the prmary by checking both
		    # if so, attribute this record to the primary
		    # the hidden assumption is that primary header
		    # keywords appear in the user area before extension
		    # keywords, if not this code must be revised
		    
		    if (gf_findhash (both, Memc[record]) != IS_FOUND) {
			fd = ext

		    } else {
			call gf_trimhash (Memc[record], Memc[keyword], 
					  SZ_KEYWORD)

			if (gf_getmkhash (both, Memc[keyword]) == 0) {
			    call gf_setmkhash (both, Memc[keyword], 1)
			    fd = prim
			} else {
			    fd = ext
			}
		    }

		case CMT_FOUND:
		    fd = oldfd

		case HIST_FOUND: # Copy HISTORY to hist spool or primary
		    if (hist != -1) {
			fd = hist
		    } else {
			fd = prim
		    }
		}

		oldfd = fd
		if (fd != NULL)
		    call putline (fd, Memc[record])
	    }

	    if (! has_extend)
		call gf_putxtn (im, prim)

	    call gf_freehash (both)
	}

	# Since history records are lost during inheritance,
	# track them using a seperate spool. 
	if ((prim != NULL) && (hist != -1)) {
	   call seek(hist, BOF)
	   call fcopyo(hist, prim)
	}

	call strclose (in)
	call sfree (sp)
end

# GF_PUTXTN -- Add an extend record to the primary spool file

procedure gf_putxtn (im, prim)

pointer	im		# i: image descriptor
int	prim		# u: primary header spool file descriptor
#--
int	extend
pointer	newprim

string	format  "EXTEND  =                    %s / %s"
string	comment "File may contain standard extensions           \n"

int	open(), gf_find_db()

begin
	# Don't need to update if no primary file

	if (prim == NULL)
	    return

	# Open a new spool file so that EXTEND can be the first record

	newprim = open ("newprim", READ_WRITE, SPOOL_FILE)

	# Add the EXTEND record to the new spool file

	extend = gf_find_db (im, PARAM_EXTEND)

	call fprintf (newprim, format)
	if (extend == NO) {
	    call pargstr ("F")
	} else {
	    call pargstr ("T")
	}
	call pargstr (comment)

	# Copy the rest of the records from the primary spool

	call seek (prim, BOF)
	call fcopyo (prim, newprim)

	# Return the new spool file in place of the old

	call close (prim)
	prim = newprim
end
