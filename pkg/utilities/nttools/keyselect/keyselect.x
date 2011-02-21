# KEYSELECT -- Copy selected image header keywords to sdas table

#* HISTORY *
#* B.Simon	12-Mar-1992	Original
#  Phil Hodge	 8-Apr-1999	Call tbfpri.

procedure keyselect ()

#--
include "keyselect.com"

pointer	input		# list of image names
pointer	output		# sdas table name
pointer	cols		# list of keyword and table column names
pointer	expr		# boolean expression used to select images
pointer cdfile		# column description file

bool	first
int	ngroup
int	phu_copied	# set by tbfpri and ignored
pointer	sp, keywords, columns, cluster, image
pointer	imlist, grplist, colptr, im, tp

string	noread  "No images read. Output table not created."

bool	tp_fetch(), eval_expr()
int	imtgetim()
pointer	imtopen(), immap(), tp_open(), op_table()

begin
	# Allocate dynamic memory for strings

	call smark(sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (cols, SZ_COMMAND, TY_CHAR)
	call salloc (expr, SZ_COMMAND, TY_CHAR)
	call salloc (cdfile, SZ_FNAME, TY_CHAR)

	call salloc (keywords, SZ_COMMAND, TY_CHAR)
	call salloc (columns, SZ_COMMAND, TY_CHAR)
	call salloc (cluster, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)

	# Read task parameters

	call clgstr ("input", Memc[input], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("cols", Memc[cols], SZ_FNAME)
	call clgstr ("expr", Memc[expr], SZ_FNAME)
	call clgstr ("cdfile", Memc[cdfile], SZ_FNAME)

	# If keyword list or expression is contained in a file, read the file

	if (Memc[cols] == '@')
	    call rd_list (Memc[cols+1], Memc[cols], SZ_COMMAND)
	call fmt_list (Memc[cols])

	if (Memc[expr] == '@')
	    call rd_list (Memc[expr+1], Memc[expr], SZ_COMMAND)
	call fmt_expr (Memc[expr])

	# Separate out the header keyword and table column names

	call sep_list (Memc[cols], Memc[keywords], Memc[columns], SZ_COMMAND)

	# Loop over all images and all groups in image

	first = true
	imlist = imtopen (Memc[input])

	while (imtgetim (imlist, Memc[cluster], SZ_FNAME) != EOF) {

	    # Hasgroup is set to true to get us through the loop the
	    # first time. It then is set to false, but can be set to
	    # true if either eval_expr() or cpy_table() accesses a
	    # group parameter.

	    hasgroup = true
	    grplist = tp_open (Memc[cluster], 0, ngroup)

	    while (hasgroup && tp_fetch (grplist, Memc[image])) {
		im = immap (Memc[image], READ_ONLY, 0)
		hasgroup = false

		# Open output table first time through loop

		if (first) {
		    first = false
		    call tbfpri (Memc[cluster], Memc[output], phu_copied)
		    tp = op_table (im, Memc[output], Memc[keywords], 
				   Memc[columns], Memc[cdfile])
		    call rd_table (Memc[columns], tp, colptr)
		}

		# Copy keywords from header to table if expression is true

		if (Memc[expr] == EOS) {
		    call cpy_table (im, tp, colptr, Memc[keywords])
		} else if (eval_expr (im, Memc[expr])) {
		    call cpy_table (im, tp, colptr, Memc[keywords])
		}

		call imunmap (im)
	    }
	    call tp_close (grplist)
	}

	# Close files and release memory

	call imtclose (imlist)
	call sfree(sp)

	if (first) {
	    call eprintf (noread)

	} else {
	    call mfree (colptr, TY_POINTER)
	    call tbtclo (tp)
	}

end
