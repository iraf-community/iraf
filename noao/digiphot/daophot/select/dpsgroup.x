include <tbset.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"

define	NCOLUMN	6

# DP_SGROUP -- Read in each group from the input file and write it to the
# output file if its size is between min_group and max_group.

procedure dp_sgroup (dao, tp_in, tp_out, text_file, min_group, max_group)

pointer	dao				# pointer to the daophot structure
pointer	tp_in				# the input file descriptor
pointer	tp_out				# the output file descriptor
bool	text_file			# text or table file
int 	min_group			# minimum sized group to extract
int	max_group			# maximum sized group to extract

int	nrow_in_table, output_row, in_record, ngroup, cur_group
pointer	sp, indices, fields, key, icolpoint, ocolpoint
int	tbpsta(), dp_ggroup()

begin
	# Allocate some working memory.
	call smark (sp)
	call salloc (icolpoint, NAPGROUP, TY_POINTER)
	call salloc (indices, NAPGROUP, TY_INT)
	call salloc (fields, SZ_LINE, TY_CHAR)
	call salloc (ocolpoint, NCOLUMN, TY_POINTER)

	# Allocate some memory for reading in the group.
	call dp_gnindices (Memi[indices])
	call dp_memapsel (dao, Memi[indices], NAPPAR, max_group + 1)

	# Initialize the output file.
	if (text_file) {
	    call dp_apheader (tp_in, tp_out)
	    call dp_xgselpars (tp_out, min_group, max_group)
	    call dp_apbanner (tp_in, tp_out)
	} else {
	    call tbtcre (tp_out)
	    call tbhcal (tp_in, tp_out)
	    call dp_tgselcol (tp_out, Memi[ocolpoint])
	    call dp_tgselpars (tp_out, min_group, max_group)
	}

	# Initialize the input file.
	if (text_file) {
	    call pt_kyinit (key)
	    call dp_gnstpsf (Memi[indices], Memc[fields], NAPGROUP)
	    nrow_in_table = 0
	} else {
	    key = NULL
	    call dp_tnsinit (tp_in, Memi[icolpoint])
	    nrow_in_table = tbpsta (tp_in, TBL_NROWS)
	}

	# Initialize the output record counter.
	output_row = 0

	# Initialize the input record counter.
	in_record = 1

	repeat {

	    # Read in the group.
	    ngroup = dp_ggroup (dao, tp_in, key, Memc[fields], Memi[indices],
		Memi[icolpoint], nrow_in_table, max_group, in_record,
		cur_group)
	    if (ngroup <= 0)
		break
	    if (ngroup < min_group || ngroup > max_group)
		next

	    # Print a message to the terminal.
	    if (DP_VERBOSE(dao) == YES) {
		call printf ("Selecting group: %6d  of %6d star(s)\n")
		    call pargi (cur_group)
		    call pargi (ngroup)
	    }

	    # Write the group to the output file.
	    if (text_file)
		call dp_xwrtselect (dao, tp_out, ngroup, cur_group)
	    else
		call dp_twrtselect (dao, tp_out, Memi[ocolpoint], ngroup,
		   cur_group, output_row) 
	}

	if (text_file)
	    call pt_kyfree (key)

	# Free memory.
	call sfree (sp)
end
