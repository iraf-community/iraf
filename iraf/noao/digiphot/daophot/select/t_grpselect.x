include	<fset.h>
include "../lib/daophotdef.h"

# T_GRPSELECT  -- Select groups from a GROUP file on the basis of the size
# of the group. Only groups of the sizes specified are copied into the
# output table.

procedure t_grpselect ()

pointer	ingroup				# the input GROUP file
pointer	outgroup			# the output GROUP file
int	min_group			# the minimum group size
int	max_group			# the maximum group size

size_t	sz_val
bool	gr_text
int	lilist, lolist, verbose, i_val
pointer ilist, olist, sp, tp_in, tp_out, dao

bool	clgetb(), itob()
int	open(), clgeti(), fstati(), btoi(), access(), fntlenb(), fntgfnb()
pointer	tbtopn(), fntopnb()

include	<nullptr.inc>

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get some memory.
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (ingroup, sz_val, TY_CHAR)
	call salloc (outgroup, sz_val, TY_CHAR)

	# Get the various task parameters.
	call clgstr ("ingroupfile", Memc[ingroup], SZ_FNAME)
	call clgstr ("outgroupfile", Memc[outgroup], SZ_FNAME)
	min_group = clgeti ("min_group")
	max_group = clgeti ("max_group")
	verbose = btoi (clgetb ("verbose"))

	# Open the daophot structure.
	call dp_init (dao)

	# Open the photometry structure.
	call dp_apsetup (dao)

	# Set some parameters.
	call dp_seti (dao, VERBOSE, verbose)

	# Get the lists.
	ilist = fntopnb (Memc[ingroup], NO)
	lilist = fntlenb (ilist)
	olist = fntopnb (Memc[outgroup], NO)
	lolist = fntlenb (olist)

	# Check the list lengths.
	if (lilist != lolist) {
	    call fntclsb (ilist)
	    call fntclsb (olist)
	    call sfree (sp)
	    call error (0,
	        "The input and output list lengths are not compatible")
	}

	# Loop over the files.
	while ((fntgfnb (ilist, Memc[ingroup], SZ_FNAME) != EOF) &&
	    (fntgfnb (olist, Memc[outgroup], SZ_FNAME) != EOF)) {

	    # Open the input file.
	    gr_text = itob (access (Memc[ingroup], 0, TEXT_FILE))
	    if (gr_text)
	        tp_in = open (Memc[ingroup], READ_ONLY, TEXT_FILE)
	    else
	        tp_in = tbtopn (Memc[ingroup], READ_ONLY, NULLPTR)

	    # Open an output file of the same type as the input file.
	    if (gr_text)
	        tp_out = open (Memc[outgroup], NEW_FILE, TEXT_FILE)
	    else
	        tp_out = tbtopn (Memc[outgroup], NEW_COPY, tp_in)

	    # Read in the groups and select by group size.
	    call dp_sgroup (dao, tp_in, tp_out, gr_text, min_group, max_group)

	    # Close the input and output files.
	    if (gr_text) {
		i_val = tp_in
	        call close (i_val)
		i_val = tp_out
	        call close (i_val)
	    } else {
	        call tbtclo (tp_in)
	        call tbtclo (tp_out)
	    }
	}

	# Close the lists.
	call fntclsb (ilist)
	call fntclsb (olist)

	# Free the photometry structure.
	call dp_apclose (dao)

	# Free the daophot structure.
	call dp_free (dao)

	call sfree (sp)
end	
