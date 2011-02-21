# tnam_cls -- close input & output fnt
# Close the file name templates.
#
# Phil Hodge, 15-Apr-1988  Subroutine created.
# Phil Hodge,  4-Oct-1995  Modify to use tbn instead of fnt.
# Phil Hodge, 25-Apr-2000  Add xin_t to calling sequence, and remove dir_only.

procedure tnam_cls (in_t, xin_t, out_t)

pointer in_t		# io: fnt pointer for input tables
pointer xin_t		# io: fnt pointer for tables of output indep var
pointer out_t		# io: fnt pointer for output tables
#--

begin
	if (in_t != NULL)
	    call tbnclose (in_t)

	if (xin_t != NULL)
	    call tbnclose (xin_t)

	if (out_t != NULL)
	    call tbnclose (out_t)
end
