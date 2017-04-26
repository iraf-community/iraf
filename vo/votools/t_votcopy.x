#
# VOTCOPY -- Copy a VOTable from one format to another.


procedure t_votcopy ()

char	in[SZ_LINE], out[SZ_LINE], format[SZ_LINE]
int	inlist, outlist
bool	header, verbose

int	clpopni(), clplen(), clgfil(), vot_convert()
bool	clgetb()

begin
	# Get the task parameters.
	inlist = clpopni ("input")
	outlist = clpopni ("output")

	call clgstr ("format", format, SZ_LINE)
	header  = clgetb ("header")
	verbose = clgetb ("verbose")

	if (clplen (inlist) != clplen (outlist)) {
	    call eprintf ("Number of input and output files not the same.\n")
	    call clpcls (inlist)
	    call clpcls (outlist)
	    return
	}

	# Loop over the files,
	while (clgfil (inlist, in, SZ_LINE) != EOF &&
	      (clgfil (outlist, out, SZ_LINE) != EOF)) {

	    if (vot_convert (in, out, format) == ERR)
		break
	}

	call clpcls (inlist)
	call clpcls (outlist)

	# Don't save the calling parameters.
	call clpstr ("input", "")
	call clpstr ("output", "")
	call clpstr ("format", "")
end
