include "../../lib/ptkeysdef.h"

# T_ISTABLE -- Decide whether an input file is an ST Table, an APPHOT style
# text file or neither.

procedure t_istable ()

pointer	infile			# name of the input file

bool	table, text, other
int	fd
pointer	sp, line
int	tbtopn(), open(), getline(), strmatch()
errchk	tbtopn(), open()

begin
	call smark (sp)
	call salloc (infile,  SZ_FNAME, TY_CHAR)
	call salloc (line, SZ_LINE, TY_CHAR)

	call clgstr ("infile", Memc[infile], SZ_FNAME)

	# Check first for an ST Table format.
	iferr {
	    fd = tbtopn (Memc[infile], READ_ONLY, 0)
	} then {
	    table = false
	    iferr {
	        fd = open (Memc[infile], READ_ONLY, TEXT_FILE) 
	    } then {
	        text = false
	        other = true
	    } else {
	        Memc[line] = EOS
	        if (getline (fd, Memc[line]) != EOF) {
		    if (strmatch (Memc[line], KY_CHAR_IRAF) != 0) {
		        text = true
		        other = false
		    } else {
		        text = false
		        other = true
		    }
		}
		call close (fd)
	    }
	} else {
	    table = true
	    text = false
	    other = false
	    call tbtclo (fd)
	}

	# Store the results in the parameter files.
	call clputb ("table", table)
	call clputb ("text", text)
	call clputb ("other", other)


	call sfree (sp)
end
