include "igi.h"

procedure ig_data (igs)

##  12 June 1992  Check for redirected STDOUT and write directly using
##                listout() instead of paging output.  ZGL
## 7/16/92  Change order of checking for text/binary table so we use
##          custom text table parsing rather than table code.  ZGL
## 3/18/93  Compare STDOUT_REDIR with YES.

pointer	igs		# igi parameters structure

pointer	tokvals		# Token value structure
int	token
int	igps
int	tdp

char	root[SZ_FNAME]

int	gettok(), access(), tbtopn()

errchk	tbtopn

begin
	tokvals = TOKEN_VALUE(igs)
	igps = PLOT_PARMS(igs)

	call lcmdcat (igs, YES)

	token = gettok (igs)

	if (IS_NEWCOMMAND(token)) {
	    # No command argument;  list the file
	    if (MG_DATASRC(igps) == NO_DATA)
		call eprintf ("No data file specified ")

	    else if (MG_DATASRC(igps) == TEXT_DATA) {
		if (STDOUT_REDIR(igs) == YES) {
		    # List to STDOUT
		    call listout (MG_DATASRC(igps))

		} else
		    # Page the input text data file
		    call gpagefile (GIO_GP(igs), MG_FILE_NAME(igps), EOS)

	    } else if (MG_DATASRC(igps) == TABLE_DATA) {
		call eprintf ("Table data file:  %s;  use DLIST to list ")
		    call pargstr (MG_FILE_NAME(igps))
	    }

	    return
	}

	call lcmdcat (igs, NO)
	call cmdcat  (igs, NO)
	call getroot (LOP_VALC(tokvals), root)

#	if (access (LOP_VALC(tokvals), 0, TEXT_FILE) == YES) {
	if (access (root, 0, TEXT_FILE) == YES) {
	    # File is text;  assume we have columns of numbers

	    if (DEBUG_OUTPUT(igs) == YES) {
		call eprintf ("File %s is a text file ")
		    call pargstr (LOP_VALC(tokvals))
	    }

	    MG_DATASRC(igps) = TEXT_DATA

	} else {
	    #  Try opening as an STSDAS (binary) table

	    iferr {
		# Open the file as a table to see if it really is one
#		tdp = tbtopn (LOP_VALC(tokvals), READ_ONLY, 0)
		tdp = tbtopn (root, READ_ONLY, 0)

	    } then {
		call eprintf ("Couldn't open input table: %s ")
		    call pargstr (LOP_VALC(tokvals))
		return
	    }

	    if (DEBUG_OUTPUT(igs) == YES) {
		call eprintf ("File %s is a table ")
		    call pargstr (LOP_VALC(tokvals))
	    }
 
	    MG_DATASRC(igps) = TABLE_DATA
	    call tbtclo (tdp)
	}

	call strcpy (LOP_VALC(tokvals), MG_FILE_NAME(igps), SZ_FNAME)
end


procedure getroot(file, root)

char	file[ARB]
char	root[ARB]

int	ic, nc
int	maxch

begin

	maxch = SZ_FNAME

	# Search for the first unescaped bracket
	# Copy all chars prior to bracket into root

	for (ic = 1; file[ic] != EOS; ic = ic + 1) {
	    if (file[ic] == '\\' && file[ic+1] != EOS) {
		ic = ic + 1
	    } else if (file[ic] == '['){
		break
	    }
	}

	nc = min (ic-1, maxch)
	call strcpy (file, root, nc)
end
