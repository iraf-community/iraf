include "igi.h"

#  GTOKST -- Find the input igi command in the dictionary or the macro 
#  symbol table.  Returns the dictionary index number of the command or
#  the (negative) symbol table index value of the  macro. 

#  10/8/91  Added missing sfree().  ZGL

int procedure gtokst (symtabd, tokvals)

pointer	symtabd		# Command symbol table descriptor
pointer	tokvals		# Token value structure

int	cmdlen
pointer	sp, cmdname
int	cmdndx
pointer	sym
char	cmd[LEN_COMMAND]

include	"cmdict.h"

pointer	stfind()
int	strlen(), strdic()

begin
	call smark (sp)
	cmdlen = strlen (LOP_VALC(tokvals))
	call salloc (cmdname, LEN_COMMAND, TY_CHAR)
	call strcpy (LOP_VALC(tokvals), Memc[cmdname], LEN_COMMAND)
	call strcpy (LOP_VALC(tokvals), cmd, LEN_COMMAND)
	call strupr (Memc[cmdname])

	# call eprintf ("%d[%s]")
	#     call pargi (cmdlen)
	#     call pargstr (Memc[cmdname])

	cmdndx = 0
	# Look up the command in the dictionary
	cmdndx = strdic (Memc[cmdname], Memc[cmdname], LEN_COMMAND, cmdict)

	# call eprintf ("%s%d")
	#     call pargstr (Memc[cmdname])
	#     call pargi (cmdndx)

	if (cmdndx > 0) {
	    # Command found in dictionary;  
	    # replace the token value by the full command name
	    call realloc (LOP_VALP(tokvals), LEN_COMMAND, TY_CHAR)
	    call strcpy (Memc[cmdname], LOP_VALC(tokvals), LEN_COMMAND)

	} else {
	    # Command not in dictionary
	    # Check (lower case) name in macro symbol table
	    call strlwr (Memc[cmdname])
	    sym = stfind (symtabd, Memc[cmdname])
	    if (sym != NULL)
		# Command found in macro symbol table
		cmdndx = -SYM_VALUE(sym)
	}

	call sfree (sp)
	return (cmdndx)
end
