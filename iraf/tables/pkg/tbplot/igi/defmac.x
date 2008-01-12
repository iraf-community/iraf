include <ctype.h>
include "igi.h"
include "commands.h"

#  DEFMAC -- Define a macro.  Read tokens from input to build the macro 
#  text.  Arguments are defined by &n, where n is any integer.  The macro 
#  name is entered into the symbol table and the text is stored as a single 
#  string in the symbol table string buffer (SBUF).  Commands in the macro 
#  text are delimited by ";".  A STAB buffer is allocated in the symbol
#  table for pointers to the macro argument (if any) value strings.  The
#  largest value of n fixes the number of macro arguments. 

procedure defmac (macro, igs)

char	macro[ARB]	# Macro name
pointer	igs		# igi parameters structure

pointer	symtabd		# Command symbol table descriptor
pointer	tokvals		# Token value structure
int	in		# Input stream
int	token		# Token code
pointer	sym		# Symbol table entry 
int	cmd		# Command index
int	ip
pointer	cline
int	maxch
int	lastchar

pointer	stenter(), stalloc()
int	readtok(), gtokst(), stnsymbols(), stpstr(), strlen(), spopfd()

common	/symcom/sym

begin
	tokvals = TOKEN_VALUE(igs)
	symtabd = SYM_TABLE(igs)
	in = INPUT_SOURCE(igs)

	# Enter the (lower case) macro name into the symbol table
	call strlwr (macro)
	sym = stenter (symtabd, macro, LEN_SYMSTRUCT)
	SYM_VALUE(sym) = stnsymbols (symtabd, 0)
	SYM_NMACARG(sym) = 0

	# Allocate space for the macro text string
	maxch = SZ_LINE
	call malloc (cline, maxch, TY_CHAR)
	Memc[cline] = EOS

	if (CMD_STATE(igs) == CURSOR_MODE)
	    # Kludge to force a prompt in cursor mode
	    call ungetc (in, '\n')

	# Change the process state to macro define mode
	call spshfd (CMD_STATE(igs), STATE_STACK(igs))
	CMD_STATE(igs) = DEFINE_MODE

	cmd = 0	

	# Use normal text scrolling ?!!!!!!
	call gdeactivate (GIO_GP(igs), 0)
	call igprompt ("macro> ", igs)

	repeat {
	    token = readtok (in, tokvals)
	    switch (token) {
	    case IDENTIFIER:
		cmd = gtokst (symtabd, tokvals)
		if (cmd != END && cmd != NOOP) {
		    call tkapst (LOP_VALC(tokvals), cline, maxch)
		    call tkapst (" ", cline, maxch)
		}

	    case ARGUMENT:
		SYM_NMACARG(sym) = max (SYM_NMACARG(sym), LOP_ARGN(tokvals))
		call tkapst (LOP_VALC(tokvals), cline, maxch)
		call tkapst (" ", cline, maxch)

	    case CONSTANT:
		call tkapst (LOP_VALC(tokvals), cline, maxch)
		call tkapst (" ", cline, maxch)

	    case STRING:
		call tkapst ("\"", cline, maxch)
		call tkapst (LOP_VALC(tokvals), cline, maxch)
		call tkapst ("\" ", cline, maxch)

	    case '\n':
		lastchar = cline + strlen (Memc[cline]) - 1
		if (Memc[cline] != EOS && Memc[lastchar] != '\n')
		    # No blank lines, please
		    call tkapst ("\n", cline, maxch)
		call igprompt ("macro> ", igs)

	    case ';':
		# Command separator
		call tkapst ("\n", cline, maxch)

	    case ',':
		# Argument separator
		call tkapst (" ", cline, maxch)
	    }

	} until (token == EOF || cmd == END)

	# Strip off trailing delimiters
	for (ip = cline + strlen (Memc[cline]) - 1;
	     ip >= cline && (IS_DELIM(Memc[ip]) || Memc[ip] == EOS);
	     ip = ip - 1)
	    Memc[ip] = EOS

	# Strip off leading delimiters
	for (ip = cline;
	     IS_DELIM(Memc[ip]) && Memc[ip] != EOS;
	     ip = ip + 1)
	    ;

	# Stuff the macro text into the symbol table
	SYM_STROFF(sym) = stpstr (symtabd, Memc[ip], 0)
	if (SYM_NMACARG(sym) > 0)
	    # Allocate space for pointers to argument value strings
	    SYM_MARGOFF(sym) = stalloc (symtabd, SYM_NMACARG(sym))

	# Restore the process state
	CMD_STATE(igs) = spopfd (STATE_STACK(igs))

	if (CMD_STATE(igs) == CURSOR_MODE)
	    # Kludge to get back to cursor mode
	    call ungetline (in, ";noop;")

	# Go back to command line scrolling
	call greactivate (GIO_GP(igs), 0)

	call mfree (cline, TY_CHAR)
end
