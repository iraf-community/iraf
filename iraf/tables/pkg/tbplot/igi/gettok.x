include <ctype.h>
include "igi.h"

#  8/20/91 Removed ^Ls. ZGL

#  GETTOK -- Get tokens from input, expanding macros

int procedure gettok (igs)

pointer	igs		# igi parameters structure

int	in		# Input stream 
pointer	symtabd		# Command symbol table descriptor
pointer	tokvals		# Token value structure
int	token
int	command

int	readtok(), gtokst()

begin
	in = INPUT_SOURCE(igs)
	symtabd = SYM_TABLE(igs)
	tokvals = TOKEN_VALUE(igs)

	repeat {
	    token = readtok (in, tokvals)
	    if (token == IDENTIFIER) {
		command = gtokst (symtabd, tokvals)
		if (command < 0)
		    # Macro
		    call expmac (igs)
		else
		    break
	    } else if (token == '$') {
		# Use cursor position for coordinates
		call rpcpos (igs)
	    } else
		break
	} until (token == EOF)

	return (token)
end


#  EXPMAC -- Expand a macro back into the input stream.  The macro text 
#  is stored in the string buffer (SBUF) in the symbol table, indexed by 
#  the macro name.  An offset buffer is allocated in STAB for pointers to 
#  the argument values (if any).  The argument values from the command line 
#  are stored in SBUF with the offsets in STAB.  When an argument (&n) is 
#  encountered while scanning the macro text, its value replaces the 
#  identifier.  The macro text string is pushed back into the input stream.

procedure expmac (igs)

pointer	igs		# igi parameters structure

int	in
pointer	symtabd		# Command symbol table descriptor
pointer	tokvals		# Token value structure
pointer	sym
pointer offset
pointer	ipm, ipa
int	token
int	argnum, arg
pointer	chin, cline
int	maxch
pointer	intp, charp
char	ch[1]

int	readtok(), ctoi(), stpstr()
pointer	stfind(), strefsbuf(), strefstab()

begin
	in = INPUT_SOURCE(igs)
	symtabd = SYM_TABLE(igs)
	tokvals = TOKEN_VALUE(igs)

	# Find the (lower case) macro name in the symbol table
	call strlwr (LOP_VALC(tokvals))
	sym = stfind (symtabd, LOP_VALC(tokvals))

	# Find the macro text in the symbol table string buffer (SBUF)
	ipm = strefsbuf (symtabd, SYM_STROFF(sym))

	call lcmdcat (igs, YES)

	maxch = SZ_LINE
	call malloc (cline, maxch, TY_CHAR)
	Memc[cline] = EOS

	if (SYM_NMACARG(sym) == 0) {
	    # No macro arguments;  However, there may be escaped &'s
	    # to unescape.
	    for (chin = ipm;  Memc[chin] != EOS;  chin = chin + 1) {
		if (Memc[chin] == '\\' && Memc[chin+1] == '&')
		    chin = chin+1
		call strcpy (Memc[chin], ch, 1)
		call tkapst (ch, cline, maxch)
	    }
	    ipa = cline
	    
	} else {
	    # Arguments
	    # Find the STAB buffer for argument value string offsets
	    intp = strefstab (symtabd, SYM_MARGOFF(sym))

	    arg = 0
	    while (arg < SYM_NMACARG(sym)) {
		# Replace macro argument
		token = readtok (in, tokvals)
		if (token == IDENTIFIER || token == CONSTANT) {
		    # Store the argument value string in the symbol table
		    arg = arg + 1
		    Memi[intp+arg-1] = stpstr (symtabd, LOP_VALC(tokvals), 0)
		    call lcmdcat (igs, NO)

		} else if (!IS_DELIM(token)) {
		    call eprintf ("Invalid macro argument:  %c ")
			call pargc (token)
		    call mfree (cline, TY_CHAR)
		    return
		}
	    }

	    for (chin = ipm;  Memc[chin] != EOS;  chin = chin + 1) {
		if (Memc[chin] == '\\' && Memc[chin+1] =='&')
		    chin = chin + 1
		if (Memc[chin] == '&' && Memc[max(ipm,chin-1)] != '\\') {
		    # Replace macro argument from symbol table
		    # Skip over the '&'
		    chin = chin + 1
		    if (ctoi (Memc, chin, argnum) > 0) {
			# Offset to the appropriate argument
			offset = Memi[intp+argnum-1]
			charp = strefsbuf (symtabd, offset)
			# Stuff the argument value string into the command line
			call tkapst (Memc[charp], cline, maxch)
			# Add a token delimiter
			#call tkapst (" ", cline, maxch)
			chin = chin - 1
		    } else {
			call eprintf ("Invalid macro argument:  %s ")
			    call pargstr (Memc[chin])
			call mfree (cline, TY_CHAR)
			return
		    }

		} else if (Memc[chin] != EOS) {
		    call strcpy (Memc[chin], ch, 1)
		    call tkapst (ch, cline, maxch)
		}
	    }
	    ipa = cline
	}

	# Mark text for mode change
	call ungetline (in, ";mode;")
	# Push the expanded macro text back into the input stream
	call ungetline (in, Memc[ipa])

	call cmdcat (igs, NO)

	call spshfd (CMD_STATE(igs), STATE_STACK(igs))
	CMD_STATE(igs) = EXPAND_MODE

	call mfree (cline, TY_CHAR)
end


procedure rpcpos (igs)

pointer	igs		# igi parameters structure

int	in
pointer	sp, line, coords
real	wx, wy
int	wcs, key

int	clgcur()

begin
	in = INPUT_SOURCE(igs)

	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (coords, SZ_LINE, TY_CHAR)

	if (clgcur ("cursor", wx, wy, wcs, key, Memc[line], SZ_LINE) != EOF && 
	    key != 'q') {
	    call sprintf (Memc[coords], SZ_LINE, "%g %g")
		call pargr (wx)
		call pargr (wy)
	    call ungetline (in, Memc[coords])
	}

	call sfree (sp)
end
