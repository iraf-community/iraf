include	"lexoper.h"

define	start_		90

# LEXER -- Lexically analyze a rule base
#
# B.Simon	25-Apr-88	Original

procedure lexer (rb, oper, value, maxch)

pointer	rb		# i: Pointer to  descriptor of rule base
int	oper		# o: Operator type found
char	value[ARB]	# o: Text of operator
int	maxch		# i: Maximum length of string
#--
char	dic_text[2]
int	junk, old_index, dic_index, dic_oper[5]
pointer	sp, ch, blanks

data	dic_oper	/SEPOPR, IMPOPR, OROPR, ANDOPR, EQOPR/
string	dict 		"/;/=>/||/&&/=/"

bool	streq()
int	getline(), ctowrd(), span(), nospan(), strdic()

begin
	# Allocate an array to hold whitespace

	call smark (sp)
	call salloc (blanks, SZ_LINE, TY_CHAR)

	# Skip over leading whitespace

start_	junk = span (" \t", RB_LINE(rb), RB_INDEX(rb), Memc[blanks], SZ_LINE)

	# Branch on first non-white character

	ch = RB_CHARPTR(rb)

	# End of line or beginning of comment

	if (Memc[ch] == '\n' || Memc[ch] == '#' || Memc[ch] == EOS) {
	    if (getline (RB_FILE(rb), RB_LINE(rb)) == EOF) {
		oper = ENDOPR
		value[1] = EOS
	    } else {
		RB_NLINE(rb) = RB_NLINE(rb) + 1		
		RB_INDEX(rb) = 1
		goto start_
	    }

	# Quoted identifier

	} else if (Memc[ch] == '\'' || Memc[ch] == '"') {
	    junk = ctowrd (RB_LINE(rb), RB_INDEX(rb), value, maxch)
	    oper = IDOPR

	# Unquoted identifier

	} else if (nospan ("=&|; \t\n", RB_LINE(rb), RB_INDEX(rb),
		   	   value, maxch) > 0			  ) {
	    oper = IDOPR

	# Other operator

	} else {
	    old_index = RB_INDEX(rb)
	    junk = span ("=>&|;", RB_LINE(rb), RB_INDEX(rb), value, 2)
	    dic_index = strdic (value, dic_text, 2, dict)
	    if (dic_index > 0 && streq (value, dic_text)) {
		oper = dic_oper[dic_index]
	    } else {
		RB_INDEX(rb) = old_index
		junk = ctowrd (RB_LINE(rb), RB_INDEX(rb), value, maxch)
		oper = IDOPR
	    }
	}

	call sfree (sp)
end

# LEXINIT -- Initialize the lexical analyzer

procedure lexinit (rbase, rb)

char	rbase[ARB]		# i: Name of rule base file
pointer	rb			# o: Pointer to rule base descriptor
#--

int	open()
errchk	calloc, open

begin
	call malloc (rb, RB_LENGTH, TY_INT)

	RB_FILE(rb) = open (rbase, READ_ONLY, TEXT_FILE)
	RB_INDEX(rb) = 1
	RB_NLINE(rb) = 0
	RB_LINE(rb) = EOS
end

#LEXCLOSE -- Close the lexical analyzer

procedure lexclose (rb)

pointer	rb			# i: Pointer to rule base descriptor
#--

errchk	close, mfree

begin
	call close (RB_FILE(rb))
	call mfree (rb, TY_INT)
end
