include "tcheck.h"

# CMDSPLIT -- Split a command into keyword and expression strings

procedure cmdsplit (command, keystart, cmdstart)

char	command[ARB]	# io: Command line
int	keystart	#  o: Start of keyword substring
int	cmdstart	#  o: Start of command substring
#--
char	comment
int	ic, jc
pointer	sp, keyword

data	comment  / '#' /

string	noexpress  "No expression following when"

bool	streq()
int	stridx(), word_fetch()

begin
	call smark (sp)
	call salloc (keyword, SZ_FNAME, TY_CHAR)

	# Strip comments from command line

	ic = stridx (comment, command)
	if (ic > 0)
	    command[ic] = EOS

	# Set output variables to default values

	keystart = 1
	cmdstart = 0

	# Find location of "when" in command and split the line there

	ic = 1
	jc = 0
	while (word_fetch (command, ic, Memc[keyword], SZ_FNAME) > 0) {
	    if (jc > 0 && streq (Memc[keyword], "when")) {
		command[jc] = EOS
		cmdstart = ic
		break
	    }
	    jc = ic
	}

	# Exit with error if no expression was found

	if (cmdstart == 0 && jc > 0)
	    call error (SYNTAX, noexpress)

	call sfree (sp)
end

