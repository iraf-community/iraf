# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pkg/xtanswer.h>

# XT_ANSWER -- Issue an STDOUT prompt and get a STDIN answer with values
# YES, NO, ALWAYSYES, or ALWAYSNO.

procedure xt_answer (prompt, answer)

char	prompt[ARB]		# Prompt to be issued
int	answer			# Answer

int	nwrd
char	word[SZ_LINE]

int	getline(), strdic(), strlen()

begin
	if ((answer == NO) || (answer == YES)) {
	    if (answer == NO) {
	        call printf ("%s (no): ")
		    call pargstr (prompt)
	    } else {
	        call printf ("%s (yes): ")
		    call pargstr (prompt)
	    }
	    call flush (STDOUT)

	    if (getline (STDIN, word) != EOF) {
		word[strlen(word)] = EOS
	        nwrd = strdic (word, word, 4, XT_ANSWERS)
		switch (nwrd) {
		case 1:
		    answer = NO
		case 2:
		    answer = YES
		case 3:
		    answer = ALWAYSNO
		case 4:
		    answer = ALWAYSYES
		}
	    }
	}
end


# XT_CLANSWER -- Issue a CLGWRD request and get an answer with values
# YES, NO, ALWAYSYES, or ALWAYSNO.

procedure xt_clanswer (parameter, answer)

char	parameter[ARB]		# CL parameter
int	answer			# Answer

pointer	sp, str

int	clgwrd()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	switch (clgwrd (parameter, Memc[str], SZ_LINE, "|no|yes|NO|YES|")) {
	case 1:
	    answer = NO
	case 2:
	    answer = YES
	case 3:
	    answer = ALWAYSNO
	case 4:
	    answer = ALWAYSYES
	default:
	    answer = YES
	}

	call sfree (sp)
end
