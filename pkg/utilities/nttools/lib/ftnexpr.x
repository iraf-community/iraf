include <ctype.h>
define	DOT		'.'
define	SQUOTE		'\''
define	DQUOTE		'"'
define  BSLASH          '\\'

#* HISTORY *
#* B.Simon	04-Jan-93	Original
#* B.Simon	01-Dec-93	No longer removes backslashes


# FTNEXPR -- Convert a Fortran boolean expression to SPP

procedure ftnexpr (oldexpr, newexpr, maxch)

char	oldexpr[ARB]	# i: Fortran expression
char	newexpr[ARB]	# o: SPP expression
int	maxch		# i: Maximum length of SPP expression
#--
char	ch, term
int	ic, jc, kc, iw
pointer	sp, dotbuf

string	ftnlist  ".eq. .and. .or. .gt. .ge. .lt. .le. .not. .ne."
string	spplist  " ==  &&    ||   >    >=   <    <=   !     !="

int	gstrcpy(), word_match(), word_find()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (dotbuf, SZ_LINE, TY_CHAR)

	# Loop over each character in the old expression
	# Characters between quote marks or dots are treated specially
	# To indicate this, term is set to the leading character

	ic = 1
	jc = 1
	kc = 0
	term = EOS

	while (oldexpr[ic] != EOS) {
	    ch = oldexpr[ic]

	    if (ch != term) {
		if (term == EOS) {
		    if (ch == DOT) {
			kc = 1
			term = ch
			Memc[dotbuf] = ch
		    } else {
			if (ch == SQUOTE || ch == DQUOTE)
			    term = ch

			newexpr[jc] = ch
			jc = jc + 1
		    }

		} else if (term == DOT) {
		    if (IS_ALPHA(ch)) {
			if (kc < SZ_LINE) {
			    Memc[dotbuf+kc] = ch
			    kc = kc + 1
			}
		    } else {
			Memc[dotbuf+kc] = ch
			Memc[dotbuf+kc+1] = EOS
			jc = jc + gstrcpy (Memc[dotbuf], newexpr[jc], 
					   maxch-jc+1)

			kc = 0
			term = EOS
		    }

		} else {
		    newexpr[jc] = ch
		    jc = jc + 1

                    if (ch == BSLASH) {
                        ic = ic + 1
			newexpr[jc] = oldexpr[ic]
			jc = jc + 1
                    }
		}

	    } else {
		term = EOS

		if (ch != DOT) {
		    newexpr[jc] = ch
		    jc = jc + 1

		} else {
		    Memc[dotbuf+kc] = ch
		    Memc[dotbuf+kc+1] = EOS
		    call strlwr (Memc[dotbuf])

		    iw = word_match (Memc[dotbuf], ftnlist)
		    if (iw == 0) {
			jc = jc + gstrcpy (Memc[dotbuf], newexpr[jc], 
					   maxch-jc+1)
		    } else {
			jc = jc + word_find (iw, spplist, newexpr[jc], 
					     maxch-jc+1)
		    }

		    kc = 0
		}
	    }

	    ic = ic + 1
	}

	# If there is anything left in the dot buffer copy it unchanged
	# to the output string

	newexpr[jc] = EOS

	if (kc > 0) {
	    Memc[dotbuf+kc] = EOS
	    call strcat (Memc[dotbuf], newexpr, maxch)
	}

	call sfree (sp)
end
