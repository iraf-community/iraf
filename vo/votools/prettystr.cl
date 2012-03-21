#{  PRETTYSTR -- Pretty-print a string.  A pretty abusive way to use the
#   CL string functions.

procedure prettystr (instr)

struct	instr				{ prompt = "Input string"	}

int	start_col = 18			{ prompt = "Starting column"	}
int	end_col   = 75			{ prompt = "Ending column"	}
bool	comment   = no			{ prompt = "Comment string?"	}

begin
	struct  ip, op, cp
	string  spaces
	int     scol, ecol, lnum
	int     sp, ep, oep, slen, ncols
 	bool	done, comm

	scol = start_col
	ecol = end_col + 2
	ip   = instr	
	comm = comment
	if (comm)
	    scol = scol + 1

	lnum   = 1
	done   = no
	spaces = "                                                      "
	op     = ""

	slen = strlen (ip)
	if (slen >= 511)
	    slen = 512
	ncols= (ecol - scol + 1)
	sp   = 1
	ep   = ncols
	oep  = 1


	# Handle the easy case first.  If it's a short string or there are
	# no spaces just print it.
	if (slen <= ep) {
	    print (ip)

	} else {
	    while (!done) {
		# Get char at ending guess.
		cp = substr (ip,ep,ep)
		if (cp != " " && cp != "\t" && cp != "\n" && cp != ",") {
		    # back up to the previous whitespace
		    while (ep > oep) {
		        cp = substr (ip,ep,ep)
			if (cp == " " || cp == "\t" || cp == "\n" || cp == ",")
			    break
			else
			    ep = ep - 1
		    }
		    if (ep <= oep) {
			# Long string, no commas, just dump it
	    		print (ip)
			return
		    }
		}

		# Whitespace - break here
		line = substr (ip, sp, ep)
		printf ("%s\n", line)
		if (comm) printf ("#")
		if (lnum >= 1)
		    printf ("%s", substr (spaces, 1, (scol-1)))

		# Update pointers
		sp = ep + 1
		if ((ep + ncols) > slen) {
		    # last chunk
		    ep = slen
		    line = substr (ip, sp, ep)
		    printf ("%s\n", line)

		    done = yes

		} else {
		    oep = ep
		    ep = ep + ncols
		}
		lnum = lnum + 1
	    }
	}
end
