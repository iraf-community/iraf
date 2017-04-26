#-h-  gettok			 2076  local   12/01/80  15:54:09
# gettok - get token. handles file inclusion and line numbers
   include  defs

character function gettok (token, toksiz)

character token (MAXTOK)
integer toksiz
include COMMON_BLOCKS
integer equal
character t, deftok
#character name(MAXNAME), t
#integer i, len, open, length

string ssubr "x$subr"
string sfunc "x$func"
#string incl "include"

#  for (;  level > 0;  level = level - 1) {

	gettok = deftok (token, toksiz)
	if (gettok != EOF) {
	    if (gettok == XPP_DIRECTIVE) {
	        if (equal (token, sfunc) == YES) {
		    call skpblk
		    t = deftok (fcname, MAXNAME)
		    call pbstr (fcname)
		    if (t != ALPHA)
		        call synerr ("Missing function name.")
		    call putbak (BLANK)
		    swvnum = 0
		    swvlev = 0
		    return
	        } else if (equal (token, ssubr) == YES) {
		    swvnum = 0
		    swvlev = 0
		    return
	        } else
		    return
	     }
	     return
	}

	token (1) = EOF
	token (2) = EOS
	gettok = EOF
	return
end


# -- Includes are now processed elsewhere

#	 else if (equal (token, incl) == NO)
#	    return
#
#	 # process 'include' statements:
#	 call skpblk
#	 t = deftok (name, MAXNAME)
#	 if (t == SQUOTE | t == DQUOTE) {
#	    len = length (name) - 1
#	    for (i = 1; i < len; i = i + 1)
#	       name (i) = name (i + 1)
#	    name (i) = EOS
#	    }
#	 i = length (name) + 1
#	 if (level >= NFILES)
#	    call synerr ("includes nested too deeply.")
#	 else {
#	    infile (level + 1) = open (name, READ)
#	    linect (level + 1) = 0
#	    if (infile (level + 1) == ERR)
#	       call synerr ("can't open include.")
#	    else {
#	       level = level + 1
#	       if (fnamp + i <= MAXFNAMES) {
#		  call scopy (name, 1, fnames, fnamp)
#		  fnamp = fnamp + i    # push file name stack
#		  }
#	       }
#	    }
#	 }
#      if (level > 1) {	    # close include file pop file name stack
#	 call close (infile (level))
#	 for (fnamp = fnamp - 1; fnamp > 1; fnamp = fnamp - 1)
#	    if (fnames (fnamp - 1) == EOS)
#	       break
#	 }

#    }

