# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	"lroff.h"

define  DIRECTIVE       1			# processing codes
define  NAME            2
define  TEXT            3

define	F_ROMAN		1			# font changes
define	F_ITALIC	2
define	F_BOLD		3
define	F_PREVIOUS	4
define	F_TELETYPE	5			# HTML-specific font

define	SPTR		Memi[$1+$2]
define	SECTION		Memc[SPTR($1,$2)]
define	MAX_SECTIONS	256


# LROFF2HTML -- Convert LROFF text to HTML.  By default we process the
# entire file however we allow for the printing of only a particular section
# or labelled text block to be compatible with the HELP task options.
# If a section name is given that section will be printed and and .ls
# block request will be ignored.

procedure lroff2html (in, out, module, parstr, center, ls_block, section)

int	in					#I input file descriptor
int	out					#I output file descriptor
char	module[ARB]				#I .help module name
char	parstr[ARB]				#I .help optional keyword 2
char	center[ARB]				#I .help optional keyword 3
char	ls_block[ARB]				#I .ls block to search for
char	section[ARB]				#I section to print

pointer sp, ip, sptr
pointer ibuf, unesc, name, level
int	lastline, font, ls_level
int	i, arg, nsec, cmd
bool	quit_at_le, quit_at_ih, formatted, in_para, in_pre

int	lh_findsection(), lh_findblock(), nextcmd()
int	stridxs(), getline(), strlen(), strmatch(), lgetarg()

define	text_ 	99
define	err_ 	98

include	"lroff.com"

begin
	call smark (sp)
	call salloc (ibuf, SZ_IBUF, TY_CHAR)
	call salloc (unesc, SZ_IBUF, TY_CHAR)
	call salloc (name, SZ_LINE, TY_CHAR)
	call salloc (level, SZ_FNAME, TY_CHAR)
	call salloc (sptr, MAX_SECTIONS, TY_POINTER)

	call aclrc (Memc[ibuf], SZ_IBUF)
	call aclrc (Memc[name], SZ_LINE)
	call aclrc (Memc[unesc], SZ_IBUF)
	call aclrc (Memc[level], SZ_FNAME)

	# Initialize.
	lastline  = TEXT
	font 	  = F_ROMAN
	in_para  = false
	nsec 	  = 0
	ls_level  = 0
        in_pre    = false
	quit_at_le = false
	quit_at_ih = false
        formatted  = false

	# Initialize the section numbering.
	call amovki (0, nh_level, MAX_NHLEVEL)

        # Determine whether or not the text is formatted.
        repeat {
            if (getline (in, Memc[ibuf]) == EOF)
                goto err_
            for (ip=1;  IS_WHITE(Memc[ibuf+ip-1]);  ip=ip+1)
               ;
        } until (Memc[ibuf+ip-1] != '\n')
        call ungetline (in, Memc[ibuf])
        if (Memc[ibuf] == '.')
           formatted = true

	# Scan forward if searching for and item.
	if (section[1] != EOS) {
	    if (lh_findsection (in, formatted, section) == EOF)
		goto err_
	} else if (ls_block[1] != EOS) {
	    if (lh_findblock (in, formatted, ls_block) == EOF)
		goto err_
	    quit_at_le = true
	}

	# Begin the output.
	call lh_prolog (out, module, parstr, center)

	# Process the file.
	while (getline (in, Memc[ibuf]) != EOF) {

	    # Make a copy of the raw line minus the newline char, we may need
	    # this to extract comments later.
	    Memc[ibuf+strlen(Memc[ibuf])-1] = EOS
	    call strcpy (Memc[ibuf], Memc[unesc], SZ_LINE)


	    # Escape problem chars for HTML and handle font changes. Changes
	    # are done in-place.
	    call lh_escape (Memc[ibuf], font, !in_pre, NO, SZ_LINE)

	    switch (Memc[ibuf]) {
	    case '\n':
		if (in_para == true) {
		    call fprintf (out, "</p>\n")
		    in_para = false
		}
		if (in_pre == true) {
		    call fprintf (out, "\n")
		}
	    case '.':
		# Swallow any help strings if present.
		if (strmatch (Memc[ibuf], "^.help") > 0)
		    next

		ip = 1
		lastline = TEXT

		# Process the directive, position the ip at the beginning
		# of any argument.
		cmd = nextcmd (Memc[ibuf], ip)
                while (IS_WHITE(Memc[ibuf+ip]))		# skip spaces
                    ip = ip + 1

		switch (cmd) {
		case FI:		# leave nofill mode; enter normal mode
		    if (in_pre == true) {
			call fprintf (out, "</pre>\n")
			in_pre = false
		    }
		case NF:		# leave fill mode (nofill)
		    if (in_para == true) {
			call fprintf(out, "</p>\n")
			in_para = false
		    }
		    if (in_pre == false) {
			call fprintf (out, "<pre>\n")
			in_pre = true
		    }
		case JU:		# enter line justification mode
		    # no-op
		    next
		case NJ:		# leave line justification mode
		    # no-op
		    next
		case RJ:		# right justify text on nf,nj line
		    # no-op
		    next

		case SH:		# section heading
		    if (in_para == true) {
			call fprintf(out, "</p>\n")
			in_para = false
		    }
		    lastline = DIRECTIVE
       	    	    Memc[level] = EOS
		    next
		case IH:		# indented section heading
		    if (in_para == true) {
		        call fprintf(out, "</p>\n")
			in_para = false
		    }
		    lastline = DIRECTIVE
       	    	    Memc[level] = EOS
		    if (quit_at_ih)
			if (stridxs ("|", section) > 0) {
			    quit_at_ih = false
			    call ungetline (in, Memc[ibuf])
			    if (lh_findsection (in, formatted, section) == EOF)
		    	        break
			} else
		    	    break
		    next
		case NH:		# numbered section heading
		    call lh_set_level (lgetarg(Memc[ibuf],ip,1), Memc[level])
		    lastline = DIRECTIVE
		    next

		case CE:		# center next line
		    if (getline (in, Memc[ibuf]) == EOF)
			break
		    else {
		        if (in_para == true) {
		            call fprintf(out, "</p>\n")
			    in_para = false
		        }
	    	        call lh_escape (Memc[ibuf], font, true, NO, SZ_LINE)
		        call fprintf (out, "<p style=\"text-align:center\">%s</p>\n")
		            call pargstr (Memc[ibuf])
		    }

		case BR:		# break line
		    call fprintf (out, "<br>\n")
		case SP:		# break, space N spaces on output
		    arg = lgetarg (Memc[ibuf], ip, 1)
		    call fprintf (out, "<br>\n")
		    for (i=1; i < arg; i = i + 1)
		        call fprintf (out, "<br>\n")
		case IN:		# indent +/- N spaces
		    # no-op
		    next

		case LS:		# begin labelled section
		    arg = lgetarg (Memc[ibuf], ip, 0)
		    if (arg == 0) 
			ip = 5

		    # Generate a HREF of the label, we use only the first word.
		    call strcpy (Memc[ibuf+ip-1], Memc[name], SZ_LINE)
		    for (i=0; IS_ALNUM(Memc[name+i]) || 
				Memc[name+i] == '_'; i=i+1)
			    	    ;
		    Memc[name+i] = EOS
	    	    Memc[ibuf+ip+strlen(Memc[ibuf+ip])-1] = EOS

		    if (in_para == true) {
		        call fprintf(out, "</p>\n")
			in_para = false
		    }
		    if (Memc[name] == EOS || ls_level > 0) {
		        call fprintf (out, "<dl>\n<dt><b>%s")
		            call pargstr (Memc[ibuf+ip-1])
		    } else {
		        call fprintf (out, "<dl id=\"l_%s\">\n<dt><b>%s")
		            call pargstr (Memc[name])
		            call pargstr (Memc[ibuf+ip-1])
		    }
		    call fprintf (out, "</b></dt>\n")

		    # Write out a comment line for the GUI to use.
	    	    call lh_escape(Memc[unesc+ip-1], font, true, YES, SZ_LINE)
	    	    Memc[unesc+strlen(Memc[unesc])-1] = EOS
		    call fprintf (out, 
			"<!-- Sec=%s Level=%d Label=\'%s\' Line=\'%s\' -->\n<dd>")
			    if (nsec > 0)
		                call pargstr (SECTION(sptr, nsec-1))
			    else
		                call pargstr ("None")
		            call pargi (ls_level)
		            call pargstr (Memc[name])
			    if (Memc[unesc+ip-1] == '\n')
		                call pargstr (" ")
			    else
		                call pargstr (Memc[unesc+ip-1])
		    ls_level = ls_level + 1

		case LE:		# end labelled section
		    call fprintf (out, "</dd>\n</dl>\n")
		    ls_level = ls_level - 1
		    if (quit_at_le)
			break

                case HR:		# HREF anchor
                    # HTML href anchor of the form ".hr <href> <anch_text>",
		    # we skip ahead to the <text> and process as a normal line.
	    	    Memc[ibuf+ip+strlen(Memc[ibuf+ip])-1] = EOS
                    for (i=0; !IS_WHITE(Memc[ibuf+ip]); ip=ip+1) {
			Memc[name+i] = Memc[ibuf+ip]
                        i = i + 1
		    }
		    Memc[name+i] = EOS

		    call fprintf (out, "<a href=\"%s\">%s</A>\n")
		        call pargstr (Memc[name])
		        call pargstr (Memc[ibuf+ip+1])

                case HN:		# NAME target
                    # HTML name target of the form ".hn <name>", strip the
		    # newline added in the escape routine.
	    	    Memc[ibuf+ip+strlen(Memc[ibuf+ip])-1] = EOS
		    call fprintf (out, "<span id=\"%s\"></span>\n")
			call pargstr (Memc[ibuf+ip])

		case BP:		# break page
		    # no-op
		    next
		case TP:		# test space left on page
		    # no-op
		    next
		case KS:		# start floating keep
		    if (in_para == true) {
		        call fprintf(out, "</p>\n")
			in_para = false
		    }
		    if (in_pre == false) {
		        call fprintf (out, "<pre>\n")
		        in_pre = true
		    }
		case KE:		# end floating keep
		    if (in_pre == true) {
		        call fprintf (out, "</pre>\n")
		        in_pre = false
		    }
		case ENDHELP:		# end of help block
		    break
		}

	    default:
		if (lastline == DIRECTIVE) {

		    # Section directive name.  For certain standard sections
		    # we'll force an indention to make the output look better,
		    # everything else gets written normally.

		    # Save the section name.
		    call salloc (SPTR(sptr,nsec), SZ_LINE, TY_CHAR)
	    	    call aclrc (SECTION(sptr,nsec), SZ_LINE)
	    	    Memc[ibuf+strlen(Memc[ibuf])-1] = EOS
		    call sprintf (SECTION(sptr,nsec), SZ_LINE, "\'%s\'")
			call pargstr (Memc[ibuf])

		    if (in_para == true) {
		        call fprintf (out, "</p>\n")
			in_para = false
		    }
		    if (nsec > 0) {
		        call fprintf (out, "</section>\n")
		    }

		    # Make the section name a URL target.
		    call lh_mkname (Memc[ibuf], Memc[name])
		    call fprintf (out, "<section id=\"s_%s\">\n")
		        call pargstr (Memc[name])

		    if (Memc[level] == EOS) {
		        call fprintf (out, "<h2>%s</h2>\n")
			    call pargstr (Memc[ibuf])
		    } else {
		        call fprintf (out,
			    "<span>%s %s</span>\n")
			    	call pargstr (Memc[level])
			    	call pargstr (Memc[ibuf])
       	    	    	Memc[level] = EOS
		    }

		    lastline = NAME
		    nsec = nsec + 1
		    if (section[1] != EOS)
	    	 	quit_at_ih = true

		} else {
		    # Ordinary text line.
text_		    if (in_para == false && in_pre == false && ls_level == 0) {
			call fprintf (out, "<p>\n")
			in_para = true
		    }
		    call fprintf (out, "%s")
	   	        call pargstr (Memc[ibuf])
		    lastline = TEXT
		}
	    }

	    call aclrc (Memc[ibuf], SZ_IBUF)
	    call aclrc (Memc[unesc], SZ_IBUF)
	    call aclrc (Memc[name], SZ_LINE)
	}

	if (in_para == true) {
	    call fprintf(out, "</p>\n")
	    in_para = false
	}
	# Close the last section.
	if (nsec > 0) {
	    call fprintf (out, "\n</section>\n\n")
	}

	# Write out an HTML comment giving the document section names.
	call fprintf (out, "<!-- Contents: ")
	for (i=0; i < nsec; i=i+1) {
	    call fprintf (out, "%s ")
		call pargstr (SECTION(sptr,i))
	}
	call fprintf (out, " -->\n\n")
	call fprintf (out, "</body>\n</html>\n")

	call flush (out)
err_	call sfree (sp)
end


# LH_PROLOG --  Begin the HTML output, print the header table for a help
# page if we have the information.

procedure lh_prolog (fd, mod, date, title)

int	fd					#I output file descriptor
char	mod[ARB]				#I .help module name
char	date[ARB]				#I .help keyword 2
char	title[ARB]				#I .help keyword 3

begin
        call fprintf (fd, "<!DOCTYPE html>\n<HTML>\n<HEAD>\n")
	if (title[1] != EOS) {
            call fprintf (fd, "<title>%s</title>\n")
                call pargstr (title)
	}
        call fprintf (fd, "</head>\n<body>\n")

	# If we only have the module name don't bother with header.
	if (date[1] == EOS && title[1] == EOS)
	    return

	# Begin the HTML output prolog.
        call fprintf (fd, "<table style=\"width:100%%; border:0;\"><tr>\n")

	# Left side page header.
        call fprintf (fd, "<td style=\"text-align: left;\">\n")
	if (date[1] == EOS) {
            call fprintf (fd, "<b>%s</b>")
                call pargstr (mod)
	} else {
            call fprintf (fd, "<b>%s (%s)</b>")
                call pargstr (mod)
                call pargstr (date)
	}
        call fprintf (fd, "</td>\n")

	# Center page header.
	if (title[1] != EOS) {
	        call fprintf (fd, "<td style=\"text-align: center;\">\n")
            call fprintf (fd, "<b>%s</b>\n")
                call pargstr (title)
            call fprintf (fd, "</td>\n")
	}

	# Right side page header.
        call fprintf (fd, "<td style=\"text-align: right;\">\n")
	if (date[1] == EOS) {
            call fprintf (fd, "<b>%s</b>")
                call pargstr (mod)
	} else {
            call fprintf (fd, "<b>%s (%s)</b>")
                call pargstr (mod)
                call pargstr (date)
	}
        call fprintf (fd, "</td>\n")

        call fprintf (fd, "</tr></table><p>\n")
end


# LH_ESCAPE -- Escape any HTML problem characters in the line ('<','>','&')
# as well as the font changes.

procedure lh_escape (str, font, format, special_only, maxch)

char	str[ARB]				#I string to edit
int	font					#U current font
bool	format					#I formatting flag
int	special_only				#I escape only special chars?
int	maxch					#I max length of string

pointer	sp, ip, buf, keyword
int	i, gstrcpy(), stridx()

define	copy_	90

begin
	call smark (sp)
	call salloc (buf, maxch, TY_CHAR)
	call salloc (keyword, maxch, TY_CHAR)
	call aclrc (Memc[buf], maxch)
	call aclrc (Memc[keyword], maxch)

	ip = buf
	for (i=1; str[i] != EOS && i <= maxch; i = i + 1) {

	    if (special_only == YES && stridx (str[i], "<>&") == 0)
		goto copy_

	    switch (str[i]) {

	    # Handle special chars.
	    case '<':
		ip = ip + gstrcpy ("&lt;", Memc[ip], SZ_LINE)
	    case '>':
		ip = ip + gstrcpy ("&gt;", Memc[ip], SZ_LINE)
	    case '&':
		ip = ip + gstrcpy ("&amp;", Memc[ip], SZ_LINE)

	    # Quoted single chars and strings get a special font.
	    case '\'':
		if (str[i+2] == '\'') {
		    ip = ip + gstrcpy ("<span style=\"font-family: monospace;\">",
		                       Memc[ip], SZ_LINE)
		    switch (str[i+1]) {
		    # Handle special chars.
		    case '<':
			ip = ip + gstrcpy ("'&lt;'", Memc[ip], SZ_LINE)
	            case '>':
		        ip = ip + gstrcpy ("'&gt;'", Memc[ip], SZ_LINE)
	            case '&':
		        ip = ip + gstrcpy ("'&amp;'", Memc[ip], SZ_LINE)
		    default:
	                ip = ip + gstrcpy (str[i],  Memc[ip], 3)
		    }
		    ip = ip + gstrcpy ("</span>", Memc[ip], SZ_LINE)
		    i = i + 2
		} else
		    goto copy_
	    case '`':
		if (str[i+2] == '`' || str[i+2] == '\'') {
		    ip = ip + gstrcpy ("<span style=\"font-family: monospace;\">",  Memc[ip], SZ_LINE)
		    switch (str[i+1]) {
		    # Handle special chars.
		    case '<':
			ip = ip + gstrcpy ("`&lt;`", Memc[ip], SZ_LINE)
	            case '>':
		        ip = ip + gstrcpy ("`&gt;`", Memc[ip], SZ_LINE)
	            case '&':
		        ip = ip + gstrcpy ("`&amp;`", Memc[ip], SZ_LINE)
		    default:
	                ip = ip + gstrcpy (str[i],  Memc[ip], 3)
		    }
		    ip = ip + gstrcpy ("</span>", Memc[ip], SZ_LINE)
		    i = i + 2
		} else
		    goto copy_
	    case '"':
                if (format) {
	            if (font == F_TELETYPE) {
	                # Do a closing quote.
	                ip = ip + gstrcpy ("\"</span>", Memc[ip], SZ_LINE)
		        font = F_ROMAN
		    } else if (font == F_ROMAN) {
		        # Do an opening quote.
		        ip = ip + gstrcpy ("<span style=\"font-family: monospace;\">\"", Memc[ip], SZ_LINE)
		        font = F_TELETYPE
		    } else
		        goto copy_
		} else 
		    goto copy_

	    # Process font changes.
	    case '\\':
		if (str[i+1] == 'f') {
		    if (str[i+2] == 'B') {
			if (font == F_BOLD)
			    next
			if (font == F_ITALIC)
			    ip = ip + gstrcpy ("</i>", Memc[ip], SZ_LINE)
			ip = ip + gstrcpy ("<b>", Memc[ip], SZ_LINE)
			font = F_BOLD

		    } else if (str[i+2] == 'I') {
			if (font == F_ITALIC)
			    next
			if (font == F_BOLD)
			    ip = ip + gstrcpy ("</b>", Memc[ip], SZ_LINE)
			ip = ip + gstrcpy ("<i>", Memc[ip], SZ_LINE)
			font = F_ITALIC

		    } else if (str[i+2] == 'R') {
			if (font == F_BOLD)
			    ip = ip + gstrcpy ("</b>", Memc[ip], SZ_LINE)
			else if (font == F_ITALIC)
			    ip = ip + gstrcpy ("</i>", Memc[ip], SZ_LINE)
			font = F_ROMAN

		    } else if (str[i+2] == 'P') {
			if (font == F_BOLD) {
			    ip = ip + gstrcpy ("</b>", Memc[ip], SZ_LINE)
			} else if (font == F_ITALIC) {
			    ip = ip + gstrcpy ("</i>", Memc[ip], SZ_LINE)
			}
			font = F_ROMAN
		    }
		    i = i + 2
		} else if (str[i+1] == '\n' || str[i+1] == EOS) {
		    Memc[ip] = str[i]
		    ip = ip + 1
		    i = i + 1
		    ip = ip + gstrcpy ("<br>", Memc[ip], SZ_LINE)
		} else {
		    goto copy_
                }

	    default:
copy_		Memc[ip] = str[i]
		ip = ip + 1
	    }
	}

	# Add the trailing newline we stripped above.
	ip = ip + gstrcpy ("\n\0", Memc[ip], SZ_LINE)

	# Move the string back.
	call amovc (Memc[buf], str, maxch)

	call sfree (sp)
end


# LH_SET_LEVEL -- Increment the level number of a numbered header.

procedure lh_set_level (n, level)

int	n					#I level number
char	level[ARB]				#U level string

int	i, strlen()
include	"lroff.com"

begin
        # Increment the desired section number; zero all higher
	# numbered section counters.
        nh_level[n] = nh_level[n] + 1
        call amovki (0, nh_level[n+1], MAX_NHLEVEL - n)

        # Output the section number followed by a blank and then
	# the section label.
	level[1] = EOS
        do i = 1, n {
	    call sprintf (level[strlen(level)+1], SZ_IBUF, "%d.")
	        call pargi (nh_level[i])
	}

        # Cancel the final "." if subsection heading.  Add a blank.
        if (n > 1 && level[strlen(level)] == '.')
            level[strlen(level)] = EOS
end


# LH_FINDBLOCK -- If text contains format directives, eat input lines until
# a ".ls" directive is found which contains the block name as a substring.
# If the text is not formatted, search for a line beginning with the pattern.

int procedure lh_findblock (fd, formatted, param)

int	fd
bool	formatted
char	param[ARB]

bool	match_found
pointer	sp, lbuf, pattern
int	len
int	getline(), strmatch(), strlen()
errchk	getline

define	err_	90

begin
	call smark (sp)
	call salloc (pattern, SZ_FNAME, TY_CHAR)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	match_found = false

	# Get the first line.
	if (getline (fd, Memc[lbuf]) == EOF)
	    goto err_

	if (formatted) {
	    call sprintf (Memc[pattern], SZ_FNAME, "{%s}")
		call pargstr (param)
	    repeat {
		if (strmatch (Memc[lbuf], "^.{ls}") > 0)
		    if (strmatch (Memc[lbuf], Memc[pattern]) > 0) {
			match_found = true
			break
		    }
	    } until (getline (fd, Memc[lbuf]) == EOF)

	} else {
	    call sprintf (Memc[pattern], SZ_FNAME, "^#{%s}")
		call pargstr (param)
	    repeat {
		if (strmatch (Memc[lbuf], Memc[pattern]) > 0) {
		    match_found = true
		    break
		}
	    } until (getline (fd, Memc[lbuf]) == EOF)
	}
        call ungetline (fd, Memc[lbuf])

err_	len = strlen (Memc[lbuf])
	call sfree (sp)
	if (match_found)
	    return (len)
	else
	    return (EOF)
end


# LH_FINDSECTION -- If text contains format directives, eat input lines until
# a ".ih" directive is found for the named section.  If the text is not
# formatted, search for a line beginning with the section name.

define	MAXPAT		10

int procedure lh_findsection (fd, formatted, sections)

int	fd			# input file
bool	formatted		# is help block formatted
char	sections[ARB]		# list of sections "a|b|c"

bool	match_found
int	npat, ip
pointer	sp, patbuf, patoff[MAXPAT], op
char	lbuf[SZ_LINE]

bool	lh_match()
int	getline(), strmatch()
errchk	getline

define	err_	91

begin
	call smark (sp)
	call salloc (patbuf, SZ_LINE, TY_CHAR)

	# Process the list of sections into patbuf and patoff, i.e., into a
	# list of EOS delimited strings in the string buffer patbuf.  Each
	# section name or abbreviation is delimited by '|' (or).

	npat = 1
	op = patbuf
	patoff[1] = op

	# Get the first line.
	if (getline (fd, lbuf) == EOF)
	    goto err_

	for (ip=1;  sections[ip] != EOS;  ip=ip+1)
	    switch (sections[ip]) {
	    case '|':
		Memc[op] = EOS
		op = op + 1
		npat = min (MAXPAT, npat + 1)
		patoff[npat] = op
	    default:
		Memc[op] = sections[ip]
		op = op + 1
	    }
	Memc[op] = EOS

	match_found = false

	if (formatted) {
	    repeat {
		if (strmatch (lbuf, "^.{ih}") > 0)
		    if (getline (fd, lbuf) != EOF) {
			match_found = lh_match (lbuf, patoff, npat)
			if (match_found)
			    break
		    }
	    } until (getline (fd, lbuf) == EOF)
            call ungetline (fd, lbuf)
	    call ungetline (fd, ".ih\n")

	} else {
	    repeat {
		match_found = lh_match (lbuf, patoff, npat)
		if (match_found)
		    break
	    } until (getline (fd, lbuf) == EOF)
            call ungetline (fd, lbuf)
	}

err_	call sfree (sp)
        if (match_found)
            return (OK)
        else
            return (EOF)
end


# LH_MATCH -- Match a set of patterns against a line of test, matching only
# at the beginning of line in either case.

bool procedure lh_match (lbuf, patoff, npat)

char    lbuf[ARB]               # line of text
pointer patoff[npat]            # pointers to pattern strings
int     npat                    # number of patterns

int     pat
pointer sp, pattern
int     strmatch()

begin
        call smark (sp)
        call salloc (pattern, SZ_FNAME, TY_CHAR)

        for (pat=1;  pat <= npat;  pat=pat+1) {
            call sprintf (Memc[pattern], SZ_FNAME, "^{%s}")
                call pargstr (Memc[patoff[pat]])
            if (strmatch (lbuf, Memc[pattern]) > 0) {
        	call sfree (sp)
                return (true)
	    }
        }

        call sfree (sp)
        return (false)
end


# LH_MKNAME -- Given a string make it suitable for use as an HREF name.

procedure lh_mkname (instr, outstr)

char	instr[ARB]
char	outstr[ARB]

int	i

begin
	# Make it a URL.  First convert the section name to a
	# lower-case string and replace the blanks.
	call strcpy (instr, outstr, SZ_LINE)
	call strlwr (outstr)
	for (i=1; i < SZ_LINE; i=i+1)
	    if (outstr[i] == EOS || outstr[i] == '\n')
		break
	    else if (!IS_ALNUM(outstr[i]))
	        outstr[i] = '_'
end
