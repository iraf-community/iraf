include	<syserr.h>
include	<ctype.h>
include	<psset.h>
include	"lroff.h"


define  DIRECTIVE       1			# processing codes
define  NAME            2
define  TEXT            3
define  NEWLINE         4

define	INDENT		5			# size of indentitudedness


# LROFF2PS -- Convert LROFF text to Postscript.  By default we process the
# entire file however we allow for the printing of only a particular section
# or labelled text block to be compatible with the HELP task options.
# If a section name is given that section will be printed and and .ls
# block request will be ignored.

procedure lroff2ps (in, out, psptr, ls_block, section)

int	in					#i input file descriptor
int	out					#i output file descriptor
pointer	psptr					#i PSIO pointer
char	ls_block[ARB]				#i .ls block to search for
char	section[ARB]				#i section to print

pointer sp, ip, ps
pointer ibuf, line, level
int	lastline, font, indent, ls_level
int	i, arg, nsec, cmd, last_cmd
bool	format, quit_at_le, quit_at_ih, formatted

pointer	ps_open()
int	lh_findsection(), lh_findblock(), nextcmd()
int	stridxs(), getline(), strlen(), strmatch(), lgetarg()

define	text_ 	99
define	err_ 	98

include	"lroff.com"

begin
	call smark (sp)
	call salloc (ibuf, SZ_IBUF, TY_CHAR)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (level, SZ_FNAME, TY_CHAR)

	call aclrc (Memc[ibuf], SZ_LINE)
	call aclrc (Memc[line], SZ_LINE)
	call aclrc (Memc[level], SZ_LINE)

	# Initialize.
	lastline  = TEXT
	font 	  = F_ROMAN
	indent    = 0
	nsec 	  = 0
	ls_level  = 0
        format    = true
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

	# Begin the output is we aren't passed a pointer that may have
	# already set up headers, page parameters, etc.
	if (psptr == NULL)
	    ps = ps_open (out, YES)
	else
	    ps = psptr
	call ps_write_prolog (ps)

	# Process the file.
	while (getline (in, Memc[ibuf]) != EOF) {

	    # Escape the line, inserting special font changes for quotes
	    # chars and strings.
	    call lp_escape (Memc[ibuf], font, format, SZ_LINE)

	    switch (Memc[ibuf]) {
	    case '\n':
		if (lastline != NEWLINE)
		    call ps_linebreak (ps, NO)
		call ps_newline (ps)
		lastline = NEWLINE

	    case '.':
		# Swallow any help strings if present.
		if (strmatch (Memc[ibuf], "^.{help}") > 0)
		    next

	        # Stomp the newline.
	        Memc[ibuf+strlen(Memc[ibuf])-1] = EOS

		# Process the directive, position the ip at the beginning
		# of any argument.
		ip = 1
		cmd = nextcmd (Memc[ibuf], ip)
                while (IS_WHITE(Memc[ibuf+ip]))		# skip spaces
                    ip = ip + 1

		switch (cmd) {
		case FI, KE:		# enter fill mode
		    call ps_setfont (ps, F_ROMAN)
		    call ps_set_justify (ps, YES)
		    lastline = NEWLINE
		    format = true
		case NF, KS:		# leave fill mode (nofill)
		    if (lastline != NEWLINE && last_cmd != KS && last_cmd != NF)
		        call ps_linebreak (ps, NO)
		    call ps_setfont (ps, F_TELETYPE)
		    call ps_set_justify (ps, NO)
		    lastline = NEWLINE
		    format = false
		case JU:		# enter line justification mode
		    call ps_set_justify (ps, YES)
		    next
		case NJ:		# leave line justification mode
		    call ps_set_justify (ps, NO)
		    next
		case RJ:		# right justify text on next line
		    call ps_linebreak (ps, NO)
		    call strcpy (Memc[ibuf+4], Memc[line], SZ_FNAME)
	    	    Memc[line+strlen(Memc[line])-1] = EOS
		    if (getline (in, Memc[ibuf]) != EOF) {
		        call ps_output (ps, Memc[ibuf], NO)
		        call ps_rightjustify (ps, Memc[line])
		    }
		    next

		case SH:		# section heading
		    if (ls_level > 0) {	# for missing .le statements
		        ls_level = 0
		        indent = 0
		    }
		    if (nsec > 0) {
		        call ps_linebreak (ps, NO)
		        call ps_linebreak (ps, NO)
		    }
		    lastline = DIRECTIVE
       	    	    Memc[level] = EOS
		    next
		case IH:		# indented section heading
		    if (ls_level > 0) {	# for missing .le statements
		        ls_level = 0
		        indent = 0
		    }
			
		    if (nsec > 0)
		        call ps_linebreak (ps, NO)
		    if (lastline == TEXT)
		        call ps_linebreak (ps, NO)
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
		    if (ls_level > 0) {	# for missing .le statements
		        ls_level = 0
		        indent = 0
		    }
		    call lh_set_level (lgetarg(Memc[ibuf],ip,1), Memc[level])
		    if (nsec > 0) {
		        call ps_linebreak (ps, NO)
		        call ps_linebreak (ps, NO)
		    }
		    lastline = DIRECTIVE
		    next

		case CE:		# center next line
		    if (getline (in, Memc[ibuf]) != EOF) {
	    	        call lp_escape (Memc[ibuf], font, true, SZ_LINE)
			call ps_linebreak (ps, NO)
			call ps_center (ps, Memc[ibuf])
		    }

		case BR:		# break line
		    call ps_linebreak (ps, NO)
		case SP:		# break, space N spaces on output
		    arg = lgetarg (Memc[ibuf], ip, 1)
		    call ps_linebreak (ps, NO)
		    for (i=1; i < arg; i = i + 1)
		        call ps_linebreak (ps, NO)
		case IN:		# indent +/- N spaces
		    arg = lgetarg (Memc[ibuf], ip, 0)
		    call ps_indent (ps, arg)
		    next

		case LS:		# begin labelled section
		    arg = lgetarg (Memc[ibuf], ip, INDENT)
	  	    if (arg == 0)
			ip = 5

		    if (lastline == TEXT)
		        call ps_linebreak (ps, NO)
		    call ps_testpage (ps, 2)
		    if (ls_level < 1 || (last_cmd == LS && lastline == TEXT))
		        call ps_linebreak (ps, NO)
		    call ps_spfont (ps, F_BOLD)
		    call ps_output (ps, Memc[ibuf+ip-1], NO)
		    if (strlen (Memc[ibuf+ip-1]) > (INDENT-1))
		        call ps_linebreak (ps, NO)
		    call ps_spfont (ps, NULL)
		    call ps_setfont (ps, F_ROMAN)

		    indent = max (0, indent + INDENT)
		    call ps_indent (ps, indent)
		    ls_level = ls_level + 1

		case LE:		# end labelled section
		    if (last_cmd != LE || (last_cmd == LE && lastline == TEXT))
			call ps_linebreak (ps, NO)
		    indent = max (0, indent - INDENT)
		    call ps_indent (ps, indent)
		    ls_level = ls_level - 1
		    lastline = NEWLINE
		    if (quit_at_le)
			break

                case HR:		# HREF anchor
                    # HTML href anchor of the form ".hr <href> <anch_text>",
		    # we skip ahead to the <text> and process as a normal line.
                    for (i=0; !IS_WHITE(Memc[ibuf+ip]); ip=ip+1)
                        i = i + 1
		    call ps_deposit (ps, Memc[ibuf+ip+1])

                case HN:		# NAME target
                    # HTML name target of the form ".hn <name>".
		    # no-op

		case BP:		# break page
		    call ps_linebreak (ps, NO)
		    call ps_pagebreak (ps)
		    next
		case TP:		# test space left on page
		    # no-op
		    next
		case ENDHELP:		# end of help block
		    break
		}
		last_cmd = cmd

	    default:
	        # Stomp the newline.
	        Memc[ibuf+strlen(Memc[ibuf])-1] = EOS

		if (lastline == DIRECTIVE) {

		    # Section directive name.  For certain standard sections
		    # we'll force an indention to make the output look better,
		    # everything else gets written normally.

		    indent = max (0, indent - INDENT)
		    call ps_indent (ps, indent)

		    call ps_setfont (ps, F_BOLD)
		    call ps_testpage (ps, 3)
		    if (Memc[level] == EOS) {
			call ps_output (ps, Memc[ibuf], NO)
		    } else {
			call sprintf (Memc[line], SZ_LINE, "%s %s")
			    call pargstr (Memc[level])
			    call pargstr (Memc[ibuf])

			call ps_output (ps, Memc[line], NO)
       	    	    	Memc[level] = EOS
		    }
		    call ps_setfont (ps, F_ROMAN)
		    call ps_linebreak (ps, NO)

		    indent = max (0, indent + INDENT)
		    call ps_indent (ps, indent)

		    lastline = NAME
		    nsec = nsec + 1
		    if (section[1] != EOS)
	    	 	quit_at_ih = true

		} else {
		    # Ordinary text line.
text_		    if (format) {
	    	        call ps_deposit (ps, Memc[ibuf])
		    } else {
			call lp_strdetab (Memc[ibuf], Memc[line], SZ_LINE)
	    	        call ps_output (ps, Memc[line], NO)
		        call ps_linebreak (ps, NO)
		    }
		    lastline = TEXT
		}
	    }

	    call aclrc (Memc[ibuf], SZ_LINE)
	    call aclrc (Memc[line], SZ_LINE)
	}

	# Close the last section.
	call ps_linebreak (ps, NO)
	call ps_close (ps)
	
	call flush (out)
err_	call sfree (sp)
end


# LP_ESCAPE -- Escape any HTML problem characters in the line ('<','>','&')
# as well as the font changes.

procedure lp_escape (str, font, format, maxch)

char	str[ARB]				#i string to edit
int	font					#u current font
bool	format					#i formatting flag
int	maxch					#i max length of string

pointer	sp, ip, buf, keyword
int	i, strmatch(), gstrcpy()
bool	is_ls

define	copy_	90

begin
	call smark (sp)
	call salloc (buf, maxch, TY_CHAR)
	call salloc (keyword, maxch, TY_CHAR)
	call aclrc (Memc[buf], maxch)
	call aclrc (Memc[keyword], maxch)

	ip = buf
        is_ls = FALSE
        if (strmatch (str, "^.{ls}") > 0)
            is_ls = TRUE

	for (i=1; str[i] != EOS && i <= maxch; i = i + 1) {

	    switch (str[i]) {

	    # Quoted single chars and strings get a special font.
	    case '\'', '`':
		if (str[i+2] == '`' || str[i+2] == '\'') {
		    if (format)
		        ip = ip + gstrcpy ("\\fT",  Memc[ip], SZ_LINE)
		    ip = ip + gstrcpy (str[i],  Memc[ip], 3)
		    if (format) {
			if (is_ls)
		            ip = ip + gstrcpy ("\\fB", Memc[ip], SZ_LINE)
			else {
			    switch (font) {
			    case F_ROMAN:
		                ip = ip + gstrcpy ("\\fR", Memc[ip], SZ_LINE)
			    case F_BOLD:
		                ip = ip + gstrcpy ("\\fB", Memc[ip], SZ_LINE)
			    case F_ITALIC:
		                ip = ip + gstrcpy ("\\fI", Memc[ip], SZ_LINE)
			    case F_TELETYPE:
		                ip = ip + gstrcpy ("\\fT", Memc[ip], SZ_LINE)
			    default:
		                ip = ip + gstrcpy ("\\fR", Memc[ip], SZ_LINE)
			    }
			}
		    }
		    i = i + 2
		} else
		    goto copy_
	    case '"':
                if (format && str[i+1] != '/' && str[i+2] != '/') {
		    if (font == F_TELETYPE) {
		        # Do a closing quote.
			if (format) {
			    if (is_ls)
		                ip = ip + gstrcpy ("\"\\fB", Memc[ip], SZ_LINE)
			    else
		                ip = ip + gstrcpy ("\"\\fR", Memc[ip], SZ_LINE)
			}
		        font = F_ROMAN
		    } else if (font == F_ROMAN) {
		        # Do an opening quote.
			if (format)
		            ip = ip + gstrcpy ("\\fT\"", Memc[ip], SZ_LINE)
		        font = F_TELETYPE
		    } else
		        goto copy_
		} else 
		    goto copy_

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


# LP_STRDETAB -- Procedure to remove tabs from a line of text.

procedure lp_strdetab (line, outline, maxch)

char   line[ARB], outline[ARB]
int    maxch

int    ip, op

begin
        ip = 1
        op = 1

        while (line[ip] != EOS && op <= maxch) {
            if (line[ip] == '\t') {
                repeat {
                    outline[op] = ' '
                    op = op + 1
                } until (mod(op,8) == 0 || op > maxch)
                ip = ip + 1
            } else {
                outline[op] = line [ip]
                ip = ip + 1
                op = op + 1
            }
        }

        outline[op] = EOS
end
