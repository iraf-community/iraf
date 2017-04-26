# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<ctype.h>
include	<ctotok.h>

define	SZ_LBUF		1024
define	MAX_TAGS	8192
define	SZ_SBUF		256000
define	TAGSFILE	"tags"



# TG_COMPARE -- String compare of two tags.

int procedure tg_compare (s1, s2)

int	s1			# t_sort index of string 1
int	s2			# t_sort index of string 2

int	ntags
pointer	tg_op
pointer	tg_sbuf
pointer	tg_tag[MAX_TAGS]
pointer	tg_file[MAX_TAGS]
pointer	tg_lnum[MAX_TAGS]
pointer	tg_lbuf[MAX_TAGS]
int	tg_sort[MAX_TAGS]

common	/tagcom/ ntags, tg_op, tg_sbuf, tg_tag, tg_file, tg_lnum, tg_lbuf,
	tg_sort

int	strncmp()

begin
	return (strncmp (Memc[tg_tag[s1]], Memc[tg_tag[s2]], ARB))
end


# TG_PUTSTR -- Add a string to the string buffer and return a pointer to
# the beginning of the string.

pointer procedure tg_putstr (str)

char	str[ARB]		# string to be appended

int	nchars
pointer	newstr
int	strlen()

int	ntags
pointer	tg_op
pointer	tg_sbuf
pointer	tg_tag[MAX_TAGS]
pointer	tg_file[MAX_TAGS]
pointer	tg_lnum[MAX_TAGS]
pointer	tg_lbuf[MAX_TAGS]
int	tg_sort[MAX_TAGS]

common	/tagcom/ ntags, tg_op, tg_sbuf, tg_tag, tg_file, tg_lnum, tg_lbuf,
	tg_sort

begin
	nchars = strlen (str)
	newstr = tg_op

	if (tg_op - tg_sbuf + nchars >= SZ_SBUF)
	    call error (2, "out of string buffer space")

	call strcpy (str, Memc[newstr], nchars)
	tg_op = tg_op + nchars + 1

	return (newstr)
end


# TG_GETLONGLINE -- Get a long line, i.e., a logical line possibly spanning
# several physical lines with the newlines escaped at the ends.  Skip
# comment lines and .help sections.

int procedure tg_getlongline (fd, obuf, maxch, linenum)

int	fd			# input file
char	obuf[ARB]		# output buffer
int	maxch
int	linenum

int	op, status
int	getline(), strncmp()

begin
	op = 1

	while (maxch - op + 1 >= SZ_LINE) {
	    # Get next non-comment line.
	    repeat {
		status = getline (fd, obuf[op])
		linenum = linenum + 1

		if (status == EOF) {
		    break
		} else if (obuf[op] == '#') {
		    next
		} else if (obuf[op] == '.') {
		    # Skip help sections.
		    if (strncmp (obuf[op], ".help", 5) == 0) {
			repeat {
			    status = getline (fd, obuf[op])
			    linenum = linenum + 1
			    if (status == EOF)
				break
			    if (strncmp (obuf[op], ".endhelp", 8) == 0)
				break
			}
		    } else
			break
		} else
		    break
	    }

	    if (status == EOF) {
		if (op == 1)
		    return (EOF)
		else
		    return (op - 1)
	    } else
		op = op + status

	    if (obuf[op-2] == '\\' && obuf[op-1] == '\n')
		op = op - 2
	    else
		break
	}

	return (op - 1)
end
