#---------------------------------------------------------------------------
.help od_parse Feb93 source
.ih
NAME
od_parse -- Parse a section for column names.
.ih
USAGE
call od_parse
.ih
ARGUMENTS
.ih
DESCRIPTION
Taken from Bernie Simon's aspare without any modifications.
.endhelp
#---------------------------------------------------------------------------
#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	06-Aug-92	removed code which deletes commas

# OD_PARSE -- Parse a file name specification into file name and section fields
#
#	Syntax:		filename[section]
#
# The [ character must be escaped to be included in the filename.
# This syntax is similar to the image section syntax in imio, but
# is intended to extract variable names or numbers, column names, etc.
# for the Astronomical Survival analysis suite of programs.
# The section field is returned as a string with no leading or trailing 
# brackets.

procedure od_parse (filespec, file, sz_file, section, sz_section)

char	filespec[ARB]		# i: full file specification
char	file[sz_file]		# o: receives file name
int	sz_file			# i: max chars in file name
char	section[sz_section]	# o: receives section
int	sz_section		# i: max chars in section name
#--
int	ch, ip, op, right

int	strlen()

begin
	ip = 1
	op = 1

	# Extract file name.  The first (unescaped) [ marks the start of
	# the section field.

	for (ch=filespec[ip];  ch != EOS && ch != '[';  ch=filespec[ip]) {
	    if (ch == '\\' && filespec[ip+1] == '[') {
		file[op] = '\\'
		op = op + 1
		file[op] = '['
		ip = ip + 1
	    } else
		file[op] = ch

	    op = min (sz_file, op + 1)
	    ip = ip + 1
	}

	file[op] = EOS
	section[1]  = EOS

	if (ch == EOS)
	    return

	# If we have a [...] field, copy the section string,
	# removing the brackets, and any commas used as delimiters.

	# Eliminate the leading "["
	ip = ip + 1
	call strcpy (filespec[ip], section, sz_section)

	# Remove the trailing "]"
	right = strlen (section)
	if (section[right] == ']')
	    section[right] = EOS

end
#---------------------------------------------------------------------------
# End of od_parse
#---------------------------------------------------------------------------
