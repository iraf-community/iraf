#---------------------------------------------------------------------------
.help fbuild Nov93 source
.ih
NAME
fbuild -- Build a file name based on components.
.ih
USAGE
call fbuild (dir, root, ext, clindex, clsize, section, ksection,
	     file, sz_file)
.ih
ARGUMENTS
.ls dir (Input: char[ARB])
The directory specification.  This may be blank.  If not, it should
include the directory separator, i.e. a '$' or '/' at the end of the
directory.
.le
.ls root (Input: char[ARB])
The rootname specification.  This may be left blank.  No other
punctuation besides what goes into a rootname is required.
.le
.ls ext (Input: char[ARB])
The file extension specification.  This may be left blank.  If
specified, the extension separater must be prepended, i.e. a period
'.' must be the first character.
.le
.ls clindex (Input: int)
The cl index or group number.  If zero, it will not be placed into the
pathname.
.le
.ls clsize (Input: int)
The cl size (or maximum group) number.  If zero, it will not be placed
into the pathname.
.le
.ls section (Input: char[ARB])
The section specification.  Must include the surrounding '[' and ']'
section separators.
.le
.ls ksection (Input: char[ARB])
The ksection specification.  Must include the surrounding '[' and ']'
ksection separators.
.le
.ls file (Output: char[sz_file])
The output pathname.
.le
.ls sz_file (Input: int)
The maximum size of the output file specification.
.le
.ih
DESCRIPTION
fbuild builds a pathname based on individual components.  This
complements the routine fparse.  For example, if a full pathname
exists, a call to fparse followed by a call to fbuild should reproduce
the pathname.
.ih
REFERENCES
Jonathan Eisenhamer, STSDAS
.ih
SEE ALSO
fparse
.endhelp
#---------------------------------------------------------------------------
procedure fbuild (dir, root, ext, clindex, clsize, section, ksection,
		  file, sz_file)

char	dir[ARB]		# I:  Directory specification.
char	root[ARB]		# I:  Rootname specification.
char	ext[ARB]		# I:  Extension specification.
int	clindex			# I:  Index number.
int	clsize			# I:  Size number.
char	section[ARB]		# I:  Section specification.
char	ksection[ARB]		# I:  KSection specification.
char	file[sz_file]		# O:  File name.
int	sz_file			# I:  Maximum size of the file name.

char Index[SZ_PATHNAME]		# The Group specification.

begin
		call strcpy (dir, file, sz_file)
		call strcat (root, file, sz_file)
		call strcat (ext, file, sz_file)
		call strcpy ("", Index, SZ_PATHNAME)
		if (clindex > 0)
		    if (clsize > 0) {
			call sprintf (Index, SZ_PATHNAME, "[%d/%d]")
			call pargi (clindex)
			call pargi (clsize)
		    } else {
			call sprintf (Index, SZ_PATHNAME, "[%d]")
			call pargi (clindex)
		    }
		call strcat (Index, file, sz_file)
		call strcat (section, file, sz_file)
		call strcat (ksection, file, sz_file)
end
#---------------------------------------------------------------------------
# End of fbuild
#---------------------------------------------------------------------------
