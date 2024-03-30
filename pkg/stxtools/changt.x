#---------------------------------------------------------------------------
.help change_ext Jun93 xtools
.ih
NAME
change_ext -- Put the specified extension on a file name.
.ih
USAGE
call change_ext (in_name, newext, out_name, max_size)
.ih
ARGUMENTS
.ls in_name (I: char[ARB])
The input pathname with which to change the arguments.
.le
.ls newext (I: char[ARB])
The extension to replace the original extension with.
.le
.ls out_name (O: char[max_size])
The resultant pathname with the new extension inserted.  May be the
same as the in_name.
.le
.ls max_size (I: int)
The maximum length of the string out_name.
.le
.ih
DESCRIPTION
This routine replaces the old extension on a pathname with the new
extension specified in the argument "newext".  Thus:

.nf
        dir$root.ext --> dir$root.newext
.fi
.ih
SEE ALSO
fparse
.endhelp
#---------------------------------------------------------------------------
procedure change_ext (in_name, newext, out_name, max_size)

char    in_name[ARB]            # I:  Original input name.
char    newext[ARB]             # I:  Extension to replace old one.
char    out_name[max_size]      # O:  File name with new extension.
int     max_size                # I:  Maximum size out_name.

# Misc.
pointer dir                     # Directory part of pathname.
int     index                   # Group index in pathname.
pointer ksection                # Unparsable part of pathname.
int     ngroup                  # Number of groups.
pointer root                    # Root part of pathname.
pointer section                 # Section part of pathname.
pointer sp                      # Stack pointer.
pointer sx                      # Generic string.

begin
        call smark (sp)
        call salloc (dir, SZ_LINE, TY_CHAR)
        call salloc (root, SZ_LINE, TY_CHAR)
        call salloc (section, SZ_LINE, TY_CHAR)
        call salloc (ksection, SZ_LINE, TY_CHAR)
        call salloc (sx, SZ_LINE, TY_CHAR)

        # Parse the file name COMPLETELY.
        call fparse (in_name, Memc[dir], SZ_LINE, Memc[root], SZ_LINE,
                     Memc[sx], SZ_LINE, index, ngroup,
                     Memc[section], SZ_LINE, Memc[ksection], SZ_LINE)

        # Put directory and root together.
        call strcpy (Memc[dir], out_name, max_size)
        call strcat (Memc[root], out_name, max_size)

        # Change the extension.
        call strcat (".", out_name, max_size)
        call strcat (newext, out_name, max_size)

        # Handle group syntax.
        if (index > 0) {
            call sprintf (Memc[sx], SZ_LINE, "[%d")
            call pargi (index)
            call strcat (Memc[sx], out_name, max_size)
            if (ngroup > 0) {
                call sprintf (Memc[sx], SZ_LINE, "/%d")
                call pargi (ngroup)
                call strcat (Memc[sx], out_name, max_size)
            }
            call strcat ("]", out_name, max_size)
        }

        # Append the "unparsable" parts.
        call strcat (Memc[ksection], out_name, max_size)

        # Finally image sections.
        call strcat (Memc[section], out_name, max_size)

        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of change_ext
#---------------------------------------------------------------------------
