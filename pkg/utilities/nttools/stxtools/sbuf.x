include "sbuf.h"

#---------------------------------------------------------------------------
.help sbuf Mar93 source
.ih
NAME
.nf
sb_open   -- Open an sbuf.
sb_cat    -- Add a string to the end of sbuf.
sb_close  -- Close an sbuf.
sb_string -- Get the string to an sbuf.
.fi
.ih
USAGE
.nf
sb = sb_open()
call sb_cat (sb, str)
call sb_close (sb)
str_ptr = sb_string (sb)
.fi
.ih
ARGUMENTS
.ls sb (pointer :input/output)
The string buffer descriptor.
.le
.ls str (char[ARB] :input)
The string to append to the string buffer.
.le
.ls str_ptr (pointer :output)
A pointer to a string array containing the contents of the string buffer.
When done, the user is required to deallocate this memory using the call
"call mfree (str_ptr, TY_CHAR)".
.le
.ih
DISCUSSION
This interface allows one to handle arbitrarily long strings without
having to worry about the memory management.

There may be other utility routines to add; feel free to do so.
.endhelp
#---------------------------------------------------------------------------
pointer procedure sb_open

pointer sb                      # The sbuf pointer

errchk  malloc

begin
        call malloc (sb, SB_SZ_SB, TY_STRUCT)
        call malloc (SB_PTR(sb), SZ_LINE, TY_CHAR)
        SB_LEN(sb) = 0
        SB_MAXLEN(sb) = SZ_LINE
        
        return (sb)
end
#---------------------------------------------------------------------------
# End of sb_open
#---------------------------------------------------------------------------
procedure sb_close (sb)

pointer sb                      # IO: The sbuf descriptor, NULL on exit.

errchk  mfree

begin
	if (sb != NULL) {
	    call mfree (SB_PTR(sb), TY_CHAR)
	    call mfree (sb, TY_STRUCT)
	}
end
#---------------------------------------------------------------------------
# End of sb_close
#---------------------------------------------------------------------------
pointer procedure sb_string (sb)

pointer sb                      # I:  The sbuf descriptor.

pointer str                     # New string pointer.

begin
        call malloc (str, SB_LEN(sb), TY_CHAR)
        call strcpy (SB_BUF(sb,0), Memc[str], SB_LEN(sb))

        return (str)
end
#---------------------------------------------------------------------------
# End of sb_string
#---------------------------------------------------------------------------
procedure sb_cat (sb, str)

pointer sb                      # I: The sbuf descriptor.
char    str[ARB]                # I:  The string to concatenate.

int     i, strlen()             # Length of input string.

errchk  realloc

begin
        i = strlen (str)
        if (i + SB_LEN(sb) >= SB_MAXLEN(sb)) {
            SB_MAXLEN(sb) = SB_MAXLEN(sb) + i + SZ_LINE
            call realloc (SB_PTR(sb), SB_MAXLEN(sb), TY_CHAR)
        }

        call strcpy (str, SB_BUF(sb,SB_LEN(sb)), i)
        SB_LEN(sb) = SB_LEN(sb) + i
end
#---------------------------------------------------------------------------
# End of sb_cat
#---------------------------------------------------------------------------
