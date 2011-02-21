# sp_change_string - Replace a string  with the indicated string.
#
# History
#   1Apr91 - Created by Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

procedure sp_change_string( input, old, new, output, max )

char input[ARB]   # I:  The input string.
char old[ARB]     # I:  The string segment to be replaced.
char new[ARB]     # I:  The string to replace the old with.
char output[ARB]  # O:  The modified input string.
int  max          # I:  The maximun length of the output string.

# Declarations.
int after         # Next character position after match.
int first         # First character position of matched string.
int ilen          # Length of input.
int ip            # Pointer into input.
int last          # Last character position of matched string.
int old_len       # Length of old.
int op            # Pointer into output.

# Function declarations.
int gstrcpy(), strlen(), gstrmatch()

begin

  # Initialize the string pointers.
  ip = 1
  op = 1
  ilen = strlen( input )
  old_len = strlen( old )

  # Keep going until either the input string has been completely copied
  # or the output string is full.
  while( ip < ( ilen + 1 ) && op < ( max + 1 ) ) {

    # Search for the old string.
    after = gstrmatch( input[ip], old, first, last )

    # If the string is not found, then copy the rest of the input to the
    # output.
    if( after == 0 ) {
      call strcpy( input[ip], output[op], max - op + 1 )
      ip = ilen + 1
    }

    # The old string is found, copy the input up to the old string
    # and replace the old string.
    else {
      first = min( first - 1, max - op + 1 )
      call sp_strncpy( input[ip], first, output[op] )
      ip = ip + last
      op = op + first
      op = op + gstrcpy( new, output[op], max - op + 1 )
    }

  }

end
#---------------------------------------------------------------------------
# End of sp_change_string
#---------------------------------------------------------------------------
