# sp_strncpy - Counted character copy.
#
# History
#   1Apr91 - Created by Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

procedure sp_strncpy( input, n_chars, output )

char input[ARB]   # I:  The input string to copy to the output.
int  n_chars      # I:  The number of characters to copy.
char output[ARB]  # O:  The output string.

# Declarations.
int i             # Index.

begin

  for( i = 1; i <= n_chars; i = i + 1 )
    output[i] = input[i]

end
#---------------------------------------------------------------------------
# End of sp_strncpy.
#---------------------------------------------------------------------------
