#---------------------------------------------------------------------------
.help sbuf.h Feb93 source
.ih
NAME
sbuf.h -- Memory structure for long strings.
.endhelp
#---------------------------------------------------------------------------
define  SB_LEN          Memi[$1]        # Current length of string.
define  SB_MAXLEN       Memi[$1+1]      # Current maximum size of buffer.
define  SB_PTR          Memi[$1+2]      # Pointer to the string array.
define  SB_BUF          Memc[SB_PTR($1)+$2]
define  SB_SZ_SB        3               # Size of memory structure.
#---------------------------------------------------------------------------
# End of sbuf.h
#---------------------------------------------------------------------------
