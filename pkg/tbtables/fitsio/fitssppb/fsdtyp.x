include "fitsio.h"

procedure fsdtyp(value,dtype,status)

# determine datatype of a FITS value field
# This assumes value field conforms to FITS standards and may not
#    detect all invalid formats.
# value   c  input value field from FITS header record only,
#            (usually the value field is in columns 11-30 of record)
#            The value string is left justified.
# dtype   c  output type (C,L,I,F) for Character string, Logical,
#              Integer, Floating point, respectively

char    value[SZ_FSTRVAL]          # i data value
%       character*70 fvalue
char    dtype           # o datatype code
%       character*1 fdtype
int     status          # o error status
char    sdtype[1]
begin

call f77pak(value,fvalue,SZ_FSTRVAL)
call ftdtyp(fvalue,fdtype,status)
call f77upk(fdtype,sdtype,1)
dtype=sdtype[1]
end
