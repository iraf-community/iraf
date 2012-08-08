include "fitsio.h"

procedure fsbnfm(tform,dtype,rcount,width,status)

# 'Binary Format'
# parse the binary table column format to determine the data
# type and the repeat count (and string width, if it is an ASCII field)

char    tform[SZ_FTFORM]       # i column format
%       character*16 ftform
int     dtype           # o datatype code
int     rcount          # o vector column repeat count
int     width           # o width of character string
int     status          # o error status

begin

call f77pak(tform  ,ftform ,SZ_FTFORM)

call ftbnfm(ftform,dtype,rcount,width,status)
end
