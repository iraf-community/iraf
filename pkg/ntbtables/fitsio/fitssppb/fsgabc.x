include "fitsio.h"

procedure fsgabc(nfield,tform,space,rowlen,tbcol,status)

# Get ASCII table Beginning Columns
# determine the byte offset of the beginning of each field of a
# ASCII table, and the total width of the table

int     nfield          # i number of fields
char    tform[SZ_FTFORM,ARB]      # i column datatypes
%       character*16 ftform(512)
int     space           # i no. spaces between col
int     rowlen          # o length of a table row
int     tbcol[ARB]      # o starting column positions
int     status          # o error status
int	i

begin

do i=1,nfield
  call f77pak(tform(1,i) ,ftform(i),SZ_FTFORM)

call ftgabc(nfield,ftform,space,rowlen,tbcol,status)
end
