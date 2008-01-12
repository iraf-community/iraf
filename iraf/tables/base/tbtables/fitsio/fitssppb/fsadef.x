include "fitsio.h"

procedure fsadef(ounit,lenrow,nfield,tbcol,tform,nrows,status)

# Ascii table data DEFinition
# define the structure of the ASCII table data unit

int     ounit           # i output file pointer
int     lenrow          # o length of a table row
int     nfield          # i number of fields
int     tbcol[ARB]      # i beginning volumn
char    tform[SZ_FTFORM,ARB]      # i column datatype
%       character*16 ftform(512)
int     nrows           # i number of rows
int     status          # o error status
int	i

begin

do i=1,nfield
  call f77pak(tform(1,i) ,ftform(i),SZ_FTFORM)

call ftadef(ounit,lenrow,nfield,tbcol,ftform,nrows,status)
end
