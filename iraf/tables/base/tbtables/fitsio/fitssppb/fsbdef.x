include "fitsio.h"

procedure fsbdef(ounit,nfield,tform,pcount,nrows,status)

# Binary table data DEFinition
# define the structure of the binary table data unit

int     ounit           # i output file pointer
int     nfield          # i number of fields
char    tform[SZ_FTFORM,ARB]      # i column datatype
%       character*16 ftform(512)
int     pcount          # i number of group parame
int     nrows           # i number of rows
int     status          # o error status
int	i

begin

do i=1,nfield
  call f77pak(tform(1,i) ,ftform(i),SZ_FTFORM)

call ftbdef(ounit,nfield,tform,pcount,nrows,status)
end
