include "fitsio.h"

procedure fsphtb(ounit,ncols,nrows,nfield,ttype,tbcol,
                 tform,tunit,extnam,status)

# write required standard header keywords for an ASCII table extension

int     ounit           # i output file pointer
int     ncols           # i number of columns
int     nrows           # i number of rows
int     nfield          # i number of fields
char    ttype[SZ_FTTYPE,ARB]      # i column name
%       character*24 fttype(512)
int     tbcol[ARB]      # i starting column position
char    tform[SZ_FTFORM,ARB]      # i column data format
%       character*16 ftform(512)
char    tunit[SZ_FTUNIT,ARB]      # i column units
%       character*24 ftunit(512)
char    extnam[SZ_FEXTNAME]     # i extension name
%       character fextna*24
int     status          # o error status
int	i

begin

do i = 1, nfield
  { call f77pak(ttype(1,i) ,fttype(i),SZ_FTTYPE)
    call f77pak(tform(1,i) ,ftform(i),SZ_FTFORM)
    call f77pak(tunit(1,i) ,ftunit(i),SZ_FTUNIT)
  }

call f77pak(extnam ,fextna,SZ_FEXTNAME)

call ftphtb(ounit,ncols,nrows,nfield,fttype,tbcol,
                 ftform,ftunit,fextna,status)
end
