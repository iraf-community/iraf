include "fitsio.h"

procedure fsibin(ounit,nrows,nfield,ttype,tform,tunit,
                    extnam,pcount,status)

# insert a binary table extension

int     ounit           # i output file pointer
int     nrows           # i number of rows
int     nfield          # i number of fields
char    ttype[SZ_FTTYPE,ARB]      # i column name
%       character*24 fttype(512)
char    tform[SZ_FTFORM,ARB]      # i column data format
%       character*16 ftform(512)
char    tunit[SZ_FTUNIT,ARB]      # i column units
%       character*24 ftunit(512)
char    extnam[SZ_FEXTNAME]     # i extension name
%       character fextna*24
int     pcount          # i size of 'heap'
int     status          # o error status
int	i

begin

do i = 1, nfield
  { call f77pak(ttype(1,i) ,fttype(i),SZ_FTTYPE)
    call f77pak(tform(1,i) ,ftform(i),SZ_FTFORM)
    call f77pak(tunit(1,i) ,ftunit(i),SZ_FTUNIT)
  }

call f77pak(extnam ,fextna,SZ_FEXTNAME)

call ftibin(ounit,nrows,nfield,fttype,ftform,ftunit,
                    fextna,pcount,status)
end
