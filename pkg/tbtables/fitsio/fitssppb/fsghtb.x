include "fitsio.h"

procedure fsghtb(iunit,maxfld,ncols,nrows,nfield,ttype,
                    tbcol,tform,tunit,extnam,status)

# read required standard header keywords from an ASCII table extension

int     iunit           # i input file pointer
int     maxfld          # i max. number of fields to return
int     ncols           # o number of columns
int     nrows           # o number of rows
int     nfield          # o number of fields
char    ttype[SZ_FTTYPE,ARB]      # o column name
%       character*24 fttype(512)
int     tbcol[ARB]      # o starting column position
char    tform[SZ_FTFORM,ARB]      # i column data format
%       character*16 ftform(512)
char    tunit[SZ_FTUNIT,ARB]      # i column units
%       character*24 ftunit(512)
char    extnam[SZ_FEXTNAME]     # i extension name
%       character fextna*24
int     status          # o error status
int	i
int	n

begin

call ftghtb(iunit,maxfld,ncols,nrows,nfield,fttype,
                    tbcol,ftform,ftunit,fextna,status)

n=min(maxfld,nfield)
do i = 1, n
  { call f77upk(fttype(i) ,ttype(1,i),SZ_FTTYPE)
    call f77upk(ftform(i) ,tform(1,i),SZ_FTFORM)
    call f77upk(ftunit(i) ,tunit(1,i),SZ_FTUNIT)
  }

call f77upk(fextna ,extnam,SZ_FEXTNAME)

end
