include "fitsio.h"

procedure fsghbn(iunit,maxfld,nrows,nfield,ttype,tform,
                   tunit,extnam,pcount,status)

# read required standard header keywords from a binary table extension

int     iunit           # i input file pointer
int     maxfld          # i max. number of fields
int     nrows           # o number of rows
int     nfield          # o number of fields
char    ttype[SZ_FTTYPE,ARB]      # o column name
%       character*24 fttype(512)
char    tform[SZ_FTFORM,ARB]      # o column datatype
%       character*16 ftform(512)
char    tunit[SZ_FTUNIT,ARB]      # o column units
%       character*16 ftunit(512)
char    extnam
%       character fextna*48
int     pcount          # o size of 'heap'
int     status          # o error status
int	i
int     n

begin

call ftghbn(iunit,maxfld,nrows,nfield,fttype,ftform,
                   ftunit,fextna,pcount,status)
n=min(maxfld,nfield)
do i = 1, n
  { call f77upk(fttype(i) ,ttype(1,i),SZ_FTTYPE)
    call f77upk(ftform(i) ,tform(1,i),SZ_FTFORM)
    call f77upk(ftunit(i) ,tunit(1,i),SZ_FTUNIT)
  }

call f77upk(fextna ,extnam,SZ_FEXTNAME)

end
