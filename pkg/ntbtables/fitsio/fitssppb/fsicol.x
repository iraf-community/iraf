include "fitsio.h"

procedure fsicol(ounit,colnum,ttype,tform,status)

# insert column in a table

int     ounit           # i output file pointer
int     colnum          # i column to be inserted
char    ttype[SZ_FTTYPE]      # i column name
%       character*24 ftype
char    tform[SZ_FTFORM]      # i column data format
%       character*16 fform
int     status          # o error status

begin

call f77pak(ttype ,ftype,SZ_FTTYPE)
call f77pak(tform ,fform,SZ_FTFORM)

call fticol(ounit,colnum,ftype,fform,status)
end
