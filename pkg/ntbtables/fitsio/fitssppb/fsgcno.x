include "fitsio.h"

procedure fsgcno(iunit,exact,colnam,colnum,status)

# determine the column number corresponding to an input column name.
# this assumes that the first 16 characters uniquely define the name

int     iunit           # i input file pointer
bool    exact           # i require same case?
char    colnam[SZ_FTTYPE]     # column name
%       character fcolna*24
int     colnum          # o column number
int     status          # o error status

begin

call f77pak(colnam,fcolna,SZ_FTTYPE)

call ftgcno(iunit,exact,fcolna,colnum,status)
end
