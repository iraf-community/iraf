include "fitsio.h"

procedure fsgcnn(iunit,exact,colnam,realnm,colnum,status)

# determine the column number corresponding to an input column name.

int     iunit           # i input file pointer
bool    exact           # i require same case?
char    colnam[SZ_FTTYPE]     # i column name template
%       character fcolna*24
char    realnm[SZ_FTTYPE]     # o column name
%       character frealn*24
int     colnum          # o column number
int     status          # o error status

begin

call f77pak(colnam,fcolna,SZ_FTTYPE)
call ftgcnn(iunit,exact,fcolna,frealn,colnum,status)
call f77upk(frealn,realnm,SZ_FTTYPE)
end
