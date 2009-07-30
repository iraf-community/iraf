include "fitsio.h"

procedure fsmkyk(ounit,keywrd,longval,comm,status)

# modify an integer value header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
long	longval          # i integer value
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

int	intval

begin
	call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
	call f77pak(comm  ,fcomm ,SZ_FCOMMENT)
	intval = longval
	call ftmkyj(ounit,fkeywr,intval,fcomm,status)
end
