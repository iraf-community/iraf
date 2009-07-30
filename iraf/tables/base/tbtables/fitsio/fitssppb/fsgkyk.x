include "fitsio.h"

procedure fsgkyk(iunit,keywrd,longval,comm,status)

# read a long integer value and the comment string from a header record

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
long	longval          # o integer value
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

int	i_val

begin
	call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
	call ftgkyj(iunit,fkeywr,i_val,fcomm,status)
	longval = i_val
	call f77upk(fcomm  ,comm ,SZ_FCOMMENT)
end
