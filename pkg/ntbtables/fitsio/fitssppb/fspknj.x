include "fitsio.h"

procedure fspknj(ounit,keywrd,nstart,nkey,intval,comm,status)

# write an array of integer values to header records

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nkey            # i number of keywords
int     intval          # i integer value
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

# only support a single comment string for all the keywords in the SPP version
%       fcomm(48:48)='&'

call ftpknj(ounit,fkeywr,nstart,nkey,intval,fcomm,status)
end
