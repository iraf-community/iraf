include "fitsio.h"

procedure fspkne(ounit,keywrd,nstart,nkey,rval,decim,comm,status)

# write an array of real*4 values to header records in E format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nkey            # i number of keywords
real    rval            # i real*4 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

# only support a single comment string for all the keywords in the SPP version
%       fcomm(48:48)='&'

call ftpkne(ounit,fkeywr,nstart,nkey,rval,decim,fcomm,status)
end
