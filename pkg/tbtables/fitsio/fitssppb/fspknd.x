include "fitsio.h"

procedure fspknd(ounit,keywrd,nstart,nkey,dval,decim,comm,status)

# write an array of real*8 values to header records in E format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nkey            # i number of keywords
double  dval            # i real*8 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

# only support a single comment string for all the keywords in the SPP version
%       fcomm(48:48)='&'

call ftpknd(ounit,fkeywr,nstart,nkey,dval,decim,fcomm,status)
end
