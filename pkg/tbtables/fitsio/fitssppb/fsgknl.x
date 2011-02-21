include "fitsio.h"

procedure fsgknl(iunit,keywrd,nstart,nmax,logval,nfound,status)

# read an array of logical values from  header records

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nmax            # i max. number of keyword
bool    logval[ARB]     # o logical values
int     nfound          # o no. of keywords found
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgknl(iunit,fkeywr,nstart,nmax,logval,nfound,status)
end
