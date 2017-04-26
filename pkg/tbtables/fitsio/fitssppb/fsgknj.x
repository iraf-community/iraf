include "fitsio.h"

procedure fsgknj(iunit,keywrd,nstart,nmax,intval,nfound,status)

# read an array of integer values from  header records

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nmax            # i max. number of keyword
int     intval[ARB]     # o integer values
int     nfound          # o no. of keywords found
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgknj(iunit,fkeywr,nstart,nmax,intval,nfound,status)
end
