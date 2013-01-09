include "fitsio.h"

procedure fsgkne(iunit,keywrd,nstart,nmax,rval,nfound,status)

# read an array of real*4 values from  header records

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nmax            # i max. number of keyword
real    rval[ARB]       # o real*4 values
int     nfound          # o no. of keywords found
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgkne(iunit,fkeywr,nstart,nmax,rval,nfound,status)
end
