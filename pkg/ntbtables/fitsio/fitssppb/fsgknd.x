include "fitsio.h"

procedure fsgknd(iunit,keywrd,nstart,nmax,dval,nfound,status)

# read an array of real*8 values from  header records

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nmax            # i max. number of keyword
double  dval[ARB]       # o real*8 value
int     nfound          # o no. of keywords found
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgknd(iunit,fkeywr,nstart,nmax,dval,nfound,status)
end
