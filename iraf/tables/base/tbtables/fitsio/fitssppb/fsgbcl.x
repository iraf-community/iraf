include "fitsio.h"

procedure fsgbcl(iunit,colnum,ttype,tunit,dtype,rcount,
        tscal,tzero,tnull,tdisp,status)

# Get information about a Binary table CoLumn
# returns the parameters which define the column

int     iunit           # i input file pointer
int     colnum          # i column number
char    ttype[SZ_FTTYPE]           # o column name
char    tunit[SZ_FTUNIT]           # o physical units of the column
int     dtype           # o datatype code
int     rcount          # o repeat count for vector column
double  tscal           # o scaling factor
double  tzero           # o scaling zero point
int     tnull           # o integer used to represent null values
char    tdisp[SZ_FTFORM]      # o Fortran display format
int     status          # o error status
%       character fttype*24, ftunit*24,ftdisp*16

begin

call ftgbcl(iunit,colnum,fttype,ftunit,dtype,rcount,
        tscal,tzero,tnull,ftdisp,status)

call f77upk(fttype,ttype,SZ_FTTYPE)
call f77upk(ftunit,tunit,SZ_FTUNIT)
call f77upk(ftdisp,tdisp,SZ_FTFORM)

end
