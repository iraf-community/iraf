include "fitsio.h"

procedure fsgacl(iunit,colnum,ttype,tbcol,tunit,tform,
        tscal,tzero,tnull,tdisp,status)

# Get information about an Ascii table CoLumn
# returns the parameters which define the column

int     iunit           # i input file pointer
int     colnum          # i column number
char    ttype[SZ_FTTYPE]           # o column name
int     tbcol           # o starting column position in the row
char    tunit[SZ_FTUNIT]           # o physical units of the column
char    tform[SZ_FTFORM]           # o FITS data format of the column
double  tscal           # o scaling factor
double  tzero           # o scaling zero point
char    tnull[SZ_FTNULL]      # o string used to represent null values
char    tdisp[SZ_FTFORM]      # o Fortran display format
int     status          # o error status
%       character fttype*24, ftunit*24,ftform*16,ftnull*16,ftdisp*16

begin

call ftgacl(iunit,colnum,fttype,tbcol,ftunit,ftform,
        tscal,tzero,ftnull,ftdisp,status)

call f77upk(fttype,ttype,SZ_FTTYPE)
call f77upk(ftunit,tunit,SZ_FTUNIT)
call f77upk(ftform,tform,SZ_FTFORM)
call f77upk(ftnull,tnull,SZ_FTNULL)
call f77upk(ftdisp,tdisp,SZ_FTFORM)

end
