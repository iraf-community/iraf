include "fitsio.h"

procedure fsgcl(iunit,colnum,frow,felem,nelem,lray,status)

# read an array of logical values from a specified column of the table.
# The binary table column being read from must have datatype 'L'
# and no datatype conversion will be perform if it is not.
# This routine ignores any undefined values in the logical array.

int     iunit           # i input file pointer
int     colnum          # i column number
long	frow            # i first row
long	felem           # i first element in row
long	nelem           # i number of elements
bool    lray[ARB]       # o logical array
int     status          # o error status

int	i_frow
int	i_felem
int	i_nelem

begin
	i_frow = frow
	i_felem = felem
	i_nelem = nelem

	call ftgcl(iunit,colnum,i_frow,i_felem,i_nelem,lray,status)
end
