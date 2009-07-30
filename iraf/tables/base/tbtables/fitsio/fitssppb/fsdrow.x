include "fitsio.h"

procedure fsdrow(ounit,frow,nrows,status)

# delete rows in a table

int     ounit           # i output file pointer
long	frow            # first row to delete
long	nrows           # number of rows
int     status          # o error status

int	i_frow
int	i_nrows

begin
	i_frow = frow
	i_nrows = nrows
	call ftdrow(ounit,i_frow,i_nrows,status)
end
