include "fitsio.h"

procedure fsirow(ounit,frow,nrows,status)

# insert rows in a table

int     ounit           # i output file pointer
long	frow            # insert rows after this row
long	nrows           # number of rows
int     status          # o error status

int	i_frow
int	i_nrows

begin
	i_frow = frow
	i_nrows = nrows
	call ftirow(ounit,i_frow,i_nrows,status)
end
