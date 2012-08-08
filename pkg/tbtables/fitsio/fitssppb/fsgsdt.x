include "fitsio.h"

procedure fsgsdt(dd,mm,yy,status)

# get the current date

int	dd		#O day of the month (1-31)
int	mm		#O month of the year (1-12)
int	yy		#O last 2 digits of the year (1992 = 92, 2001 = 01)
int     status          # o error status

begin
call ftgsdt (dd, mm, yy, status)
end
