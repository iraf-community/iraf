include	<error.h>

# T_ASTTIMES -- Print and record astronomical times for the given date.

procedure t_asttimes ()

int	list			# List of files
int	header			# Print header?
int	year			# Year
int	month			# Month
int	day			# Day
double	zt			# Zone time
int	zone			# Time zone from Greenwich
double	longitude		# Longitude for LMST

double	ut			# Universal time (output)
double	epoch			# Epoch (output)
double	jd			# Julian day (output)
double	lmst			# Local mean siderial time (output)

int	fd
char	file[SZ_FNAME]

int	clpopnu(), clplen(), clgfil(), clgeti(), btoi()
int	open(), fscan(), nscan()
bool	clgetb()
double	clgetd()

begin
	# Get parameters other than date.
	list = clpopnu ("files")
	header = btoi (clgetb ("header"))
	zone = clgeti ("zone")
	longitude = clgetd ("longitude")

	# If no files are given then get dates from the CL.
	if (clplen (list) == 0) {
	    # Get and print times.
	    year = clgeti ("year")
	    month = clgeti ("month")
	    day = clgeti ("day")
	    zt = clgetd ("time")

	    call times (year, month, day, zt, zone, longitude, ut, epoch, jd,
		lmst, header)
	
	    # Record results in the parameter file.
	    call clputd ("ut", ut)
	    call clputd ("epoch", epoch)
	    call clputd ("jd", jd)
	    call clputd ("lmst", lmst)

	} else {
	    # Scan each file in the list.
	    while (clgfil (list, file, SZ_FNAME) != EOF) {
		iferr (fd = open (file, READ_ONLY, TEXT_FILE)) {
		    call erract (EA_WARN)
		    next
		}

		# Get and print times.
		while (fscan (fd) != EOF) {
		    call gargi (year)
		    call gargi (month)
		    call gargi (day)
		    call gargd (zt)
		    if (nscan() < 4)
			next

	    	    call times (year, month, day, zt, zone, longitude, ut,
			epoch, jd, lmst, header)
		}

		call close (fd)
	    }
	    call clpcls (list)
	}

end


# TIMES -- Print times.

procedure times (year, month, day, zt, zone, longitude, ut, epoch, jd, lmst,
	header)

int	year		# Year
int	month		# Month (1-12)
int	day		# Day of month
double	zt		# Zone time
int	zone		# Time zone
double	longitude	# Longitude
double	ut		# UT
double	epoch		# Epoch in 365.25 solar mean days
double	jd		# Julian date
double	lmst		# Mean Sidereal Time
int	header		# Print header?

char	dow[3]
int	d

double	ast_julday(), ast_mst()

begin
	# Determine day of the week in zone time.
	call ast_date_to_epoch (year, month, day, zt, epoch)
	jd = ast_julday (epoch)
	call ast_day_of_week (jd, d, dow, 3)

	# Determine UT, EPOCH, JD, and MST.
	ut = zt + zone
	call ast_date_to_epoch (year, month, day, ut, epoch)
	jd = ast_julday (epoch)
	lmst = ast_mst (epoch, longitude)

	# Print Times.
	if (header == YES) {
	    call printf ("##%2s %3s %6s %10s %10s %10s %12s %10s\n")
		call pargstr ("YR")
		call pargstr ("MON")
		call pargstr ("  DAY ")
		call pargstr ("ZT")
		call pargstr ("UT")
		call pargstr ("EPOCH")
		call pargstr ("JD")
		call pargstr ("LMST")
	    header = NO
	}
	call printf ("%4d %3d %2d %3s %10h %10h %10.5f %12.4f %10h\n")
	    call pargi (year)
	    call pargi (month)
	    call pargi (day)
	    call pargstr (dow)
	    call pargd (zt)
	    call pargd (ut)
	    call pargd (epoch)
	    call pargd (jd)
	    call pargd (lmst)
end
