# D1900 -- Function to return the number of days since the turn of the
# century.

int procedure d1900 (month, day, year)

int	month, day, year		# m,d,y of date

int	mac[12]
data	mac/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334/

begin
	d1900 = 365 * year + (year - 1) / 4 + mac[month] + day
	if (month >= 3 && mod(year,4) == 0)
	    d1900 = d1900 + 1
end
