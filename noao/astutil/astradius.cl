# ASTRADIUS -- Find images within a radius.

procedure astradius (images, racenter, deccenter, epcenter, radius)

string	images = ""		{prompt="List of images"}
string	racenter = ""		{prompt="RA center (hours)"}
string	deccenter = ""		{prompt="DEC center (degrees)"}
real	epcenter = 2000.	{prompt="Epoch of center"}
real	radius = 60.		{prompt="Radius in arc seconds"}
pset	keywpars = ""		{prompt="Keywords for RA, DEC, EPOCH\n"}

file	commands = "astutil$astradius.dat"	{prompt="ASTCALC file"}

begin
	astcalc (commands=commands, images=images, table="", verbose=no)
end
