# PIXBIT -- Look up which bits should be set for this character on this line.

procedure pixbit (code, line, bitarray)

int	code		# character we are writing
int	line		# line of the character we are writing
int	bitarray[5]	# bit-array to receive data

int	pix, i
short	asciilook[128]
short	font[455]
int	bitupk()
include	"pixelfont.inc"
include	"asciilook.inc"

begin
	pix = font[asciilook[code+1]+line-1]
	bitarray[5] = bitupk (pix, 1, 1)
	bitarray[4] = bitupk (pix, 4, 1)
	bitarray[3] = bitupk (pix, 7, 1)
	bitarray[2] = bitupk (pix, 10, 1)
	bitarray[1] = bitupk (pix, 13, 1)
end
