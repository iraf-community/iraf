include	<mach.h>
include "cyber.h"

# T_LDUMPF -- list permanent files stored on a DUMPF tape.
# The permanent file name, owner id, cycle number, creation date,
# last attach and last alteration dates are listed for specified files
# of a Cyber DUMPF format tape.

procedure t_ldumpf ()

pointer	sp, dmp
char	dumpf_file[SZ_FNAME], file_list[SZ_LINE], in_fname[SZ_FNAME]
int	file_number, ranges[3, MAX_RANGES], nfiles
int	read_pf_table(), get_next_number(), decode_ranges()
int	mtfile()

begin
	# Allocate space for program data structure.
	call smark (sp)
	call salloc (dmp, LEN_DMP, TY_STRUCT)

	# Get parameters; get file_list only if dump_file is a general
	# tape name.
	call clgstr ("dumpf_file", dumpf_file, SZ_FNAME)
	if (mtfile (dumpf_file) == YES)
	    call clgstr ("file_list", file_list, SZ_LINE)
	else
	    call strcpy ("1", file_list, SZ_LINE)
	if (decode_ranges (file_list, ranges, MAX_RANGES, nfiles) == ERR)
	    call error (0, "Illegal file number list")

	# For each file in file_list call read_pf_table.
	file_number = 0
	while (get_next_number (ranges, file_number) != EOF) {
	    if (mtfile (dumpf_file) == YES)
		call mtfname (dumpf_file, file_number + 1, in_fname, SZ_FNAME)
	    else
	        call strcpy (dumpf_file, in_fname, SZ_FNAME)
	    if (read_pf_table (in_fname, file_number, dmp) == EOF) {
		call printf ("End of DUMPF tape\n")
		call sfree (sp)
		return
	    }
	}
	call sfree (sp)
end


# READ_PF_TABLE -- reads and prints out the Cyber permanent file information.

int procedure read_pf_table (dumpf_file, file_num, dmp)

char	dumpf_file[SZ_FNAME]
int	file_num
pointer	dmp
int	dump_fd, init
pointer	dump_buf, spp_buf, sp
int	mtopen(), read_cyber(), bitupk(), read_cyber_init()

begin
	# Allocate buffer space for the Permanent File Table
	call smark (sp)
	call salloc (dump_buf, SZ_PFT, TY_CHAR)
	call salloc (spp_buf, SZ_PFT, TY_CHAR)

	# Open input DUMPF tape
	dump_fd = mtopen (dumpf_file, READ_ONLY, SZ_TAPE_BUFFER)
	init = read_cyber_init()

	# Read File Header, Permanent File Catalog and Permanent File Table
	if (read_cyber (dump_fd, Memc[dump_buf], SZ_PFT) == EOF) {
	    call sfree (sp)
	    call close (dump_fd)
	    return (EOF)
	}

	# Reorder Cyber bits into packed SPP bit array
	call order_cyber_bits (Memc[dump_buf], 1, Memc[spp_buf], LEN_PFT)

	# Get number of characters in permanent file name
	NCHARS_PFN(dmp) = bitupk (Memc[spp_buf], NCHARS_OFFSET, 6)

	# Unpack file information from Permanent File Table
	call unpk_pf_info (Memc[spp_buf], dmp)

	# Print Permanent File information
	call print_pf_info (file_num, dmp)
	call flush (STDOUT)

	# Return buffer, close tape file
	call sfree (sp)
	call close (dump_fd)
	return (OK)
end


# DECIPHER_DC -- An ascii character string is decoded from an input
# bit stream.  An offset into the bit stream and the number of characters
# to unpack are input.

procedure decipher_dc (inbuf, bit_offset, nchars, outbuf)

char	inbuf[ARB], outbuf[nchars + 1]
int	offset, nchars, ip, op, temp, bit_offset
int	bitupk()

begin
	op = 1
	offset = bit_offset
	do ip = 1, nchars {
	    temp = bitupk (inbuf, offset, NBITS_DC)
	    if (temp == 55B) {
		# Blank
	        offset = offset - NBITS_DC
		next
	    }
	    else {
	        call display_code (temp, outbuf[op])
		op = op + 1
	        offset = offset - NBITS_DC
	    }
	}
	outbuf[op] = EOS
end


# UNPK_PF_INFO -- unpacks words from the Permanent File Information Table
# and fills program data structure dmp.  

procedure unpk_pf_info (inbuf, dmp)

char	inbuf[SZ_PFT]
pointer	dmp
int	creation, attach, alteration
int	bitupk()

begin	
	# Extract Permanent File Name
	call decipher_dc (inbuf, PFN_OFFSET, NCHARS_PFN(dmp), PFN(dmp))

	# Extract ID
	call decipher_dc (inbuf, PF_ID_OFFSET, SZ_PF_ID, ID(dmp))

	# Extract cycle number
	CY(dmp) = bitupk (inbuf, CY_OFFSET, NBITS_CY)
	
	# Extract creation, last_attach and last_alteration dates which are
	# written in 18-bits as "yyddd".
	creation = bitupk (inbuf, CREATE_OFFSET, NBITS_DATE)
	call ld_get_date (creation, D_CREATE(dmp), M_CREATE(dmp), Y_CREATE(dmp))

	attach = bitupk (inbuf, ATTACH_OFFSET, NBITS_DATE)
	call ld_get_date (attach, D_ATTACH(dmp), M_ATTACH(dmp), Y_ATTACH(dmp))

	alteration = bitupk (inbuf, ALTER_OFFSET, NBITS_DATE)
	call ld_get_date (alteration, D_ALTER(dmp), M_ALTER(dmp), Y_ALTER(dmp))
end


# GET_DATE -- The day, month and year are decoded from the lower 18 bits
# of the input integer.  The input format is "yyddd"; three integers are
# returned as arguments: day, month, year.

procedure ld_get_date (yyddd, day, month, year)

int	yyddd, day, month, year, ddd
int	days_in_month[12], i, bitupk()

data 	(days_in_month[i], i=1,9) /31, 28, 31, 30, 31, 30, 31, 31, 30/
data 	(days_in_month[i], i=10, 12) /31, 30, 31/

begin	
	year = bitupk (yyddd, 10, 9) + 1900
	ddd = bitupk (yyddd, 1, 9)

	# Leap year tests
	if (mod (year, 4) == 0)
	    days_in_month[2] = 29
	if (mod (year, 100) == 0)
	    days_in_month[2] = 28
	if (mod (year, 400) == 0)
	    days_in_month[2] = 29

	for (i=1; i<=12; i=i+1) {
	    if (ddd <= days_in_month[i])
		break
	    else
	        ddd = ddd - days_in_month[i]
	}

	month = i 
	day = ddd
end


# PRINT_PF_INFO -- information from the permanent file table is printed.

procedure print_pf_info (file_num, dmp)

pointer	dmp
int	file_num

begin
	call printf ("\n[%d]%6tPFN= %s, ID= %s, CY= %d\n")
	    call pargi (file_num)
	    call pargstr (PFN(dmp))
	    call pargstr (ID(dmp))
	    call pargi (CY(dmp))
	call printf ("%6tCreation: %d/%d/%d, Last Alteration: %d/%d/%d, ")
	    call pargi (M_CREATE(dmp))
	    call pargi (D_CREATE(dmp))
	    call pargi (Y_CREATE(dmp))
	    call pargi (M_ALTER(dmp))
	    call pargi (D_ALTER(dmp))
	    call pargi (Y_ALTER(dmp))
	call printf ("Last Attach: %d/%d/%d\n")
	    call pargi (M_ATTACH(dmp))
	    call pargi (D_ATTACH(dmp))
	    call pargi (Y_ATTACH(dmp))
end
