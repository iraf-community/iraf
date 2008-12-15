# input parameters
long szb_outblock	# size of output block in bytes
long szb_inrecord	# size of input record in bytes
long szb_outrecord	# size of output record in bytes
long nskip		# number  blocks (tape) or records (disk) to be skipped
long ncopy		# number of blocks (tape) or records (disk) to be copied
int  padvalue		# integer value of padcharacter

# integer switches
int intape			# input tape device
int outtape			# output tape device	
int reblock			# reformat?
int pad_block			# pad short blocks
int pad_record			# pad records
int trim_record			# trim records
int byteswap			# swap every other byte (2 bytes)
int wordswap			# swap every other word (4 bytes)
int longwordswap		# swap every other long word (8 bytes)

common /reblock/ szb_outblock, szb_inrecord, szb_outrecord, nskip, ncopy,
		    padvalue, intape, outtape, reblock, pad_block, pad_record,
		    trim_record, byteswap, wordswap, longwordswap
