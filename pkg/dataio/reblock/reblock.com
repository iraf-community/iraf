# input parameters
int  szb_outblock	# size of output block in bytes
int  szb_inrecord	# size of input record in bytes
int  szb_outrecord	# size of output record in bytes
int  nskip		# number  blocks (tape) or records (disk) to be skipped
int  ncopy		# number of blocks (tape) or records (disk) to be copied
int  padvalue		# integer value of padcharacter

# integer switches
int intape			# input tape device
int outtape			# output tape device	
int reblock			# reformat?
int pad_block			# pad short blocks
int pad_record			# pad records
int trim_record			# trim records
int byteswap			# swap every other byte
int wordswap			# swap every other word

common /reblock/ szb_outblock, szb_inrecord, szb_outrecord, nskip, ncopy,
		    padvalue, intape, outtape, reblock, pad_block, pad_record,
		    trim_record, byteswap, wordswap
