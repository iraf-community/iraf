int	dump_records		
int	byteswap
int	byte_chunk
char	output_format

common	/mtexam/ dump_records, byteswap, byte_chunk, output_format
