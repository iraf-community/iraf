# map 6 file modes into one of five simpler modes
int	mmap[TEMP_FILE]
data	mmap[READ_ONLY]  /READ_ONLY/
data	mmap[READ_WRITE] /READ_WRITE/
data	mmap[WRITE_ONLY] /WRITE_ONLY/
data	mmap[APPEND]     /APPEND/
data	mmap[NEW_FILE]   /NEW_FILE/
data	mmap[TEMP_FILE]  /NEW_FILE/
