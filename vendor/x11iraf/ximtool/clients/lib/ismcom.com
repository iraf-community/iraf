# ISM interrupt handler variables common.
int     ismepa, ismstat, old_onint, ism_fd, ism_jmp[LEN_JUMPBUF]
common  /ismcom/ ism_fd, ism_jmp, ismepa, ismstat, old_onint

