#include <libelf.h>

/*
 * ELF -- Test program to access an ELF format file (executable).
 */
main (argc,argv)
int argc;
char **argv;
{
	register Elf32_Phdr *phdr;
	register Elf32_Ehdr *ehdr;
	register Elf32_Shdr *shdr;
	Elf32_Phdr *phdr_array;
	int phnum, fd, i;
	char strbuf[512];
	Elf_Scn *scn;
	Elf *elf;

	elf_version (EV_CURRENT);

	fd = open (argv[1], 2);
	if (fd < 0) {
	    printf ("cannot open file\n");
	    exit (1);
	}
	elf = elf_begin (fd, ELF_C_READ, NULL);
	if (!elf) {
	    printf ("not an ELF format file\n");
	    exit (2);
	}

	/* Read and print file header.
	 */
	ehdr = elf32_getehdr (elf);
	if (!ehdr) {
	    printf ("cannot read file header\n");
	    exit (3);
	}
	printf ("File type=%d machine=%d version=%d shnum=%d phnum=%d\n",
	    ehdr->e_type, 
	    ehdr->e_machine,
	    ehdr->e_version,
	    ehdr->e_shnum,
	    phnum = ehdr->e_phnum);
	printf ("--------------------------------------------------------\n");

	/* Read and print program header.
	 */
	phdr_array = elf32_getphdr (elf);
	if (phnum <= 0 || !phdr_array) {
	    printf ("cannot read program header\n");
	} else {
	    for (i=0;  i < phnum;  i++) {
		phdr = (Elf32_Phdr *)
		    ((char *)phdr_array + i*ehdr->e_phentsize);
		printf ("type=%d offset=%d",
		    phdr->p_type, 
		    phdr->p_offset);
		printf (" vaddr=0x%x fsize=0x%x msize=0x%x align=0x%x\n",
		    phdr->p_vaddr, 
		    phdr->p_filesz, 
		    phdr->p_memsz,
		    phdr->p_align); 
	    }
	}
	printf ("--------------------------------------------------------\n");

	/* Summarize files sections.
	 */

	/* Get section header string buffer. */
	scn = elf_getscn (elf, ehdr->e_shstrndx);
	shdr = elf32_getshdr (scn);
	if (!scn || !shdr)
	    goto nosec;
	lseek (fd, (long)shdr->sh_offset, 0);
	if (read (fd, strbuf, sizeof(strbuf)) < sizeof(strbuf)) {
nosec:	    printf ("cannot read section header\n");
	    exit (4);
	}

	/* Print section headers. */
	scn = NULL;
	while (scn = elf_nextscn(elf,scn)) {
	    shdr = elf32_getshdr (scn);
	    printf ("type=%d addr=0x%x offset=0x%x size=0x%x  %s\n",
		shdr->sh_type,
		shdr->sh_addr,
		shdr->sh_offset,
		shdr->sh_size,
		strbuf + shdr->sh_name);
	}

	elf_end (elf);
	close (fd);
}
