#define	isdigit(c)	((c)>='0'&&(c)<='9')
#define	isxdigit(c)	(isdigit(c)||(c)>='a'&&(c)<='f'||(c)>='A'&&(c)<='F')
#define	islower(c)	((c)>='a'&&(c)<='z')
#define	isspace(c)	((c)==' '||(c)=='\t'||(c)=='\n')
