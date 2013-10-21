
/* decl.c */
void  d_newproc (char *name, int dtype);
int   d_declaration (int dtype);
void  d_codegen (register FILE *fp);
void  d_runtime (char *text);
//void  d_makedecl (struct symbol *sp, FILE *fp);
struct symbol *d_enter (char *name, int dtype, int flags);
struct symbol *d_lookup (char *name);
void  d_chksbuf (void);
int   d_gettok (char *tokstr, int maxch);
//void  d_declfunc (struct symbol *sp, FILE *fp);


/* xppcode.c */
void  setcontext (int new_context);
void  pushcontext (int new_context);
int   popcontext (void);
void  hashtbl (void);
int   findkw (void);
void  mapident (void);
void  str_enter (void);
char *str_fetch (register char *strname);
void  macro_redef (void);
void  setline (void);
void  output (char ch);

void  do_type (int type);
void  do_char (void);
void  skip_helpblock (void);
int   parse_task_statement (void);
int   get_task (char *task_name, char *proc_name, int maxch);
int   get_name (char *outstr, int maxch);
int   nextch (void);
void  put_dictionary (void);
void  put_interpreter (void);
void  outstr (char *string);
void  begin_code (void);
void  end_code (void);
void  init_strings (void);
//void  write_string_data_statement (struct string *s);
void  do_string (char delim, int strtype);
void  do_hollerith (void);
void  sbuf_check (void);

char *str_uniqid (void);
void  traverse (char delim);
void  error (int errcode, char *errmsg);
void  xpp_warn (char *warnmsg);
long  accum (int base, char **strp);

int   charcon (char *string);
void  int_constant (char *string, int base);
void  hms (char *number);

