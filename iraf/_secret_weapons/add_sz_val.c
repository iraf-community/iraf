/* gcc -O -Wall add_sz_val.c -o add_sz_val */
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define DEBUG 0

#define SZ_LINE_BUF 65536
#define SZ_NUM_PROC 1024

static int add_sz_val( const char *, int, const char *, const char *,
		       const char * );

int main( int argc, char *argv[] )
{
    int return_status = -1;
    const char *proc_name;
    const char *arg_type;
    const char *arg_val = NULL;
    int target_arg;
    int i;

    if ( argc < 4 ) {
	fprintf(stderr,"[USAGE]\n");
	fprintf(stderr,"%s proc_name arg_index arg_type foo.x foo1.x ...\n",argv[0]);
	fprintf(stderr,"(example)\n");
	fprintf(stderr,"%s salloc 1 size_t foo.x foo1.x ...\n",argv[0]);
	goto quit;
    }

    i=1;
    proc_name = argv[i++];
    target_arg = atoi(argv[i++]);
    arg_type = argv[i++];

    if ( strcmp(arg_type,"size_t") == 0 ) {
	arg_val = "sz_val";
    }
    else if ( strcmp(arg_type,"long") == 0 ) {
	arg_val = "lg_val";
    }
    else {
	fprintf(stderr,"[ERROR] invalid arg_type: %s\n",arg_type);
	goto quit;
    }

    for ( ; i < argc ; i++ ) {
	int status;
	status = add_sz_val(proc_name,target_arg,arg_type,arg_val,
			    argv[i]);
	if ( status != 0 ) {
	    fprintf(stderr,"[ERROR] add_sz_val() failed: file = %s\n",argv[i]);
	    goto quit;
	}
    }

    return_status = 0;
 quit:
    return return_status;
}

static int is_valchar( int ch )
{
    return ( isalpha(ch) || isdigit(ch) || ch=='_' );
}

static int parse_in_braces ( const char *char_ptr_in_brace, char *lines[], 
			     int current_line_idx, int max_line_idx,
			     const char *end_chars, const char **end_ptr )
{
    const char *ip;
    int br_in_0 = 0;	/* [] */
    int br_in_1 = 0;	/* () */
    bool qt_in_0 = false;	/* "" */
    bool qt_in_1 = false;	/* '' */
    bool flg_complete_line = false;
    int j = current_line_idx;
    for ( ip=char_ptr_in_brace ; j <= max_line_idx ; ip=lines[++j] ) {
	while ( *ip != '\n' && *ip != '\0' ) {
	    if ( qt_in_0 == true && *ip==0x5c )
		ip++;
	    else if ( qt_in_1 == true && *ip==0x5c )
		ip++;
	    else if ( qt_in_0 == true && *ip=='"' )
		qt_in_0=false;
	    else if ( qt_in_1 == true && *ip==0x27 )
		qt_in_1=false;
	    else {
		if ( qt_in_0 == false && qt_in_1 == false ) {
		    if ( *ip == '#' ) break;
		    else if ( *ip == '"' ) qt_in_0=true;
		    else if ( *ip == 0x27 ) qt_in_1=true;
		    else if ( strchr(end_chars,*ip)!=NULL &&
			      br_in_0 == 0 && br_in_1 == 0 ) {
			flg_complete_line = true;
			break;
		    }
		    else if ( *ip == '[' ) br_in_0++;
		    else if ( *ip == '(' ) br_in_1++;
		    else if ( *ip == ']' ) br_in_0--;
		    else if ( *ip == ')' ) br_in_1--;
		}
	    }
	    ip++;
	}
	if ( flg_complete_line == true ) break;
    }

    if ( end_ptr != NULL ) *end_ptr = ip;

    if ( flg_complete_line == true ) return j;
    else return -1;
}

#define FLG_IF_WITHOUT_BRACE 1

static int add_sz_val( const char *proc_name, int target_arg, 
		       const char *ap_type, const char *ap_val,
		       const char *file_name )
{
    int return_status = -1;
    char line_buf[SZ_LINE_BUF];
    FILE *fp = NULL;
    int num_lines = 0;
    char **lines = NULL;
    int *flags = NULL;
    int num_proc;
    bool needs_update = false;
    /* line indices */
    int proc_decl[SZ_NUM_PROC];
    int proc_begin[SZ_NUM_PROC];
    int proc_end[SZ_NUM_PROC];
    int insert_idx[SZ_NUM_PROC];	/* index to insert `size_t sz_val' */
    /* */
    int i;

    /* read all lines */
    fp = fopen(file_name,"r");
    if ( fp == NULL ) {
	fprintf(stderr,"[ERROR] file not found: %s\n",file_name);
	goto quit;
    }

    while ( fgets(line_buf,SZ_LINE_BUF,fp) != NULL ) {
	void *tmp_ptr;
	tmp_ptr = realloc(lines,sizeof(char **)*(num_lines+1));
	if ( tmp_ptr == NULL ) {
	    fprintf(stderr,"[ERROR] realloc() failed\n");
	    goto quit;
	}
	lines = tmp_ptr;
	tmp_ptr = realloc(flags,sizeof(int)*(num_lines+1));
	if ( tmp_ptr == NULL ) {
	    fprintf(stderr,"[ERROR] realloc() failed\n");
	    goto quit;
	}
	flags = tmp_ptr;
	lines[num_lines] = strdup(line_buf);
	flags[num_lines] = 0;
	if ( lines[num_lines] == NULL ) {
	    fprintf(stderr,"[ERROR] strdup() failed\n");
	    goto quit;
	}
	num_lines++;
    }
    fclose(fp);
    fp = NULL;

    for ( i=0 ; i < SZ_NUM_PROC ; i++ ) {
	proc_decl[i] = -1;
	proc_begin[i] = -1;
	proc_end[i] = -1;
	insert_idx[i] = -1;
    }

    /* detect `procedure' `begin' and `end' */
    num_proc = 0;
    for ( i=0 ; i < num_lines ; i++ ) {
	const char *ip;
	ip = lines[i];
	while ( *ip == ' ' || *ip == '\t' ) ip++;
	if ( strncmp(ip,"procedure",9) == 0 &&
	     (ip[9] == ' ' || ip[9] == '\t') && is_valchar(ip[10]) ) {
	    proc_decl[num_proc] = i;
	}
	else if ( strncmp(ip,"begin",5) == 0 &&
	     (ip[5] == ' ' || ip[5] == '\t' || ip[5] == '#' || ip[5] == '\n') ) {
	    proc_begin[num_proc] = i;
	}
	else if ( strncmp(ip,"end",3) == 0 &&
	     (ip[3] == ' ' || ip[3] == '\t' || ip[3] == '#' || ip[3] == '\n') ) {
	    proc_end[num_proc] = i;
	    if ( DEBUG ) {
		fprintf(stderr,"debug: decl=%d  begin=%d  end=%d\n",
	          proc_decl[num_proc],proc_begin[num_proc],proc_end[num_proc]);
	    }
	    if ( 0 <= proc_decl[num_proc] &&
		 proc_decl[num_proc] < proc_begin[num_proc] &&
		 proc_begin[num_proc] < proc_end[num_proc] ) {
		num_proc++;
	    }
	}
	else {
	    while ( is_valchar(*ip) ) ip++;
	    while ( *ip == ' ' || *ip == '\t' ) ip++;
	    if ( strncmp(ip,"procedure",9) == 0 &&
		 (ip[9] == ' ' || ip[9] == '\t') && is_valchar(ip[10]) ) {
		proc_decl[num_proc] = i;
	    }
	}
	if ( SZ_NUM_PROC <= num_proc ) {
	    fprintf(stderr,"[Warning] Max num_proc\n");
	    break;
	}
    }

    if ( DEBUG ) {
	fprintf(stderr,"debug: decide insert_idx\n");
    }

    /* decide insert_idx */
    for ( i=0 ; i < num_proc ; i++ ) {
	int j, j0;
	int last_decl = -1;
	int fix_last_decl = 0;
	const char *comm_ptr;

	/* for long procedure declarations */
	j0 = proc_decl[i];
	while ( j0 < proc_begin[i] ) {
	    comm_ptr = strrchr(lines[j0],',');
	    if ( comm_ptr != NULL ) {
		comm_ptr++;
		while ( *comm_ptr==' ' || *comm_ptr=='\t' )
		    comm_ptr++;
		if ( is_valchar(*comm_ptr) || *comm_ptr == '$' ) break;
		else if ( *comm_ptr!='#' && *comm_ptr!='\n' && *comm_ptr!='\0' ) {
		    fprintf(stderr,"[ERROR] file = %s  line = %d: Unexpected syntax\n",
			    file_name,j0+1);
		    goto quit;
		}
	    }
	    else break;
	    j0++;
	}
	j0++;

	/* skip `$endif' or `$else' in foo.gx  */
	while ( j0 < proc_begin[i] ) {
	    const char *ip;
	    ip = lines[j0];
	    while ( *ip==' ' || *ip=='\t' ) ip++;
	    if ( *ip != '$' && *ip != '#' && 
		 *ip != '\n' && *ip != '\0' ) break;
	    j0++;
	}

	for ( j=j0 ; j < proc_begin[i] ; j++ ) {
	    const char *ip;
	    bool contain_alpha = false;
	    ip = lines[j];
	    while ( is_valchar(*ip) || *ip==' ' || *ip=='\t'||
		    *ip==',' || *ip=='[' || *ip==']' ) {
		if ( is_valchar(*ip) ) contain_alpha = true;
		ip++;
	    }
	    if ( contain_alpha == true ) {
		if ( fix_last_decl < 2 ) {
		    last_decl = j;
		    if ( fix_last_decl == 1 ) fix_last_decl++;
		}
	    }
	    else {
		if ( 0 <= last_decl && fix_last_decl == 0 ) fix_last_decl = 1;
	    }
	    if ( *ip != '#' && *ip != '\n' ) break;
	}
	if ( last_decl < 0 ) insert_idx[i] = proc_decl[i] + 1;
	else insert_idx[i] = last_decl;
	if ( DEBUG ) {
	    fprintf(stderr,"debug: insert_idx[i]=%d\n",insert_idx[i]);
	}
	/* check sz_val decl. */
	for ( j=j0 ; j < proc_begin[i] ; j++ ) {
	    const char *ip;
	    ip = strchr(lines[j],'\t');
	    if ( ip != NULL && strncmp(ip+1,ap_val,strlen(ap_val))==0 ) {
		ip++;
		if ( is_valchar(ip[strlen(ap_val)]) == 0 ) {
		    /* already exists */
		    insert_idx[i] = -1;
		}
	    }
	}
    }

    if ( DEBUG ) {
	fprintf(stderr,"debug: modify: call ...\n");
    }

    /* modify: call salloc(a,b,c) -> sz_val = b;  call salloc(a,sz_val,c) */
    for ( i=0 ; i < num_proc ; i++ ) {
	char prev_arg_str[SZ_LINE_BUF] = {'\0'};
	int  prev_line_idx = -1;
	int j, j_not_gc;
	bool target_found = false;
	j_not_gc = -1;

	/* check if,else part */
	for ( j=proc_begin[i]+1 ; j < proc_end[i] ; j++ ) {
	    const char *ip;
	    const char *ip1;
	    ip = lines[j];
	    while ( *ip == ' ' || *ip == '\t' ) ip++;
	    if ( *ip == '#' || *ip == '\n' || *ip == '$' ) continue;
	    if ( *ip == '\0' ) continue;
	    /* */
	    for ( ip1=ip ; ; ) {
		const char *ip2 = NULL;
		while ( isalpha(*ip1)==0 && *ip1!='\0' ) ip1++;
		if ( (strncmp(ip1,"if",2)==0 && is_valchar(ip1[2])==0) ) ip2=ip1+2;
		if ( (strncmp(ip1,"else",4)==0 && is_valchar(ip1[4])==0) ) ip2=ip1+4;
		if ( (strncmp(ip1,"while",5)==0 && is_valchar(ip1[5])==0) ) ip2=ip1+5;
		if ( (strncmp(ip1,"do",2)==0 && is_valchar(ip1[2])==0) ) ip2=ip1+2;
		if ( (strncmp(ip1,"for",3)==0 && is_valchar(ip1[3])==0) ) ip2=ip1+3;
		if ( (strncmp(ip1,"repeat",6)==0 && is_valchar(ip1[6])==0) ) ip2=ip1+6;
		if ( (strncmp(ip1,"iferr",5)==0 && is_valchar(ip1[5])==0) ) ip2=ip1+5;
		if ( (strncmp(ip1,"ifnoerr",7)==0 && is_valchar(ip1[7])==0) ) ip2=ip1+7;
		if ( ip2 == NULL ) {
		    break;
		}
		else {
		    while ( *ip2 == ' ' || *ip2 == '\t' ) ip2++;
		    if ( *ip2 == '#' || *ip2 == '\n'|| *ip2 == '\0' ) {
			flags[j] |= FLG_IF_WITHOUT_BRACE;
			break;
		    }
		    else if ( *ip2 == '{' ) {
			break;
		    }
		    else if ( *ip2 == '(' ) {	/* brace begins... */
			int o_j = j;
			const char *ip3;
			j = parse_in_braces ( ip2+1, lines, j, proc_end[i]-1, ")", &ip3 );
			if ( j < 0 ) {
			    fprintf(stderr,"[ERROR] file = %s  line = %d: Cannot parse\n",
				    file_name,o_j+1);
			    goto quit;
			}
			ip3++;
			while ( *ip3 == ' ' || *ip3 == '\t' ) ip3++;
			if ( *ip3 != '{' ) {
			    if ( *ip3 == '#' || *ip3 == '\n' || *ip3 == '\0' ) {
				flags[j] |= FLG_IF_WITHOUT_BRACE;
			    }
			    //else {
			    //	printf("[INFO] file = %s  line = %d: Unexpected syntax\n",
			    //	       file_name,o_j+1);
			    //}
			}
			break;
		    }
		}
		ip1 = ip2;
	    }
	}

	/* */
	for ( j=proc_begin[i]+1 ; j < proc_end[i] ; j++ ) {
	    const char *ip;
	    const char *ptr_call_begin;
	    const char *ptr_proc_begin;
	    const char *end_ptr;
	    int arg_cnt = 0;
	    bool flg_prev_if_without_brace;
	    int prev_j, end_j;
	    if ( DEBUG ) {
		fprintf(stderr,"debug: j = %d\n",j);
	    }
	    ip = lines[j];
	    while ( *ip == ' ' || *ip == '\t' ) ip++;
	    if ( *ip == '#' || *ip == '\n' || *ip == '$' ) continue;
	    if ( *ip == '\0' ) continue;
	    /* previous valid line */
	    prev_j = j_not_gc;
	    /* */
	    if ( 0 <= prev_j && (flags[prev_j] & FLG_IF_WITHOUT_BRACE) )
		flg_prev_if_without_brace = true;
	    else flg_prev_if_without_brace = false;
	    /* Register valid line */
	    j_not_gc = j;
	    /* */
	    if ( strncmp(ip,"call ",5) != 0 && strncmp(ip,"call\t",5) != 0 ) {
		continue;
	    }
	    ptr_call_begin = ip;
	    ip += 5;
	    while ( *ip == ' ' || *ip == '\t' ) ip++;
	    if ( strncmp(ip,proc_name,strlen(proc_name)) != 0 ) continue;
	    ptr_proc_begin = ip;
	    ip += strlen(proc_name);
	    if ( *ip != ' ' && *ip != '\t' && *ip != '(' ) continue;
	    while ( *ip == ' ' || *ip == '\t' ) ip++;
	    if ( *ip != '(' ) continue;
	    /* parse (....) */
	    end_j = parse_in_braces (ip+1, lines, j, proc_end[i]-1, ")", &end_ptr);
	    if ( end_j < 0 ) {
		fprintf(stderr,"[ERROR] file = %s  line = %d: Syntax Error??\n",
			file_name,j+1);
		goto quit;
	    }
	    end_ptr++;
	    while ( *end_ptr == ' ' || *end_ptr == '\t' ) end_ptr++;
	    if ( *end_ptr != '#' && *end_ptr != '\n' && *end_ptr != '\0' &&
		 *end_ptr != ';' ) {
		fprintf(stderr,"[ERROR] file = %s  line = %d: Unexpected syntax\n",
			file_name,j+1);
		goto quit;
	    }
	    /* merge lines */
	    if ( j < end_j ) {
		char tmp_buf[SZ_LINE_BUF];
		const char *ip1;
		int k;
		char *old_ptr = lines[j];
		snprintf(tmp_buf,SZ_LINE_BUF,"%s",lines[j]);
		for ( k=j+1 ; k <= end_j ; k++ ) {
		    char tmp_buf1[SZ_LINE_BUF];
		    char *op;
		    op = strrchr(tmp_buf,'\n');
		    if ( op != NULL ) *op = ' ';
		    ip1 = lines[k];
		    while ( *ip1 == ' ' || *ip1 == '\t' ) ip1++;
		    strcpy(tmp_buf1,tmp_buf);
		    snprintf(tmp_buf,SZ_LINE_BUF,"%s%s",
			     tmp_buf1,ip1);
		    lines[k][0] = '\0';
		}
		free(lines[j]);
		lines[j] = strdup(tmp_buf);
		if ( lines[j] == NULL ) {
		    fprintf(stderr,"[ERROR] strdup() failed\n");
		    goto quit;
		}
		ip += lines[j] - old_ptr;
		ptr_call_begin += lines[j] - old_ptr;
		ptr_proc_begin += lines[j] - old_ptr;
		/* */
		printf("[INFO] file = %s  concatenated lines: %d - %d\n",
		       file_name,j+1,end_j+1);
		fflush(stdout);
	    }
	    /* */
	    if ( flg_prev_if_without_brace == true ) {
		char tmp_buf[SZ_LINE_BUF];
		const char *ip1;
		char *op1;
		char *maxop1;
		int next_j;
		printf("[INFO] file = %s  line = %d: appended braces\n",
		       file_name,j+1);
		fflush(stdout);
		if ( prev_j < 0 ) {
		    fprintf(stderr,"[ERROR] Invalid prev_j\n");
		    goto quit;
		}
		op1 = strchr(lines[prev_j],'\n');
		if ( op1 != NULL ) *op1 = ' ';
		snprintf(tmp_buf,SZ_LINE_BUF,"%s{\n",lines[prev_j]);
		free(lines[prev_j]);
		lines[prev_j] = strdup(tmp_buf);
		if ( lines[prev_j] == NULL ) {
		    fprintf(stderr,"[ERROR] strdup() failed\n");
		    goto quit;
		}
		/* */
		for ( next_j = j+1 ; next_j < proc_end[i] ; next_j++ ) {
		    ip1 = lines[next_j];
		    while ( *ip1 == ' ' || *ip1 == '\t' ) ip1++;
		    if ( *ip1 != '#' && *ip1 != '\n' && 
			 *ip1 != '$' && *ip1 != '\0' ) {
			break;
		    }
		}
		if ( next_j == proc_end[i] ) next_j = j+1;
		if ( next_j != j+1 ) {
		    printf("[INFO] file = %s  line = %d: found blank line\n",
			   file_name,j+1);
		    fflush(stdout);
		}
		op1 = tmp_buf;
		maxop1 = tmp_buf + SZ_LINE_BUF -1;
		ip1 = lines[next_j];
		while ( *ip1 == ' ' || *ip1 == '\t' ) {
		    if ( op1 < maxop1 ) {
			*op1 = *ip1;
			op1++;
		    }
		    ip1++;
		}
		/* */
		if ( strncmp("else",ip1,4)==0 && is_valchar(ip1[4])==0 ) {
		    const char *ip2 = "} ";
		    while ( *ip2 != '\0' ) {
			if ( op1 < maxop1 ) {
			    *op1 = *ip2;
			    op1++;
			}
			ip2++;
		    }
		    ip2 = ip1;
		    while ( *ip2 != '\0' ) {
			if ( op1 < maxop1 ) {
			    *op1 = *ip2;
			    op1++;
			}
			ip2++;
		    }
		    *op1 = '\0';
		    free(lines[next_j]);
		    lines[next_j] = strdup(tmp_buf);
		    if ( lines[next_j] == NULL ) {
			fprintf(stderr,"[ERROR] strdup() failed\n");
			goto quit;
		    }
		}
		else {
		    char tmp_buf1[SZ_LINE_BUF];
		    const char *ip2;
		    op1 = tmp_buf;
		    ip2 = lines[prev_j];
		    while ( *ip2 == ' ' || *ip2 == '\t' ) {
			if ( op1 < maxop1 ) {
			    *op1 = *ip2;
			    op1++;
			}
			ip2++;
		    }
		    ip2 = "}\n";
		    while ( *ip2 != '\0' ) {
			if ( op1 < maxop1 ) {
			    *op1 = *ip2;
			    op1++;
			}
			ip2++;
		    }
		    *op1 = '\0';
		    snprintf(tmp_buf1,SZ_LINE_BUF,"%s%s",
			     lines[j],tmp_buf);
		    free(lines[j]);
		    lines[j] = strdup(tmp_buf1);
		    if ( lines[j] == NULL ) {
			fprintf(stderr,"[ERROR] strdup() failed\n");
			goto quit;
		    }
		    j--;	/* try again */
		    continue;
		}
	    }
	    /* 1st arg begins... */
	    if ( DEBUG ) {
		fprintf(stderr,"debug: 1st arg begins...\n");
	    }
	    do {
		const char *next_arg = NULL;
		const char *current_arg = NULL;
		const char *current_arg_str = NULL;
		ip++;			/* skip '(', or ',' */
		current_arg = ip;
		current_arg_str = ip;
		while ( *current_arg_str==' ' || *current_arg_str=='\t' )
		    current_arg_str++;
		if ( DEBUG ) {
		    fprintf(stderr,"debug: current_arg = [%s]\n",current_arg);
		}
		if ( parse_in_braces(ip, lines, j, proc_end[i]-1, ",)", &next_arg) != j ) {
		    next_arg = NULL;
		}
		if ( next_arg == NULL ) {
		    fprintf(stderr,"[ERROR] file = %s  Cannot handle: %d\n",
			    file_name,j+1);
		    goto quit;
		}
		if ( arg_cnt == target_arg ) {
		    char tmp_buf[SZ_LINE_BUF];
		    char *op = tmp_buf;
		    char *maxop = tmp_buf + SZ_LINE_BUF -1;
		    const char *ip2;
		    size_t len_target;
		    bool skip_ok;
		    /* */
		    if ( strncmp(current_arg_str,ap_val,strlen(ap_val)) == 0 && 
			 is_valchar(current_arg_str[6]) == 0 ) {
			break;
		    }
		    /* */
		    len_target = next_arg - current_arg_str;
		    while ( 0 < len_target && 
			    (current_arg_str[len_target-1]==' ' ||
			     current_arg_str[len_target-1]=='\t') ) {
			len_target--;
		    }
		    if ( prev_line_idx + 1 == j && 
			 strlen(prev_arg_str) == len_target &&
			 strncmp(prev_arg_str,current_arg_str,len_target)==0 ){
			skip_ok = true;
			prev_line_idx++;
		    }
		    else skip_ok = false;
		    /* [tab]sz_val = ... */
		    if ( skip_ok == false ) {
			if ( len_target < SZ_LINE_BUF ) {
			    strncpy(prev_arg_str,current_arg_str,len_target);
			    prev_arg_str[len_target] = '\0';
			    prev_line_idx = j;
			}
			else {
			    prev_arg_str[0] = '\0';
			    prev_line_idx = -1;
			}
			/* */
			ip2 = lines[j];
			for ( ; ip2 < ptr_call_begin ; op++, ip2++ ) {
			    if ( op < maxop ) *op = *ip2;
			}
			ip2 = ap_val;
			for ( ; *ip2 != '\0' ; op++, ip2++ ) {
			    if ( op < maxop ) *op = *ip2;
			}
			ip2 = " = ";
			for ( ; *ip2 != '\0' ; op++, ip2++ ) {
			    if ( op < maxop ) *op = *ip2;
			}
			/* */
			ip2 = current_arg_str;
			/* check `long( ... )' */
			if ( strcmp(ap_type,"long")==0 &&
			     strncmp("Meml[",ip,5)==0 ) {
			    break;
			}
			if ( strncmp("long",ip,4)==0 ) {
			    const char *ip3;
			    ip3 = ip2 + 4;
			    while ( *ip3==' ' || *ip3=='\t' ) ip3++;
			    if ( *ip3 == '(' && ip2[len_target-1] == ')' ) {
				ip3++;
				len_target --;
				ip2 = ip3;
			    }
			}
			for ( ; ip2 < current_arg_str + len_target ; op++, ip2++ ) {
			    if ( op < maxop ) *op = *ip2;
			}
			ip2 = "\n";
			for ( ; *ip2 != '\0' ; op++, ip2++ ) {
			    if ( op < maxop ) *op = *ip2;
			}
		    }
		    /* [tab]call ... */
		    ip2 = lines[j];
		    for ( ; ip2 < ptr_call_begin ; op++, ip2++ ) {
			if ( op < maxop ) *op = *ip2;
		    }
		    ip2 = "call ";
		    for ( ; *ip2 != '\0' ; op++, ip2++ ) {
			if ( op < maxop ) *op = *ip2;
		    }
		    ip2 = ptr_proc_begin;
		    for ( ; ip2 < current_arg ; op++, ip2++ ) {
			if ( op < maxop ) *op = *ip2;
		    }
		    if ( arg_cnt != 0 ) {
			ip2 = " ";
			for ( ; *ip2 != '\0' ; op++, ip2++ ) {
			    if ( op < maxop ) *op = *ip2;
			}
		    }
		    ip2 = ap_val;
		    for ( ; *ip2 != '\0' ; op++, ip2++ ) {
			if ( op < maxop ) *op = *ip2;
		    }
		    ip2 = next_arg;
		    for ( ; *ip2 != '\0' ; op++, ip2++ ) {
			if ( op < maxop ) *op = *ip2;
		    }
		    *op = '\0';
		    target_found = true;
		    /* replace */
		    free(lines[j]);
		    lines[j] = strdup(tmp_buf);
		    if ( lines[j] == NULL ) {
			fprintf(stderr,"[ERROR] strdup() failed\n");
			goto quit;
		    }
		    break;
		}
		ip = next_arg;
		arg_cnt++;
	    } while ( *ip != ')' );
	}
	if ( target_found == false ) insert_idx[i] = -1;
	else needs_update = true;
    }

    /* update the source file */
    if ( needs_update == true ) {
	fp = fopen(file_name,"w");
	if ( fp == NULL ) {
	    fprintf(stderr,"[ERROR] cannot open: %s\n",file_name);
	    goto quit;
	}
	printf("updating: %s\n",file_name);
	fflush(stdout);
	for ( i=0 ; i < num_lines ; i++ ) {
	    int j;
	    for ( j=0 ; j < num_proc ; j++ ) {
		if ( i == insert_idx[j] ) {
		    fprintf(fp,"%s\t%s\n",ap_type,ap_val);
		}
	    }
	    fprintf(fp,"%s",lines[i]);
	}
	fclose(fp);
	fp = NULL;
    }

    return_status = 0;
 quit:
    if ( lines != NULL ) {
	for ( i=0 ; i < num_lines ; i++ ) {
	    free(lines[i]);
	}
	free(lines);
    }
    if ( flags != NULL ) free(flags);
    if ( fp != NULL ) fclose(fp);
    return return_status;
}

