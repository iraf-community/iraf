/* gcc -O -Wall add_sz_val.c -o add_sz_val */
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define DEBUG 0

#define SZ_LINE_BUF 65536
#define SZ_NUM_PROC 1024

static int add_sz_val( const char *, int, const char * );

int main( int argc, char *argv[] )
{
    int return_status = -1;
    const char *proc_name;
    int target_arg;
    int i;

    if ( argc < 4 ) {
	fprintf(stderr,"[USAGE]\n");
	fprintf(stderr,"%s proc_name arg_index foo.x foo1.x ...\n",argv[0]);
	fprintf(stderr,"(example)\n");
	fprintf(stderr,"%s salloc 1 foo.x foo1.x ...\n",argv[0]);
	goto quit;
    }

    proc_name = argv[1];
    target_arg = atoi(argv[2]);

    for ( i=3 ; i < argc ; i++ ) {
	int status;
	//printf("[INFO] target = %s\n",argv[i]);
	status = add_sz_val(proc_name,target_arg,argv[i]);
	if ( status != 0 ) {
	    fprintf(stderr,"[ERROR] add_sz_val() failed\n");
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

static int add_sz_val( const char *proc_name, int target_arg, 
		       const char *file_name )
{
    int return_status = -1;
    char line_buf[SZ_LINE_BUF];
    FILE *fp = NULL;
    int num_lines = 0;
    char **lines = NULL;
    int num_proc;
    bool needs_update = false;
    /* line indices */
    int proc_decl[SZ_NUM_PROC];
    int proc_begin[SZ_NUM_PROC];
    int proc_end[SZ_NUM_PROC];
    int insert_idx[SZ_NUM_PROC];	/* index to insert sz_val */
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
	lines[num_lines] = strdup(line_buf);
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
	     (ip[5] == ' ' || ip[5] == '\t' || ip[5] == '\n') ) {
	    proc_begin[num_proc] = i;
	}
	else if ( strncmp(ip,"end",3) == 0 &&
	     (ip[3] == ' ' || ip[3] == '\t' || ip[3] == '\n') ) {
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

	j0 = proc_decl[i];
	do {
	    comm_ptr = strrchr(lines[j0],',');
	    if ( comm_ptr != NULL ) {
		comm_ptr++;
		while ( *comm_ptr==' ' || *comm_ptr=='\t' || *comm_ptr=='\n' )
		    comm_ptr++;
		if ( *comm_ptr != '\0' ) comm_ptr = NULL;
	    }
	    j0++;
	} while ( comm_ptr != NULL );

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
	    ip = strstr(lines[j],"\tsz_val");
	    if ( ip != NULL && is_valchar(ip[7]) == 0 ) {
		/* already exists */
		insert_idx[i] = -1;
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
	int j;
	bool target_found = false;
	for ( j=proc_begin[i]+1 ; j < proc_end[i] ; j++ ) {
	    const char *ip;
	    const char *ptr_call_begin;
	    const char *ptr_proc_begin;
	    int arg_cnt = 0;
	    if ( DEBUG ) {
		fprintf(stderr,"debug: j = %d\n",j);
	    }
	    ip = lines[j];
	    while ( *ip == ' ' || *ip == '\t' ) ip++;
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
	    /* 1st arg begins... */
	    if ( DEBUG ) {
		fprintf(stderr,"debug: 1st arg begins...\n");
	    }
	    do {
		const char *ip1;
		const char *next_arg = NULL;
		const char *current_arg = NULL;
		const char *current_arg_str = NULL;
		int br_in_0 = 0;	/* [] */
		int br_in_1 = 0;	/* () */
		ip++;			/* skip '(', or ',' */
		ip1 = ip;
		current_arg = ip;
		current_arg_str = ip;
		while ( *current_arg_str==' ' || *current_arg_str=='\t' )
		    current_arg_str++;
		if ( DEBUG ) {
		    fprintf(stderr,"debug: current_arg = [%s]\n",current_arg);
		}
		while ( *ip1 != '\n' && *ip1 != '\0' ) {
		    if ( (*ip1 == ','||*ip1 == ')') &&
			 br_in_0 == 0 && br_in_1 == 0 ) {
			next_arg = ip1;
			break;
		    }
		    else if ( *ip1 == '[' ) br_in_0++;
		    else if ( *ip1 == '(' ) br_in_1++;
		    else if ( *ip1 == ']' ) br_in_0--;
		    else if ( *ip1 == ')' ) br_in_1--;
		    ip1 ++;
		}
		if ( next_arg == NULL ) {
		    fprintf(stderr,"[ERROR] Invalid line: '%s'\n",lines[j]);
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
		    if ( strncmp(current_arg_str,"sz_val",6) == 0 && 
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
			ip2 = "sz_val = ";
			for ( ; *ip2 != '\0' ; op++, ip2++ ) {
			    if ( op < maxop ) *op = *ip2;
			}
			ip2 = current_arg_str;
			for ( ; ip2 < next_arg ; op++, ip2++ ) {
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
		    ip2 = " sz_val";
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

    if ( needs_update == true ) {
	fp = fopen(file_name,"w");
	if ( fp == NULL ) {
	    fprintf(stderr,"[ERROR] file not found: %s\n",file_name);
	    goto quit;
	}
	printf("updating: %s\n",file_name);
	for ( i=0 ; i < num_lines ; i++ ) {
	    int j;
	    for ( j=0 ; j < num_proc ; j++ ) {
		if ( i == insert_idx[j] ) {
		    fprintf(fp,"size_t\tsz_val\n");
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
    if ( fp != NULL ) fclose(fp);
    return return_status;
}

