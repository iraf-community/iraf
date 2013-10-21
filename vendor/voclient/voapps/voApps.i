/**
 *  VOAPPS.I -- SWIG Interface definition file.
 *
 *  @file       voApps.i
 *  @author     Mike Fitzpatrick
 *  @date       6/03/12
 *
 *  @brief       SWIG Interface definition file.
 */

%module voapps
%{

/*  SAMP messaging
 */
extern int  vosamp (int argc, char **argv, size_t *len, void **result);


/*  VOTable manipulation tasks.
 */
extern int  votcnv (int argc, char **argv, size_t *len, void **result);
extern int  votget (int argc, char **argv, size_t *len, void **result);
extern int  votinfo (int argc, char **argv, size_t *len, void **result);
extern int  votpos (int argc, char **argv, size_t *len, void **result);
extern int  votsort (int argc, char **argv, size_t *len, void **result);
extern int  votstat (int argc, char **argv, size_t *len, void **result);
/*
extern int  votcat (int argc, char **argv, size_t *len, void **result);
extern int  votjoin (int argc, char **argv, size_t *len, void **result);
extern int  votsplit (int argc, char **argv, size_t *len, void **result);
*/


/*  VO Data Access tasks.
 */
extern int  vodata (int argc, char **argv, size_t *len, void **result);
extern int  voatlas (int argc, char **argv, size_t *len, void **result);


/*  VO Registry query tasks.
 */
extern int  voregistry (int argc, char **argv, size_t *len, void **result);


/*  VO Name Resolution Tasks.
 */
extern int  vosesame (int argc, char **argv, size_t *len, void **result);

%}





/*  SAMP messaging
 */
extern int  vosamp (int argc, char **argv, size_t *len, void **result);


/*  VOTable manipulation tasks.
 */
extern int  votcnv (int argc, char **argv, size_t *len, void **result);
extern int  votget (int argc, char **argv, size_t *len, void **result);
extern int  votinfo (int argc, char **argv, size_t *len, void **result);
extern int  votpos (int argc, char **argv, size_t *len, void **result);
extern int  votsort (int argc, char **argv, size_t *len, void **result);
extern int  votstat (int argc, char **argv, size_t *len, void **result);
/*
extern int  votcat (int argc, char **argv, size_t *len, void **result);
extern int  votjoin (int argc, char **argv, size_t *len, void **result);
extern int  votsplit (int argc, char **argv, size_t *len, void **result);
*/


/*  VO Data Access tasks.
 */
extern int  vodata (int argc, char **argv, size_t *len, void **result);
extern int  voatlas (int argc, char **argv, size_t *len, void **result);


/*  VO Registry query tasks.
 */
extern int  voregistry (int argc, char **argv, size_t *len, void **result);


/*  VO Name Resolution Tasks.
 */
extern int  vosesame (int argc, char **argv, size_t *len, void **result);


