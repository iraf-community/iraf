/************************************************************************
     Together, f77_wrap1.c and f77_wrap2.c contain C wrappers for all
     the CFITSIO routines prototyped in fitsio.h, except for the
     generic datatype routines and features not supported in fortran
     (eg, unsigned integers), a few routines prototyped in fitsio2.h,
     which only a handful of FTOOLS use, plus a few obsolete FITSIO
     routines not present in CFITSIO.  This file allows Fortran code
     to use the CFITSIO library instead of the FITSIO library without
     modification.  It also gives access to new routines not present
     in FITSIO.  Fortran FTOOLS must continue using the old routine
     names from FITSIO (ie, ftxxxx), but most of the C-wrappers simply
     redirect those calls to the corresponding CFITSIO routines (ie,
     ffxxxx), with appropriate parameter massaging where necessary.
     The main exception are read/write routines ending in j (ie, long
     data) which get redirected to C routines ending in k (ie, int
     data). This is more consistent with the default integer type in
     Fortran. f77_wrap1.c primarily holds routines operating on whole
     files and extension headers.  f77_wrap2.c handle routines which
     read and write the data portion, plus miscellaneous extra routines.
     
        File created by Peter Wilson (HSTX), Oct-Dec. 1997
************************************************************************/

#include "fitsio2.h"
#include "f77_wrap.h"

unsigned long gMinStrLen=80L;
fitsfile *gFitsFiles[MAXFITSFILES]={0};

/*----------------  Fortran Unit Number Allocation -------------*/

void Cffgiou( int *unit, int *status );
void Cffgiou( int *unit, int *status )
{
   int i;

   if( *status>0 ) return;
   for( i=50;i<MAXFITSFILES;i++ ) /* Using a unit=0 sounds bad, so start at 1 */
      if( gFitsFiles[i]==NULL ) break;
   if( i==MAXFITSFILES ) {
      *unit = 0;
      *status = TOO_MANY_FILES;
      ffpmsg("Cffgiou has no more available unit numbers.");
   } else {
      *unit=i;
      gFitsFiles[i] = (fitsfile *)1; /*  Flag it as taken until ftopen/init  */
                                     /*  can be called and set a real value  */
   }
}
FCALLSCSUB2(Cffgiou,FTGIOU,ftgiou,PINT,PINT)

void Cfffiou( int unit, int *status );
void Cfffiou( int unit, int *status )
{
   if( *status>0 ) return;
   if( unit == -1 ) {
      int i; for( i=50; i<MAXFITSFILES; ) gFitsFiles[i++]=NULL;
   } else if( unit<1 || unit>=MAXFITSFILES ) {
      *status = BAD_FILEPTR;
      ffpmsg("Cfffiou was sent an unacceptable unit number.");
   } else gFitsFiles[unit]=NULL;
}
FCALLSCSUB2(Cfffiou,FTFIOU,ftfiou,INT,PINT)


int CFits2Unit( fitsfile *fptr );
int CFits2Unit( fitsfile *fptr )
     /* Utility routine to convert a fitspointer to a Fortran unit number */
     /* for use when a C program is calling a Fortran routine which could */
     /* in turn call CFITSIO... Modelled after code by Ning Gan.          */
{
   static fitsfile *last_fptr = (fitsfile *)NULL; /* Remember last fptr */
   static int last_unit = 0;                      /* Remember last unit */
   int status = 0;

   /*  Test whether we are repeating the last lookup  */

   if( last_unit && fptr==gFitsFiles[last_unit] )
      return( last_unit );

   /*  Check if gFitsFiles has an entry for this fptr.  */
   /*  Allows Fortran to call C to call Fortran to      */
   /*  call CFITSIO... OUCH!!!                          */

   last_fptr = fptr;
   for( last_unit=1; last_unit<MAXFITSFILES; last_unit++ ) {
      if( fptr == gFitsFiles[last_unit] )
	 return( last_unit );
   }

   /*  Allocate a new unit number for this fptr  */
   Cffgiou( &last_unit, &status );
   if( status )
      last_unit = 0;
   else
      gFitsFiles[last_unit] = fptr;
   return( last_unit );
}

     /**************************************************/
     /*   Start of wrappers for routines in fitsio.h   */
     /**************************************************/

/*----------------  FITS file URL parsing routines -------------*/

FCALLSCSUB9(ffiurl,FTIURL,ftiurl,STRING,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,PINT)
FCALLSCSUB3(ffrtnm,FTRTNM,ftrtnm,STRING,PSTRING,PINT)
FCALLSCSUB3(ffextn,FTEXTN,ftextn,STRING,PINT,PINT)

/*---------------- FITS file I/O routines ---------------*/

void Cffopen( fitsfile **fptr, const char *filename, int iomode, int *blocksize, int *status );
void Cffopen( fitsfile **fptr, const char *filename, int iomode, int *blocksize, int *status )
{
   int hdutype;

   if( *fptr==NULL || *fptr==(fitsfile*)1 ) {
      ffopen( fptr, filename, iomode, status );
      ffmahd( *fptr, 1, &hdutype, status );
      *blocksize = 1;
   } else {
      *status = FILE_NOT_OPENED;
      ffpmsg("Cffopen tried to use an already opened unit.");
   }
}
FCALLSCSUB5(Cffopen,FTOPEN,ftopen,PFITSUNIT,STRING,INT,PINT,PINT)

void Cffnopn( fitsfile **fptr, const char *filename, int iomode, int *status );
void Cffnopn( fitsfile **fptr, const char *filename, int iomode, int *status )
{
   if( *fptr==NULL || *fptr==(fitsfile*)1 ) {
      ffopen( fptr, filename, iomode, status );
   } else {
      *status = FILE_NOT_OPENED;
      ffpmsg("Cffnopn tried to use an already opened unit.");
   }
}
FCALLSCSUB4(Cffnopn,FTNOPN,ftnopn,PFITSUNIT,STRING,INT,PINT)

void Cffreopen( fitsfile *openfptr, fitsfile **newfptr, int *status );
void Cffreopen( fitsfile *openfptr, fitsfile **newfptr, int *status )
{
   if( *newfptr==NULL || *newfptr==(fitsfile*)1 ) {
      ffreopen( openfptr, newfptr, status );
   } else {
      *status = FILE_NOT_OPENED;
      ffpmsg("Cffreopen tried to use an already opened unit.");
   }
}
FCALLSCSUB3(Cffreopen,FTREOPEN,ftreopen,FITSUNIT,PFITSUNIT,PINT)

void Cffinit( fitsfile **fptr, const char *filename, int blocksize, int *status );
void Cffinit( fitsfile **fptr, const char *filename, int blocksize, int *status )
{
   if( *fptr==NULL || *fptr==(fitsfile*)1 ) {
      ffinit( fptr, filename, status );
   } else {
      *status = FILE_NOT_CREATED;
      ffpmsg("Cffinit tried to use an already opened unit.");
   }
}
FCALLSCSUB4(Cffinit,FTINIT,ftinit,PFITSUNIT,STRING,INT,PINT)

void Cfftplt( fitsfile **fptr, const char *filename, const char *tempname,
	      int *status );
void Cfftplt( fitsfile **fptr, const char *filename, const char *tempname, 
	      int *status )
{
   if( *fptr==NULL || *fptr==(fitsfile*)1 ) {
      fftplt( fptr, filename, tempname, status );
   } else {
      *status = FILE_NOT_CREATED;
      ffpmsg("Cfftplt tried to use an already opened unit.");
   }
}
FCALLSCSUB4(Cfftplt,FTTPLT,fttplt,PFITSUNIT,STRING,STRING,PINT)

FCALLSCSUB2(ffflus,FTFLUS,ftflus,FITSUNIT,PINT)

void Cffclos( int unit, int *status );
void Cffclos( int unit, int *status )
{
   if( gFitsFiles[unit]!=NULL && gFitsFiles[unit]!=(void*)1 ) {
      ffclos( gFitsFiles[unit], status );  /* Flag unit number as unavailable */
      gFitsFiles[unit]=(fitsfile*)1;       /* in case want to reuse it        */
   }
}
FCALLSCSUB2(Cffclos,FTCLOS,ftclos,INT,PINT)

void Cffdelt( int unit, int *status );
void Cffdelt( int unit, int *status )
{
   if( gFitsFiles[unit]!=NULL && gFitsFiles[unit]!=(void*)1 ) {
      ffdelt( gFitsFiles[unit], status );  /* Flag unit number as unavailable */
      gFitsFiles[unit]=(fitsfile*)1;       /* in case want to reuse it        */
   }
}
FCALLSCSUB2(Cffdelt,FTDELT,ftdelt,INT,PINT)

FCALLSCSUB3(ffflnm,FTFLNM,ftflnm,FITSUNIT,PSTRING,PINT)
FCALLSCSUB3(ffflmd,FTFLMD,ftflmd,FITSUNIT,PINT,PINT)

/*--------------- utility routines ---------------*/
FCALLSCSUB1(ffvers,FTVERS,ftvers,PFLOAT)
FCALLSCSUB1(ffupch,FTUPCH,ftupch,PSTRING)
FCALLSCSUB2(ffgerr,FTGERR,ftgerr,INT,PSTRING)
FCALLSCSUB1(ffpmsg,FTPMSG,ftpmsg,STRING)
FCALLSCSUB1(ffgmsg,FTGMSG,ftgmsg,PSTRING)
FCALLSCSUB0(ffcmsg,FTCMSG,ftcmsg)

void Cffrprt( char *fname, int status );
void Cffrprt( char *fname, int status )
{
   if( !strcmp(fname,"STDOUT") || !strcmp(fname,"stdout") )
      ffrprt( stdout, status );
   else if( !strcmp(fname,"STDERR") || !strcmp(fname,"stderr") )
      ffrprt( stderr, status );
   else {
      FILE *fptr;

      fptr = fopen(fname, "a");
      if (fptr==NULL)
	 printf("file pointer is null.\n");
      else {
	 ffrprt(fptr,status);
	 fclose(fptr);
      }
   }
}
FCALLSCSUB2(Cffrprt,FTRPRT,ftrprt,STRING,INT)

FCALLSCSUB5(ffcmps,FTCMPS,ftcmps,STRING,STRING,LOGICAL,PLOGICAL,PLOGICAL)
FCALLSCSUB2(fftkey,FTTKEY,fttkey,STRING,PINT)
FCALLSCSUB2(fftrec,FTTREC,fttrec,STRING,PINT)
FCALLSCSUB2(ffnchk,FTNCHK,ftnchk,FITSUNIT,PINT)
FCALLSCSUB4(ffkeyn,FTKEYN,ftkeyn,STRING,INT,PSTRING,PINT)
FCALLSCSUB4(ffgknm,FTGKNM,ftgknm,STRING,PSTRING, PINT, PINT)
FCALLSCSUB4(ffnkey,FTNKEY,ftnkey,INT,STRING,PSTRING,PINT)
FCALLSCSUB3(ffdtyp,FTDTYP,ftdtyp,STRING,PSTRING,PINT)
FCALLSCFUN1(INT,ffgkcl,FTGKCL,ftgkcl,STRING)
FCALLSCSUB4(ffpsvc,FTPSVC,ftpsvc,STRING,PSTRING,PSTRING,PINT)
FCALLSCSUB4(ffgthd,FTGTHD,ftgthd,STRING,PSTRING,PINT,PINT)
FCALLSCSUB5(ffasfm,FTASFM,ftasfm,STRING,PINT,PLONG,PINT,PINT)
FCALLSCSUB5(ffbnfm,FTBNFM,ftbnfm,STRING,PINT,PLONG,PLONG,PINT)

#define ftgabc_STRV_A2 NUM_ELEM_ARG(1)
#define ftgabc_LONGV_A5 A1
FCALLSCSUB6(ffgabc,FTGABC,ftgabc,INT,STRINGV,INT,PLONG,LONGV,PINT)

/*----------------- write single keywords --------------*/
FCALLSCSUB3(ffprec,FTPREC,ftprec,FITSUNIT,STRING,PINT)
FCALLSCSUB3(ffpcom,FTPCOM,ftpcom,FITSUNIT,STRING,PINT)
FCALLSCSUB4(ffpunt,FTPUNT,ftpunt,FITSUNIT,STRING,STRING,PINT)
FCALLSCSUB3(ffphis,FTPHIS,ftphis,FITSUNIT,STRING,PINT)
FCALLSCSUB2(ffpdat,FTPDAT,ftpdat,FITSUNIT,PINT)
FCALLSCSUB3(ffgstm,FTGSTM,ftgstm,PSTRING,PINT,PINT)
FCALLSCSUB4(ffgsdt,FTGSDT,ftgsdt,PINT,PINT,PINT,PINT)
FCALLSCSUB5(ffdt2s,FTDT2S,ftdt2s,INT,INT,INT,PSTRING,PINT)
FCALLSCSUB9(fftm2s,FTTM2S,fttm2s,INT,INT,INT,INT,INT,DOUBLE,INT,PSTRING,PINT)
FCALLSCSUB5(ffs2dt,FTS2DT,fts2dt,STRING,PINT,PINT,PINT,PINT)
FCALLSCSUB8(ffs2tm,FTS2TM,fts2tm,STRING,PINT,PINT,PINT,PINT,PINT,PDOUBLE,PINT)
FCALLSCSUB4(ffpkyu,FTPKYU,ftpkyu,FITSUNIT,STRING,STRING,PINT)
FCALLSCSUB5(ffpkys,FTPKYS,ftpkys,FITSUNIT,STRING,STRING,STRING,PINT)
FCALLSCSUB5(ffpkls,FTPKLS,ftpkls,FITSUNIT,STRING,STRING,STRING,PINT)
FCALLSCSUB2(ffplsw,FTPLSW,ftplsw,FITSUNIT,PINT)
FCALLSCSUB5(ffpkyl,FTPKYL,ftpkyl,FITSUNIT,STRING,INT,STRING,PINT)
FCALLSCSUB5(ffpkyj,FTPKYJ,ftpkyj,FITSUNIT,STRING,LONG,STRING,PINT)
FCALLSCSUB6(ffpkyf,FTPKYF,ftpkyf,FITSUNIT,STRING,FLOAT,INT,STRING,PINT)
FCALLSCSUB6(ffpkye,FTPKYE,ftpkye,FITSUNIT,STRING,FLOAT,INT,STRING,PINT)
FCALLSCSUB6(ffpkyg,FTPKYG,ftpkyg,FITSUNIT,STRING,DOUBLE,INT,STRING,PINT)
FCALLSCSUB6(ffpkyd,FTPKYD,ftpkyd,FITSUNIT,STRING,DOUBLE,INT,STRING,PINT)
FCALLSCSUB6(ffpkyc,FTPKYC,ftpkyc,FITSUNIT,STRING,FLOATV,INT,STRING,PINT)
FCALLSCSUB6(ffpkym,FTPKYM,ftpkym,FITSUNIT,STRING,DOUBLEV,INT,STRING,PINT)
FCALLSCSUB6(ffpkfc,FTPKFC,ftpkfc,FITSUNIT,STRING,FLOATV,INT,STRING,PINT)
FCALLSCSUB6(ffpkfm,FTPKFM,ftpkfm,FITSUNIT,STRING,DOUBLEV,INT,STRING,PINT)
FCALLSCSUB6(ffpkyt,FTPKYT,ftpkyt,FITSUNIT,STRING,LONG,DOUBLE,STRING,PINT)

#define ftptdm_LONGV_A4 A3
FCALLSCSUB5(ffptdm,FTPTDM,ftptdm,FITSUNIT,INT,INT,LONGV,PINT)

/*----------------- write array of keywords --------------*/
#define ftpkns_STRV_A5 NUM_ELEM_ARG(4)
#define ftpkns_STRV_A6 NUM_ELEM_ARG(4)
FCALLSCSUB7(ffpkns,FTPKNS,ftpkns,FITSUNIT,STRING,INT,INT,STRINGV,STRINGV,PINT)

/*   Must handle LOGICALV conversion manually... ffpknl uses ints   */
void Cffpknl( fitsfile *fptr, char *keyroot, int nstart, int nkeys,
              int *numval, char **comment, int *status );
void Cffpknl( fitsfile *fptr, char *keyroot, int nstart, int nkeys,
              int *numval, char **comment, int *status )
{
   int i;
 
   for( i=0; i<nkeys; i++ )
      numval[i] = F2CLOGICAL(numval[i]);
   ffpknl( fptr, keyroot, nstart, nkeys, numval, comment, status );
   for( i=0; i<nkeys; i++ )
      numval[i] = C2FLOGICAL(numval[i]);
}
#define ftpknl_STRV_A6 NUM_ELEM_ARG(4)
FCALLSCSUB7(Cffpknl,FTPKNL,ftpknl,FITSUNIT,STRING,INT,INT,INTV,STRINGV,PINT)

#define ftpknj_STRV_A6 NUM_ELEM_ARG(4)
#define ftpknj_LONGV_A5 A4
FCALLSCSUB7(ffpknj,FTPKNJ,ftpknj,FITSUNIT,STRING,INT,INT,LONGV,STRINGV,PINT)

#define ftpknf_STRV_A7 NUM_ELEM_ARG(4)
FCALLSCSUB8(ffpknf,FTPKNF,ftpknf,FITSUNIT,STRING,INT,INT,FLOATV,INT,STRINGV,PINT)

#define ftpkne_STRV_A7 NUM_ELEM_ARG(4)
FCALLSCSUB8(ffpkne,FTPKNE,ftpkne,FITSUNIT,STRING,INT,INT,FLOATV,INT,STRINGV,PINT)

#define ftpkng_STRV_A7 NUM_ELEM_ARG(4)
FCALLSCSUB8(ffpkng,FTPKNG,ftpkng,FITSUNIT,STRING,INT,INT,DOUBLEV,INT,STRINGV,PINT)

#define ftpknd_STRV_A7 NUM_ELEM_ARG(4)
FCALLSCSUB8(ffpknd,FTPKND,ftpknd,FITSUNIT,STRING,INT,INT,DOUBLEV,INT,STRINGV,PINT)

FCALLSCSUB6(ffcpky,FTCPKY,ftcpky,FITSUNIT,FITSUNIT,INT,INT,STRING,PINT)

/*----------------- write required header keywords --------------*/
#define ftphps_LONGV_A4 A3
FCALLSCSUB5(ffphps,FTPHPS,ftphps,FITSUNIT,INT,INT,LONGV,PINT)

void Cffphpr( fitsfile *fptr, int simple, int bitpix, int naxis, long naxes[], long pcount, long gcount, int extend, int *status );
void Cffphpr( fitsfile *fptr, int simple, int bitpix, int naxis, long naxes[], long pcount, long gcount, int extend, int *status )
{
   if( gcount==0 ) gcount=1;
   ffphpr( fptr, simple, bitpix, naxis, naxes, pcount,
           gcount, extend, status );
}
#define ftphpr_LONGV_A5 A4
FCALLSCSUB9(Cffphpr,FTPHPR,ftphpr,FITSUNIT,LOGICAL,INT,INT,LONGV,LONG,LONG,LOGICAL,PINT)


#define ftphtb_STRV_A5 NUM_ELEM_ARG(4)
#define ftphtb_STRV_A7 NUM_ELEM_ARG(4)
#define ftphtb_STRV_A8 NUM_ELEM_ARG(4)
#define ftphtb_LONGV_A6 A4
FCALLSCSUB10(ffphtb,FTPHTB,ftphtb,FITSUNIT,LONG,LONG,INT,STRINGV,LONGV,STRINGV,STRINGV,STRING,PINT)

#define ftphbn_STRV_A4 NUM_ELEM_ARG(3)
#define ftphbn_STRV_A5 NUM_ELEM_ARG(3)
#define ftphbn_STRV_A6 NUM_ELEM_ARG(3)
FCALLSCSUB9(ffphbn,FTPHBN,ftphbn,FITSUNIT,LONG,INT,STRINGV,STRINGV,STRINGV,STRING,LONG,PINT)

/*  Archaic names exist for preceding 3 functions...
    continue supporting them.                           */

#define ftpprh_LONGV_A5 A4
FCALLSCSUB9(Cffphpr,FTPPRH,ftpprh,FITSUNIT,LOGICAL,INT,INT,LONGV,LONG,LONG,LOGICAL,PINT)

#define ftpbnh_STRV_A4 NUM_ELEM_ARG(3)
#define ftpbnh_STRV_A5 NUM_ELEM_ARG(3)
#define ftpbnh_STRV_A6 NUM_ELEM_ARG(3)
FCALLSCSUB9(ffphbn,FTPBNH,ftpbnh,FITSUNIT,LONG,INT,STRINGV,STRINGV,STRINGV,STRING,LONG,PINT)

#define ftptbh_STRV_A5 NUM_ELEM_ARG(4)
#define ftptbh_STRV_A7 NUM_ELEM_ARG(4)
#define ftptbh_STRV_A8 NUM_ELEM_ARG(4)
#define ftptbh_LONGV_A6 A4
FCALLSCSUB10(ffphtb,FTPTBH,ftptbh,FITSUNIT,LONG,LONG,INT,STRINGV,LONGV,STRINGV,STRINGV,STRING,PINT)

/*----------------- write template keywords --------------*/
FCALLSCSUB3(ffpktp,FTPKTP,ftpktp,FITSUNIT,STRING,PINT)

/*------------------ get header information --------------*/
FCALLSCSUB4(ffghsp,FTGHSP,ftghsp,FITSUNIT,PINT,PINT,PINT)
FCALLSCSUB4(ffghps,FTGHPS,ftghps,FITSUNIT,PINT,PINT,PINT)

/*------------------ move position in header -------------*/
FCALLSCSUB3(ffmaky,FTMAKY,ftmaky,FITSUNIT,INT,PINT)
FCALLSCSUB3(ffmrky,FTMRKY,ftmrky,FITSUNIT,INT,PINT)

/*------------------ read single keywords ----------------*/
#define ftgnxk_STRV_A2 NUM_ELEM_ARG(3)
#define ftgnxk_STRV_A4 NUM_ELEM_ARG(5)
FCALLSCSUB7(ffgnxk,FTGNXK,ftgnxk,FITSUNIT,STRINGV,INT,STRINGV,INT,PSTRING,PINT)
FCALLSCSUB4(ffgrec,FTGREC,ftgrec,FITSUNIT,INT,PSTRING,PINT)
FCALLSCSUB4(ffgcrd,FTGCRD,ftgcrd,FITSUNIT,STRING,PSTRING,PINT)
FCALLSCSUB4(ffgunt,FTGUNT,ftgunt,FITSUNIT,STRING,PSTRING,PINT)
FCALLSCSUB6(ffgkyn,FTGKYN,ftgkyn,FITSUNIT,INT,PSTRING,PSTRING,PSTRING,PINT)
FCALLSCSUB5(ffgkey,FTGKEY,ftgkey,FITSUNIT,STRING,PSTRING,PSTRING,PINT)

/*   FTGKYS supported the long string convention but FFGKYS does not,
     so redirect to FFGKLS.  To handle the pointer to a pointer,
     manually expand the FCALLSC macro and modify function call.       */

CFextern VOID_cfF(FTGKYS,ftgkys)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FITSUNIT,STRING,PSTRING,PSTRING,PINT,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0));
CFextern VOID_cfF(FTGKYS,ftgkys)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FITSUNIT,STRING,PSTRING,PSTRING,PINT,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0))
{
   QCF(FITSUNIT,1)
   QCF(STRING,2)
   QCF(PSTRING,3)   /*  Defines a character pointer  */
   QCF(PSTRING,4)
   QCF(PINT,5)

   ffgkls( TCF(ftgkys,FITSUNIT,1,0)
           TCF(ftgkys,STRING,2,1)
           , &B3                        /*  Pass address of pointer  */
           TCF(ftgkys,PSTRING,4,1)
           TCF(ftgkys,PINT,5,1)     );

   RCF(FITSUNIT,1)
   RCF(STRING,2)
   RCF(PSTRING,3)      /*  Copies as much of pointer as will fit   */
   RCF(PSTRING,4)      /*     into fortran string and frees space  */
   RCF(PINT,5)
}

/*   This is the *real* wrapper to FFGKLS, although it is exactly the
     same as the one for FFGKYS.  To handle the pointer to a pointer,
     manually expand the FCALLSC macro and modify function call.       */

CFextern VOID_cfF(FTGKLS,ftgkls)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FITSUNIT,STRING,PSTRING,PSTRING,PINT,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0));
CFextern VOID_cfF(FTGKLS,ftgkls)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FITSUNIT,STRING,PSTRING,PSTRING,PINT,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0))
{
   QCF(FITSUNIT,1)
   QCF(STRING,2)
   QCF(PSTRING,3)   /*  Defines a character pointer  */
   QCF(PSTRING,4)
   QCF(PINT,5)

   ffgkls( TCF(ftgkls,FITSUNIT,1,0)
           TCF(ftgkls,STRING,2,1)
           , &B3                        /*  Pass address of pointer  */
           TCF(ftgkls,PSTRING,4,1)
           TCF(ftgkls,PINT,5,1)     );

   RCF(FITSUNIT,1)
   RCF(STRING,2)
   RCF(PSTRING,3)      /*  Copies as much of pointer as will fit   */
   RCF(PSTRING,4)      /*     into fortran string and frees space  */
   RCF(PINT,5)
}

FCALLSCSUB5(ffgkyl,FTGKYL,ftgkyl,FITSUNIT,STRING,PINT,PSTRING,PINT)
FCALLSCSUB5(ffgkyj,FTGKYJ,ftgkyj,FITSUNIT,STRING,PLONG,PSTRING,PINT)
FCALLSCSUB5(ffgkye,FTGKYE,ftgkye,FITSUNIT,STRING,PFLOAT,PSTRING,PINT)
FCALLSCSUB5(ffgkyd,FTGKYD,ftgkyd,FITSUNIT,STRING,PDOUBLE,PSTRING,PINT)
FCALLSCSUB5(ffgkyc,FTGKYC,ftgkyc,FITSUNIT,STRING,PFLOAT,PSTRING,PINT)
FCALLSCSUB5(ffgkym,FTGKYM,ftgkym,FITSUNIT,STRING,PDOUBLE,PSTRING,PINT)
FCALLSCSUB6(ffgkyt,FTGKYT,ftgkyt,FITSUNIT,STRING,PLONG,PDOUBLE,PSTRING,PINT)

#define ftgtdm_LONGV_A5 A3
FCALLSCSUB6(ffgtdm,FTGTDM,ftgtdm,FITSUNIT,INT,INT,PINT,LONGV,PINT)

/*------------------ read array of keywords -----------------*/

     /*    Handle array of strings such that only the number of     */
     /*    keywords actually found get copied back to the Fortran   */
     /*    array.  Faster as well as won't cause array overflows    */
     /*    if the the array is smaller than nkeys, but larger than  */
     /*    nfound.                                                  */

#define ftgkns_STRV_A5 NUM_ELEM_ARG(4)
CFextern VOID_cfF(FTGKNS,ftgkns)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FITSUNIT,STRING,INT,INT,PSTRINGV,PINT,PINT,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0));
CFextern VOID_cfF(FTGKNS,ftgkns)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FITSUNIT,STRING,INT,INT,PSTRINGV,PINT,PINT,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0))
{
   QCF(FITSUNIT,1)
   QCF(STRING,2)
   QCF(INT,3)
   QCF(INT,4)
   QCF(PSTRINGV,5)
   QCF(PINT,6)
   QCF(PINT,7)

   ffgkns( TCF(ftgkns,FITSUNIT,1,0)
           TCF(ftgkns,STRING,2,1)
           TCF(ftgkns,INT,3,1)
           TCF(ftgkns,INT,4,1)
           TCF(ftgkns,PSTRINGV,5,1)   /*  Defines the number of strings  */
                                      /*  in array, B5N                  */
           TCF(ftgkns,PINT,6,1)
           TCF(ftgkns,PINT,7,1)     );

   if ( *A7 )       /*  Redefine number of array elements to  */
      B5N = 0;      /*  number found, or none if error.       */
   else
      B5N = *A6;
		      
   RCF(FITSUNIT,1)
   RCF(STRING,2)
   RCF(INT,3)
   RCF(INT,4)
   RCF(PSTRINGV,5)     /*  Copies only found keywords back to Fortran    */
   RCF(PINT,6)
   RCF(PINT,7)
}

/*   Must handle LOGICALV conversion manually... ffgknl uses ints   */
void Cffgknl( fitsfile *fptr, char *keyroot, int nstart, int nkeys,
              int *numval, int *nfound, int *status );
void Cffgknl( fitsfile *fptr, char *keyroot, int nstart, int nkeys,
              int *numval, int *nfound, int *status )
{
   int i;
 
   for( i=0; i<nkeys; i++ )  /*  This preserves array elements across call  */
      numval[i] = F2CLOGICAL(numval[i]);
   ffgknl( fptr, keyroot, nstart, nkeys, numval, nfound, status );
   for( i=0; i<nkeys; i++ )
      numval[i] = C2FLOGICAL(numval[i]);
}
FCALLSCSUB7(Cffgknl,FTGKNL,ftgknl,FITSUNIT,STRING,INT,INT,INTV,PINT,PINT)

#define ftgknj_LONGV_A5 A4
FCALLSCSUB7(ffgknj,FTGKNJ,ftgknj,FITSUNIT,STRING,INT,INT,LONGV,PINT,PINT)
FCALLSCSUB7(ffgkne,FTGKNE,ftgkne,FITSUNIT,STRING,INT,INT,FLOATV,PINT,PINT)
FCALLSCSUB7(ffgknd,FTGKND,ftgknd,FITSUNIT,STRING,INT,INT,DOUBLEV,PINT,PINT)

/*----------------- read required header keywords --------------*/
#define ftghpr_LONGV_A6 A2
FCALLSCSUB10(ffghpr,FTGHPR,ftghpr,FITSUNIT,INT,PLOGICAL,PINT,PINT,LONGV,PLONG,PLONG,PLOGICAL,PINT)


    /*  The following 2 routines contain 3 string vector parameters,  */
    /*  intended to hold column information.  Normally the vectors    */
    /*  are defined with 500-999 elements, but very rarely do tables  */
    /*  have that many columns.  So, to prevent the allocation of     */
    /*  240K of memory to hold all these empty strings and the waste  */
    /*  of CPU time converting Fortran strings to C, *and* back       */
    /*  again, get the number of columns in the table and only        */
    /*  process that many strings (or maxdim, if it is smaller).      */

#define ftghtb_STRV_A6 NUM_ELEMS(maxdim)
#define ftghtb_STRV_A8 NUM_ELEMS(maxdim)
#define ftghtb_STRV_A9 NUM_ELEMS(maxdim)
#define ftghtb_LONGV_A7 A2
CFextern VOID_cfF(FTGHTB,ftghtb)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FITSUNIT,INT,PLONG,PLONG,PINT,PSTRINGV,LONGV,PSTRINGV,PSTRINGV,PSTRING,PINT,CF_0,CF_0,CF_0));
CFextern VOID_cfF(FTGHTB,ftghtb)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FITSUNIT,INT,PLONG,PLONG,PINT,PSTRINGV,LONGV,PSTRINGV,PSTRINGV,PSTRING,PINT,CF_0,CF_0,CF_0))
{
   QCF(FITSUNIT,1)
   QCF(INT,2)
   QCF(PLONG,3)
   QCF(PLONG,4)
   QCF(PINT,5)
   QCF(PSTRINGV,6)
   QCF(LONGV,7)
   QCF(PSTRINGV,8)
   QCF(PSTRINGV,9)
   QCF(PSTRING,A)
   QCF(PINT,B)

   fitsfile *fptr;
   long tfields;
   int maxdim,*status;

   fptr = TCF(ftghtb,FITSUNIT,1,0);
   status =  TCF(ftghtb,PINT,B,0);
   maxdim =  TCF(ftghtb,INT,2,0);
   ffgkyj( fptr, "TFIELDS", &tfields, 0, status );
   maxdim = (maxdim<0) ? tfields : _cfMIN(tfields,maxdim);

   ffghtb(   fptr, maxdim
             TCF(ftghtb,PLONG,3,1)
             TCF(ftghtb,PLONG,4,1)
             TCF(ftghtb,PINT,5,1)
             TCF(ftghtb,PSTRINGV,6,1)
             TCF(ftghtb,LONGV,7,1)
             TCF(ftghtb,PSTRINGV,8,1)
             TCF(ftghtb,PSTRINGV,9,1)
             TCF(ftghtb,PSTRING,A,1)
             , status );

   RCF(FITSUNIT,1)
   RCF(INT,2)
   RCF(PLONG,3)
   RCF(PLONG,4)
   RCF(PINT,5)
   RCF(PSTRINGV,6)
   RCF(LONGV,7)
   RCF(PSTRINGV,8)
   RCF(PSTRINGV,9)
   RCF(PSTRING,A)
   RCF(PINT,B)
}

#define ftghbn_STRV_A5 NUM_ELEMS(maxdim)
#define ftghbn_STRV_A6 NUM_ELEMS(maxdim)
#define ftghbn_STRV_A7 NUM_ELEMS(maxdim)
CFextern VOID_cfF(FTGHBN,ftghbn)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FITSUNIT,INT,PLONG,PINT,PSTRINGV,PSTRINGV,PSTRINGV,PSTRING,PLONG,PINT,CF_0,CF_0,CF_0,CF_0));
CFextern VOID_cfF(FTGHBN,ftghbn)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FITSUNIT,INT,PLONG,PINT,PSTRINGV,PSTRINGV,PSTRINGV,PSTRING,PLONG,PINT,CF_0,CF_0,CF_0,CF_0))
{
   QCF(FITSUNIT,1)
   QCF(INT,2)
   QCF(PLONG,3)
   QCF(PINT,4)
   QCF(PSTRINGV,5)
   QCF(PSTRINGV,6)
   QCF(PSTRINGV,7)
   QCF(PSTRING,8)
   QCF(PLONG,9)
   QCF(PINT,A)

   fitsfile *fptr;
   long tfields;
   int maxdim,*status;

   fptr = TCF(ftghbn,FITSUNIT,1,0);
   status =  TCF(ftghbn,PINT,A,0);
   maxdim =  TCF(ftghbn,INT,2,0);
   ffgkyj( fptr, "TFIELDS", &tfields, 0, status );
   maxdim = (maxdim<0) ? tfields : _cfMIN(tfields,maxdim);

   ffghbn(   fptr, maxdim
             TCF(ftghbn,PLONG,3,1)
             TCF(ftghbn,PINT,4,1)
             TCF(ftghbn,PSTRINGV,5,1)
             TCF(ftghbn,PSTRINGV,6,1)
             TCF(ftghbn,PSTRINGV,7,1)
             TCF(ftghbn,PSTRING,8,1)
             TCF(ftghbn,PLONG,9,1)
             , status );

   RCF(FITSUNIT,1)
   RCF(INT,2)
   RCF(PLONG,3)
   RCF(PINT,4)
   RCF(PSTRINGV,5)
   RCF(PSTRINGV,6)
   RCF(PSTRINGV,7)
   RCF(PSTRING,8)
   RCF(PLONG,9)
   RCF(PINT,A)
}

    /*   The following 3 routines are obsolete and dangerous to use as       */
    /*   there is no bounds checking with the arrays.  Call ftghxx instead.  */
    /*   To get cfortran to work, ftgtbh and ftgbnh require information      */
    /*   on the array size of the string vectors.  The "TFIELDS" key word    */
    /*   is read and used as the vector size.  This *will* cause a           */
    /*   problem if ttype, tform, and tunit are declared with fewer          */
    /*   elements than the actual number of columns.                         */

#if defined(DECFortran) || (defined(__alpha) && defined(g77Fortran)) \
    || (defined(mipsFortran) && _MIPS_SZLONG==64) \
    || (defined(IBMR2Fortran) && defined(__64BIT__))
    /*   If running under DECFortran, we also need to worry about the length */
    /*   of the long naxes array.  So read NAXIS manually. :(                */

void Cffgprh( fitsfile *fptr, int *simple, int *bitpix, int *naxis, int naxes[],
             long *pcount, long *gcount, int *extend, int *status );
void Cffgprh( fitsfile *fptr, int *simple, int *bitpix, int *naxis, int naxes[],
             long *pcount, long *gcount, int *extend, int *status )
{
   long *LONGnaxes, size;

   ffgkyj( fptr, "NAXIS", &size, 0, status );
   LONGnaxes = F2Clongv(size,naxes);
   ffghpr( fptr, (int)size, simple, bitpix, naxis, LONGnaxes,
           pcount, gcount, extend, status );
   C2Flongv(size,naxes,LONGnaxes);
}
FCALLSCSUB9(Cffgprh,FTGPRH,ftgprh,FITSUNIT,PLOGICAL,PINT,PINT,INTV,PLONG,PLONG,PLOGICAL,PINT)

#else

void Cffgprh( fitsfile *fptr, int *simple, int *bitpix, int *naxis, long naxes[],
             long *pcount, long *gcount, int *extend, int *status );
void Cffgprh( fitsfile *fptr, int *simple, int *bitpix, int *naxis, long naxes[],
             long *pcount, long *gcount, int *extend, int *status )
{
   ffghpr( fptr, -1, simple, bitpix, naxis, naxes,
           pcount, gcount, extend, status );
}
#define ftghpr_LONGV_A5 NONE
FCALLSCSUB9(Cffgprh,FTGPRH,ftgprh,FITSUNIT,PLOGICAL,PINT,PINT,LONGV,PLONG,PLONG,PLOGICAL,PINT)

#endif

#define ftgtbh_STRV_A5 NUM_ELEMS(tfields)
#define ftgtbh_STRV_A7 NUM_ELEMS(tfields)
#define ftgtbh_STRV_A8 NUM_ELEMS(tfields)
CFextern VOID_cfF(FTGTBH,ftgtbh)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FITSUNIT,PLONG,PLONG,PINT,PSTRINGV,PLONG,PSTRINGV,PSTRINGV,PSTRING,PINT,CF_0,CF_0,CF_0,CF_0));
CFextern VOID_cfF(FTGTBH,ftgtbh)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FITSUNIT,PLONG,PLONG,PINT,PSTRINGV,PLONG,PSTRINGV,PSTRINGV,PSTRING,PINT,CF_0,CF_0,CF_0,CF_0))
{
   QCF(FITSUNIT,1)
   QCF(PLONG,2)
   QCF(PLONG,3)
   QCF(PINT,4)
   QCF(PSTRINGV,5)
   QCF(PLONG,6)
   QCF(PSTRINGV,7)
   QCF(PSTRINGV,8)
   QCF(PSTRING,9)
   QCF(PINT,A)

   fitsfile *fptr;
   long tfields;
   int *status;

   fptr = TCF(ftgtbh,FITSUNIT,1,0);
   status =  TCF(ftgtbh,PINT,A,0);
   ffgkyj( fptr, "TFIELDS", &tfields, 0, status );

   ffghtb(   fptr, (int)tfields
             TCF(ftgtbh,PLONG,2,1)
             TCF(ftgtbh,PLONG,3,1)
             TCF(ftgtbh,PINT,4,1)
             TCF(ftgtbh,PSTRINGV,5,1)
             TCF(ftgtbh,PLONG,6,1)
             TCF(ftgtbh,PSTRINGV,7,1)
             TCF(ftgtbh,PSTRINGV,8,1)
             TCF(ftgtbh,PSTRING,9,1)
             , status );

   RCF(FITSUNIT,1)
   RCF(PLONG,2)
   RCF(PLONG,3)
   RCF(PINT,4)
   RCF(PSTRINGV,5)
   RCF(PLONG,6)
   RCF(PSTRINGV,7)
   RCF(PSTRINGV,8)
   RCF(PSTRING,9)
   RCF(PINT,A)
}

#define ftgbnh_STRV_A4 NUM_ELEMS(tfields)
#define ftgbnh_STRV_A5 NUM_ELEMS(tfields)
#define ftgbnh_STRV_A6 NUM_ELEMS(tfields)
CFextern VOID_cfF(FTGBNH,ftgbnh)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FITSUNIT,PLONG,PINT,PSTRINGV,PSTRINGV,PSTRINGV,PSTRING,PLONG,PINT,CF_0,CF_0,CF_0,CF_0,CF_0));
CFextern VOID_cfF(FTGBNH,ftgbnh)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FITSUNIT,PLONG,PINT,PSTRINGV,PSTRINGV,PSTRINGV,PSTRING,PLONG,PINT,CF_0,CF_0,CF_0,CF_0,CF_0))
{
   QCF(FITSUNIT,1)
   QCF(PLONG,2)
   QCF(PINT,3)
   QCF(PSTRINGV,4)
   QCF(PSTRINGV,5)
   QCF(PSTRINGV,6)
   QCF(PSTRING,7)
   QCF(PLONG,8)
   QCF(PINT,9)

   fitsfile *fptr;
   long tfields;
   int *status;

   fptr = TCF(ftgbnh,FITSUNIT,1,0);
   status =  TCF(ftgbnh,PINT,9,0);
   ffgkyj( fptr, "TFIELDS", &tfields, 0, status );

   ffghbn(   fptr, (int)tfields
             TCF(ftgbnh,PLONG,2,1)
             TCF(ftgbnh,PINT,3,1)
             TCF(ftgbnh,PSTRINGV,4,1)
             TCF(ftgbnh,PSTRINGV,5,1)
             TCF(ftgbnh,PSTRINGV,6,1)
             TCF(ftgbnh,PSTRING,7,1)
             TCF(ftgbnh,PLONG,8,1)
             , status );

   RCF(FITSUNIT,1)
   RCF(PLONG,2)
   RCF(PINT,3)
   RCF(PSTRINGV,4)
   RCF(PSTRINGV,5)
   RCF(PSTRINGV,6)
   RCF(PSTRING,7)
   RCF(PLONG,8)
   RCF(PINT,9)
}


/*--------------------- update keywords ---------------*/
FCALLSCSUB4(ffucrd,FTUCRD,ftucrd,FITSUNIT,STRING,STRING,PINT)
FCALLSCSUB4(ffukyu,FTUKYU,ftukyu,FITSUNIT,STRING,STRING,PINT)
FCALLSCSUB5(ffukys,FTUKYS,ftukys,FITSUNIT,STRING,STRING,STRING,PINT)
FCALLSCSUB5(ffukls,FTUKLS,ftukls,FITSUNIT,STRING,STRING,STRING,PINT)
FCALLSCSUB5(ffukyl,FTUKYL,ftukyl,FITSUNIT,STRING,INT,STRING,PINT)
FCALLSCSUB5(ffukyj,FTUKYJ,ftukyj,FITSUNIT,STRING,LONG,STRING,PINT)
FCALLSCSUB6(ffukyf,FTUKYF,ftukyf,FITSUNIT,STRING,FLOAT,INT,STRING,PINT)
FCALLSCSUB6(ffukye,FTUKYE,ftukye,FITSUNIT,STRING,FLOAT,INT,STRING,PINT)
FCALLSCSUB6(ffukyg,FTUKYG,ftukyg,FITSUNIT,STRING,DOUBLE,INT,STRING,PINT)
FCALLSCSUB6(ffukyd,FTUKYD,ftukyd,FITSUNIT,STRING,DOUBLE,INT,STRING,PINT)
FCALLSCSUB6(ffukyc,FTUKYC,ftukyc,FITSUNIT,STRING,FLOATV,INT,STRING,PINT)
FCALLSCSUB6(ffukym,FTUKYM,ftukym,FITSUNIT,STRING,DOUBLEV,INT,STRING,PINT)
FCALLSCSUB6(ffukfc,FTUKFC,ftukfc,FITSUNIT,STRING,FLOATV,INT,STRING,PINT)
FCALLSCSUB6(ffukfm,FTUKFM,ftukfm,FITSUNIT,STRING,DOUBLEV,INT,STRING,PINT)

/*--------------------- modify keywords ---------------*/
FCALLSCSUB4(ffmrec,FTMREC,ftmrec,FITSUNIT,INT,STRING,PINT)
FCALLSCSUB4(ffmcrd,FTMCRD,ftmcrd,FITSUNIT,STRING,STRING,PINT)
FCALLSCSUB4(ffmnam,FTMNAM,ftmnam,FITSUNIT,STRING,STRING,PINT)
FCALLSCSUB4(ffmcom,FTMCOM,ftmcom,FITSUNIT,STRING,STRING,PINT)
FCALLSCSUB4(ffmkyu,FTMKYU,ftmkyu,FITSUNIT,STRING,STRING,PINT)
FCALLSCSUB5(ffmkys,FTMKYS,ftmkys,FITSUNIT,STRING,STRING,STRING,PINT)
FCALLSCSUB5(ffmkls,FTMKLS,ftmkls,FITSUNIT,STRING,STRING,STRING,PINT)
FCALLSCSUB5(ffmkyl,FTMKYL,ftmkyl,FITSUNIT,STRING,INT,STRING,PINT)
FCALLSCSUB5(ffmkyj,FTMKYJ,ftmkyj,FITSUNIT,STRING,LONG,STRING,PINT)
FCALLSCSUB6(ffmkyf,FTMKYF,ftmkyf,FITSUNIT,STRING,FLOAT,INT,STRING,PINT)
FCALLSCSUB6(ffmkye,FTMKYE,ftmkye,FITSUNIT,STRING,FLOAT,INT,STRING,PINT)
FCALLSCSUB6(ffmkyg,FTMKYG,ftmkyg,FITSUNIT,STRING,DOUBLE,INT,STRING,PINT)
FCALLSCSUB6(ffmkyd,FTMKYD,ftmkyd,FITSUNIT,STRING,DOUBLE,INT,STRING,PINT)
FCALLSCSUB6(ffmkyc,FTMKYC,ftmkyc,FITSUNIT,STRING,FLOATV,INT,STRING,PINT)
FCALLSCSUB6(ffmkym,FTMKYM,ftmkym,FITSUNIT,STRING,DOUBLEV,INT,STRING,PINT)
FCALLSCSUB6(ffmkfc,FTMKFC,ftmkfc,FITSUNIT,STRING,FLOATV,INT,STRING,PINT)
FCALLSCSUB6(ffmkfm,FTMKFM,ftmkfm,FITSUNIT,STRING,DOUBLEV,INT,STRING,PINT)

/*--------------------- insert keywords ---------------*/
FCALLSCSUB4(ffirec,FTIREC,ftirec,FITSUNIT,INT,STRING,PINT)
FCALLSCSUB4(ffikyu,FTIKYU,ftikyu,FITSUNIT,STRING,STRING,PINT)
FCALLSCSUB5(ffikys,FTIKYS,ftikys,FITSUNIT,STRING,STRING,STRING,PINT)
FCALLSCSUB5(ffikls,FTIKLS,ftikls,FITSUNIT,STRING,STRING,STRING,PINT)
FCALLSCSUB5(ffikyl,FTIKYL,ftikyl,FITSUNIT,STRING,INT,STRING,PINT)
FCALLSCSUB5(ffikyj,FTIKYJ,ftikyj,FITSUNIT,STRING,LONG,STRING,PINT)
FCALLSCSUB6(ffikyf,FTIKYF,ftikyf,FITSUNIT,STRING,FLOAT,INT,STRING,PINT)
FCALLSCSUB6(ffikye,FTIKYE,ftikye,FITSUNIT,STRING,FLOAT,INT,STRING,PINT)
FCALLSCSUB6(ffikyg,FTIKYG,ftikyg,FITSUNIT,STRING,DOUBLE,INT,STRING,PINT)
FCALLSCSUB6(ffikyd,FTIKYD,ftikyd,FITSUNIT,STRING,DOUBLE,INT,STRING,PINT)
FCALLSCSUB6(ffikyc,FTIKYC,ftikyc,FITSUNIT,STRING,FLOATV,INT,STRING,PINT)
FCALLSCSUB6(ffikym,FTIKYM,ftikym,FITSUNIT,STRING,DOUBLEV,INT,STRING,PINT)
FCALLSCSUB6(ffikfc,FTIKFC,ftikfc,FITSUNIT,STRING,FLOATV,INT,STRING,PINT)
FCALLSCSUB6(ffikfm,FTIKFM,ftikfm,FITSUNIT,STRING,DOUBLEV,INT,STRING,PINT)

/*--------------------- delete keywords ---------------*/
FCALLSCSUB3(ffdkey,FTDKEY,ftdkey,FITSUNIT,STRING,PINT)
FCALLSCSUB3(ffdrec,FTDREC,ftdrec,FITSUNIT,INT,PINT)

/*--------------------- get HDU information -------------*/
FCALLSCSUB2(ffghdn,FTGHDN,ftghdn,FITSUNIT,PINT)
FCALLSCSUB3(ffghdt,FTGHDT,ftghdt,FITSUNIT,PINT,PINT)

FCALLSCSUB5(ffghad,FTGHAD,ftghad,FITSUNIT,PLONG,PLONG,PLONG,PINT)

FCALLSCSUB3(ffgidt,FTGIDT,ftgidt,FITSUNIT,PINT,PINT)
FCALLSCSUB3(ffgidm,FTGIDM,ftgidm,FITSUNIT,PINT,PINT)

#define ftgisz_LONGV_A3 A2
FCALLSCSUB4(ffgisz,FTGISZ,ftgisz,FITSUNIT,INT,LONGV,PINT)

#define ftgipr_LONGV_A5 A2
FCALLSCSUB6(ffgipr,FTGIPR,ftgipr,FITSUNIT,INT,PINT,PINT,LONGV,PINT)

/*--------------------- HDU operations -------------*/
FCALLSCSUB4(ffmahd,FTMAHD,ftmahd,FITSUNIT,INT,PINT,PINT)
FCALLSCSUB4(ffmrhd,FTMRHD,ftmrhd,FITSUNIT,INT,PINT,PINT)
FCALLSCSUB5(ffmnhd,FTMNHD,ftmnhd,FITSUNIT,INT,STRING,INT,PINT)
FCALLSCSUB3(ffthdu,FTTHDU,ftthdu,FITSUNIT,PINT,PINT)
FCALLSCSUB2(ffcrhd,FTCRHD,ftcrhd,FITSUNIT,PINT)

#define ftcrim_LONGV_A4 A3
FCALLSCSUB5(ffcrim,FTCRIM,ftcrim,FITSUNIT,INT,INT,LONGV,PINT)

#define ftcrtb_STRV_A5 NUM_ELEM_ARG(4)
#define ftcrtb_STRV_A6 NUM_ELEM_ARG(4)
#define ftcrtb_STRV_A7 NUM_ELEM_ARG(4)
FCALLSCSUB9(ffcrtb,FTCRTB,ftcrtb,FITSUNIT,INT,LONG,INT,STRINGV,STRINGV,STRINGV,STRING,PINT)

#define ftiimg_LONGV_A4 A3
FCALLSCSUB5(ffiimg,FTIIMG,ftiimg,FITSUNIT,INT,INT,LONGV,PINT)

#define ftitab_STRV_A5 NUM_ELEM_ARG(4)
#define ftitab_LONGV_A6 A4
#define ftitab_STRV_A7 NUM_ELEM_ARG(4)
#define ftitab_STRV_A8 NUM_ELEM_ARG(4)
FCALLSCSUB10(ffitab,FTITAB,ftitab,FITSUNIT,LONG,LONG,INT,STRINGV,LONGV,STRINGV,STRINGV,STRING,PINT)

#define ftibin_STRV_A4 NUM_ELEM_ARG(3)
#define ftibin_STRV_A5 NUM_ELEM_ARG(3)
#define ftibin_STRV_A6 NUM_ELEM_ARG(3)
FCALLSCSUB9(ffibin,FTIBIN,ftibin,FITSUNIT,LONG,INT,STRINGV,STRINGV,STRINGV,STRING,LONG,PINT)

#define ftrsim_LONGV_A4 A3
FCALLSCSUB5(ffrsim,FTRSIM,ftrsim,FITSUNIT,INT,INT,LONGV,PINT)
FCALLSCSUB3(ffdhdu,FTDHDU,ftdhdu,FITSUNIT,PINT,PINT)
FCALLSCSUB4(ffcopy,FTCOPY,ftcopy,FITSUNIT,FITSUNIT,INT,PINT)
FCALLSCSUB3(ffcphd,FTCPHD,ftcphd,FITSUNIT,FITSUNIT,PINT)
FCALLSCSUB3(ffcpdt,FTCPDT,ftcpdt,FITSUNIT,FITSUNIT,PINT)
FCALLSCSUB2(ffchfl,FTCHFL,ftchfl,FITSUNIT,PINT)
FCALLSCSUB2(ffcdfl,FTCDFL,ftcdfl,FITSUNIT,PINT)

FCALLSCSUB2(ffrdef,FTRDEF,ftrdef,FITSUNIT,PINT)
FCALLSCSUB3(ffhdef,FTHDEF,fthdef,FITSUNIT,INT,PINT)
FCALLSCSUB3(ffpthp,FTPTHP,ftpthp,FITSUNIT,LONG,PINT)

FCALLSCSUB2(ffpcks,FTPCKS,ftpcks,FITSUNIT,PINT)
FCALLSCSUB4(ffvcks,FTVCKS,ftvcks,FITSUNIT,PINT,PINT,PINT)

     /*  Checksum changed from double to long  */

void Cffgcks( fitsfile *fptr, double *datasum, double *hdusum, int *status );
void Cffgcks( fitsfile *fptr, double *datasum, double *hdusum, int *status )
{
   unsigned long data, hdu;

   ffgcks( fptr, &data, &hdu, status );
   *datasum = data;
   *hdusum  = hdu;
}
FCALLSCSUB4(Cffgcks,FTGCKS,ftgcks,FITSUNIT,PDOUBLE,PDOUBLE,PINT)

void Cffcsum( fitsfile *fptr, long nrec, double *dsum, int *status );
void Cffcsum( fitsfile *fptr, long nrec, double *dsum, int *status )
{
   unsigned long sum;

   ffcsum( fptr, nrec, &sum, status );
   *dsum = sum;
}
FCALLSCSUB4(Cffcsum,FTCSUM,ftcsum,FITSUNIT,LONG,PDOUBLE,PINT)

void Cffesum( double dsum, int complm, char *ascii );
void Cffesum( double dsum, int complm, char *ascii )
{
   unsigned long sum=(unsigned long)dsum;

   ffesum( sum, complm, ascii );
}
FCALLSCSUB3(Cffesum,FTESUM,ftesum,DOUBLE,LOGICAL,PSTRING)

void Cffdsum( char *ascii, int complm, double *dsum );
void Cffdsum( char *ascii, int complm, double *dsum )
{
   unsigned long sum;

   ffdsum( ascii, complm, &sum );
   *dsum = sum;
}
FCALLSCSUB3(Cffdsum,FTDSUM,ftdsum,PSTRING,LOGICAL,PDOUBLE)

     /*   Name changed, so support both versions   */
FCALLSCSUB2(ffupck,FTUPCK,ftupck,FITSUNIT,PINT)
FCALLSCSUB2(ffupck,FTUCKS,ftucks,FITSUNIT,PINT)

/*--------------- define scaling or null values -------------*/
FCALLSCSUB4(ffpscl,FTPSCL,ftpscl,FITSUNIT,DOUBLE,DOUBLE,PINT)
FCALLSCSUB3(ffpnul,FTPNUL,ftpnul,FITSUNIT,LONG,PINT)
FCALLSCSUB5(fftscl,FTTSCL,fttscl,FITSUNIT,INT,DOUBLE,DOUBLE,PINT)
FCALLSCSUB4(fftnul,FTTNUL,fttnul,FITSUNIT,INT,LONG,PINT)
FCALLSCSUB4(ffsnul,FTSNUL,ftsnul,FITSUNIT,INT,STRING,PINT)

/*--------------------- get column information -------------*/
FCALLSCSUB5(ffgcno,FTGCNO,ftgcno,FITSUNIT,LOGICAL,STRING,PINT,PINT)
FCALLSCSUB6(ffgcnn,FTGCNN,ftgcnn,FITSUNIT,LOGICAL,STRING,PSTRING,PINT,PINT)
FCALLSCSUB3(ffgnrw,FTGNRW,ftgnrw,FITSUNIT,PLONG,PINT)
FCALLSCSUB3(ffgncl,FTGNCL,ftgncl,FITSUNIT,PINT,PINT)
FCALLSCSUB4(ffgcdw,FTGCDW,ftgcdw,FITSUNIT,INT,PINT,PINT)

FCALLSCSUB6(ffgtcl,FTGTCL,ftgtcl,FITSUNIT,INT,PINT,PLONG,PLONG,PINT)
FCALLSCSUB11(ffgacl,FTGACL,ftgacl,FITSUNIT,INT,PSTRING,PLONG,PSTRING,PSTRING,PDOUBLE,PDOUBLE,PSTRING,PSTRING,PINT)
FCALLSCSUB11(ffgbcl,FTGBCL,ftgbcl,FITSUNIT,INT,PSTRING,PSTRING,PSTRING,PLONG,PDOUBLE,PDOUBLE,PLONG,PSTRING,PINT)
FCALLSCSUB3(ffgrsz,FTGRSZ,ftgrsz,FITSUNIT,PLONG,PINT)


