/**
 *  VOTPARSE_F77.C -- Public interface for the libVOTable F77 wrapper
 *
 *  @file       votParse_f77.c
 *  @author     Mike Fitzpatrick and Eric Timmermann
 *  @date       8/03/09
 *
 *  @brief      Public interface for the libVOTable F77 wrapper
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <expat.h>
#include <unistd.h>
#include <ctype.h>
#include <errno.h>

#include "votParseP.h"
#include "votParse.h"



/*  Fortran Interface Definitions.
 *
 *  Fortran compilers on various platforms may append one or more trailing
 *  underscores to symbol names, we'll use macros for the interface names
 *  and use defines to see what the symbol name is.
 */
#ifdef  _NO_US_			/*  MACHDEP	*/

#define VF_OPENVOTABLE   	vf_openvotable
#define VF_CLOSEVOTABLE   	vf_closevotable
#define VF_GETRESOURCE   	vf_getresource
#define VF_GETTABLE   		vf_gettable
#define VF_GETFIELD   		vf_getfield
#define VF_GETDATA   		vf_getdata
#define VF_GETTABLEDATA   	vf_gettabledata
#define VF_GETTR   		vf_gettr
#define VF_GETTD   		vf_gettd
#define VF_GETBINARY   		vf_getbinary
#define VF_GETBINARY2  		vf_getbinary2
#define VF_GETFITS   		vf_getfits
#define VF_GETGROUP   		vf_getgroup
#define VF_GETFIELDREF   	vf_getfieldref
#define VF_GETPARAMREF   	vf_getparamref
#define VF_GETDESCRIPTION   	vf_getdescription
#define VF_GETPARAM   		vf_getparam
#define VF_GETINFO   		vf_getinfo
#define VF_GETSTREAM   		vf_getstream
#define VF_GETVALUES   		vf_getvalues
#define VF_GETMIN   		vf_getmin
#define VF_GETMAX   		vf_getmax
#define VF_GETOPTION   		vf_getoption
#define VF_GETLINK   		vf_getlink
#define VF_GETCOOSYS   		vf_getcoosys
#define VF_GETDATATYPE   	vf_getdatatype
#define VF_GETDATATYPESTR       vf_getdatatypestr

#define VF_NEWRESOURCE   	vf_newresource
#define VF_NEWTABLE   		vf_newtable
#define VF_NEWFIELD   		vf_newfield
#define VF_NEWDATA   		vf_newdata
#define VF_NEWTABLEDATA   	vf_newtabledata
#define VF_NEWTR   		vf_newtr
#define VF_NEWTD   		vf_newtd
#define VF_NEWBINARY   		vf_newbinary
#define VF_NEWBINARY2  		vf_newbinary2
#define VF_NEWFITS   		vf_newfits
#define VF_NEWGROUP   		vf_newgroup
#define VF_NEWFIELDREF   	vf_newfieldref
#define VF_NEWPARAMREF   	vf_newparamref
#define VF_NEWDESCRIPTION   	vf_newdescription
#define VF_NEWPARAM   		vf_newparam
#define VF_NEWINFO   		vf_newinfo
#define VF_NEWSTREAM   		vf_newstream
#define VF_NEWVALUES   		vf_newvalues
#define VF_NEWMIN   		vf_newmin
#define VF_NEWMAX   		vf_newmax
#define VF_NEWOPTION   		vf_newoption
#define VF_NEWLINK   		vf_newlink
#define VF_NEWCOOSYS   		vf_newcoosys

#define VF_NEWNODE   		vf_newnode
#define VF_FREENODE   		vf_freenode
#define VF_ATTACHNODE   	vf_attachnode
#define VF_DELETENODE   	vf_deletenode
#define VF_COPYELEMENT   	vf_copyelement
#define VF_GETNCOLS   		vf_getncols
#define VF_GETNROWS   		vf_getnrows
#define VF_GETTABLECELL   	vf_gettablecell
#define VF_GETTABLEINT   	vf_gettableint
#define VF_GETTABLEREAL   	vf_gettablereal
#define VF_GETLENGTH   		vf_getlength
#define VF_GETNUMBEROF   	vf_getnumberof
#define VF_FINDBYATTR   	vf_findbyattr
#define VF_FINDINGROUP   	vf_findingroup
#define VF_NEXTINGROUP   	vf_nextingroup
#define VF_GETNEXT   		vf_getnext
#define VF_GETSIBLING		vf_getsibling
#define VF_GETCHILD   		vf_getchild
#define VF_GETPARENT   		vf_getparent
#define VF_CHILDOFTYPE   	vf_childoftype
#define VF_VALUEOF   		vf_valueof
#define VF_TYPEOF   		vf_typeof
#define VF_SETVALUE   		vf_setvalue
#define VF_GETVALUE   		vf_getvalue
#define VF_GETINTVALUE   	vf_getintvalue
#define VF_GETREALVALUE   	vf_getrealvalue
#define VF_SETATTR   		vf_setattr
#define VF_GETATTR   		vf_getattr
#define VF_WRITEXML		vf_writexml
#define VF_WRITEHTML            vf_writehtml
#define VF_WRITESHTML           vf_writeshtml
#define VF_WRITEASV             vf_writeasv
#define VF_WRITEBSV             vf_writebsv
#define VF_WRITECSV             vf_writecsv
#define VF_WRITETSV             vf_writetsv
#define VF_SETWARN		vf_setwarn

#else

#define VF_OPENVOTABLE   	vf_openvotable_
#define VF_CLOSEVOTABLE   	vf_closevotable_

#define VF_GETRESOURCE   	vf_getresource_
#define VF_GETTABLE   		vf_gettable_
#define VF_GETFIELD   		vf_getfield_
#define VF_GETDATA   		vf_getdata_
#define VF_GETTABLEDATA   	vf_gettabledata_
#define VF_GETTR   		vf_gettr_
#define VF_GETTD   		vf_gettd_
#define VF_GETBINARY   		vf_getbinary_
#define VF_GETBINARY2   	vf_getbinary2_
#define VF_GETFITS   		vf_getfits_
#define VF_GETGROUP   		vf_getgroup_
#define VF_GETFIELDREF   	vf_getfieldref_
#define VF_GETPARAMREF   	vf_getparamref_
#define VF_GETDESCRIPTION   	vf_getdescription_
#define VF_GETPARAM   		vf_getparam_
#define VF_GETINFO   		vf_getinfo_
#define VF_GETSTREAM   		vf_getstream_
#define VF_GETVALUES   		vf_getvalues_
#define VF_GETMIN   		vf_getmin_
#define VF_GETMAX   		vf_getmax_
#define VF_GETOPTION   		vf_getoption_
#define VF_GETLINK   		vf_getlink_
#define VF_GETCOOSYS   		vf_getcoosys_
#define VF_GETDATATYPE   	vf_getdatatype_
#define VF_GETDATATYPESTR       vf_getdatatypestr_

#define VF_NEWRESOURCE   	vf_newresource_
#define VF_NEWTABLE   		vf_newtable_
#define VF_NEWFIELD   		vf_newfield_
#define VF_NEWDATA   		vf_newdata_
#define VF_NEWTABLEDATA   	vf_newtabledata_
#define VF_NEWTR   		vf_newtr_
#define VF_NEWTD   		vf_newtd_
#define VF_NEWBINARY   		vf_newbinary_
#define VF_NEWBINARY2  		vf_newbinary2_
#define VF_NEWFITS   		vf_newfits_
#define VF_NEWGROUP   		vf_newgroup_
#define VF_NEWFIELDREF   	vf_newfieldref_
#define VF_NEWPARAMREF   	vf_newparamref_
#define VF_NEWDESCRIPTION   	vf_newdescription_
#define VF_NEWPARAM   		vf_newparam_
#define VF_NEWINFO   		vf_newinfo_
#define VF_NEWSTREAM   		vf_newstream_
#define VF_NEWVALUES   		vf_newvalues_
#define VF_NEWMIN   		vf_newmin_
#define VF_NEWMAX   		vf_newmax_
#define VF_NEWOPTION   		vf_newoption_
#define VF_NEWLINK   		vf_newlink_
#define VF_NEWCOOSYS   		vf_newcoosys_

#define VF_NEWNODE   		vf_newnode_
#define VF_FREENODE   		vf_freenode_
#define VF_ATTACHNODE   	vf_attachnode_
#define VF_DELETENODE   	vf_deletenode_
#define VF_COPYELEMENT   	vf_copyelement_
#define VF_GETNCOLS   		vf_getncols_
#define VF_GETNROWS   		vf_getnrows_
#define VF_GETTABLECELL   	vf_gettablecell_
#define VF_GETTABLEINT   	vf_gettableint_
#define VF_GETTABLEREAL   	vf_gettablereal_
#define VF_GETLENGTH   		vf_getlength_
#define VF_GETNUMBEROF   	vf_getnumberof_
#define VF_FINDBYATTR   	vf_findbyattr_
#define VF_FINDINGROUP   	vf_findingroup_
#define VF_NEXTINGROUP   	vf_nextingroup_
#define VF_GETNEXT   		vf_getnext_
#define VF_GETSIBLING		vf_getsibling_
#define VF_GETCHILD   		vf_getchild_
#define VF_GETPARENT   		vf_getparent_
#define VF_CHILDOFTYPE   	vf_childoftype_
#define VF_VALUEOF   		vf_valueof_
#define VF_TYPEOF   		vf_typeof_
#define VF_SETVALUE   		vf_setvalue_
#define VF_GETVALUE   		vf_getvalue_
#define VF_GETINTVALUE   	vf_getintvalue_
#define VF_GETREALVALUE   	vf_getrealvalue_
#define VF_SETATTR   		vf_setattr_
#define VF_GETATTR   		vf_getattr_
#define VF_WRITEXML		vf_writexml_
#define VF_WRITEHTML            vf_writehtml_
#define VF_WRITESHTML           vf_writeshtml_
#define VF_WRITEASV             vf_writeasv_
#define VF_WRITEBSV             vf_writebsv_
#define VF_WRITECSV             vf_writecsv_
#define VF_WRITETSV             vf_writetsv_
#define VF_SETWARN		vf_setwarn_

#endif


/**
 *   Public function prototypes.
 */
handle_t  VF_OPENVOTABLE (char *arg, int alen);
void 	  VF_CLOSEVOTABLE (handle_t *vot);

handle_t  VF_GETRESOURCE (handle_t *handle);
handle_t  VF_GETTABLE (handle_t *handle);
handle_t  VF_GETFIELD (handle_t *handle);
handle_t  VF_GETDATA (handle_t *handle);
handle_t  VF_GETTABLEDATA (handle_t *handle);
handle_t  VF_GETTR (handle_t *handle);
handle_t  VF_GETTD (handle_t *handle);
handle_t  VF_GETBINARY (handle_t *handle);
handle_t  VF_GETBINARY2 (handle_t *handle);
handle_t  VF_GETFITS (handle_t *handle);
handle_t  VF_GETGROUP (handle_t *handle);
handle_t  VF_GETFIELDRef (handle_t *handle);
handle_t  VF_GETPARAMREF (handle_t *handle);
handle_t  VF_GETDESCRIPTION (handle_t *handle);
handle_t  VF_GETPARAM (handle_t *handle);
handle_t  VF_GETINFO (handle_t *handle);
handle_t  VF_GETSTREAM (handle_t *handle);
handle_t  VF_GETVALUES (handle_t *handle);
handle_t  VF_GETMIN (handle_t *handle);
handle_t  VF_GETMAX (handle_t *handle);
handle_t  VF_GETOPTION (handle_t *handle);
handle_t  VF_GETLINK (handle_t *handle);
handle_t  VF_GETCOOSYS (handle_t *handle);

handle_t  VF_NEWRESOURCE (handle_t *parent_h);
handle_t  VF_NEWTABLE (handle_t *parent_h);
handle_t  VF_NEWFIELD (handle_t *parent_h);
handle_t  VF_NEWDATA (handle_t *parent_h);
handle_t  VF_NEWTABLEDATA (handle_t *parent_h);
handle_t  VF_NEWTR (handle_t *parent_h);
handle_t  VF_NEWTD (handle_t *parent_h);
handle_t  VF_NEWBINARY (handle_t *parent_h);
handle_t  VF_NEWBINARY2 (handle_t *parent_h);
handle_t  VF_NEWFITS (handle_t *parent_h);
handle_t  VF_NEWGROUP (handle_t *parent_h);
handle_t  VF_NEWFIELDRef (handle_t *parent_h);
handle_t  VF_NEWPARAMREF (handle_t *parent_h);
handle_t  VF_NEWDESCRIPTION (handle_t *parent_h);
handle_t  VF_NEWPARAM (handle_t *parent_h);
handle_t  VF_NEWINFO (handle_t *parent_h);
handle_t  VF_NEWSTREAM (handle_t *parent_h);
handle_t  VF_NEWVALUES (handle_t *parent_h);
handle_t  VF_NEWMIN (handle_t *parent_h);
handle_t  VF_NEWMAX (handle_t *parent_h);
handle_t  VF_NEWOPTION (handle_t *parent_h);
handle_t  VF_NEWLINK (handle_t *parent_h);
handle_t  VF_NEWCOOSYS (handle_t *parent_h);

int 	  VF_GETDATAType (handle_t *data);
void 	  VF_GETDATATYPESTR (handle_t *data, char *type, int *len, int tlen);

handle_t  VF_NEWNODE (handle_t *parent, int *type);
void 	  VF_ATTACHNODE (handle_t *parent, handle_t *new);
void 	  VF_FREENODE (handle_t *elem);
void 	  VF_DELETENODE (handle_t *elem);
handle_t  VF_COPYELEMENT (handle_t *src, handle_t *parent);
;
int 	  VF_GETNCOLS (handle_t *tdata);
int 	  VF_GETNROWS (handle_t *tdata);
void 	  VF_GETTABLECELL (handle_t *tdata, int *row, int *col, char *value, 
		int *maxch);
int 	  VF_GETTABLEINT (handle_t *tdata, int *row, int *col);
float 	  VF_GETTABLEREAL (handle_t *tdata, int *row, int *col);
int 	  VF_GETLENGTH (handle_t *elem);
int 	  VF_GETNUMBEROF (handle_t *elem, int *type);
handle_t  VF_FINDBYATTR (handle_t *parent, char *name, char *value, int nlen, 
		int vlen);
handle_t  VF_FINDINGROUP (handle_t *group, int *type);
handle_t  VF_NEXTINGROUP (void);
handle_t  VF_GETNEXT (handle_t *elem);
handle_t  VF_GETSIBLING (handle_t *elem);
handle_t  VF_GETCHILD (handle_t *elem);
handle_t  VF_GETPARENT (handle_t *elem);
handle_t  VF_CHILDOFTYPE (handle_t *elem, int *type);
int 	  VF_VALUEOF (handle_t *elem);
int 	  VF_TYPEOF (handle_t *elem);
;
int 	  VF_SETVALUE (handle_t *elem, char *value, int vlen);
void 	  VF_GETVALUE (handle_t *elem, char *value, int *maxch, int vlen);
int 	  VF_GETINTVALUE (handle_t *elem);
float 	  VF_GETREALVALUE (handle_t *elem);
void 	  VF_GETATTR (handle_t *elem, char *name, char *val, int *len, 
		int nlen, int vlen);
int 	  VF_SETATTR (handle_t *elem, char *attr, char *value, int alen, 
		int vlen);
void 	  VF_WRITEXML (handle_t *elem, char *fname, int flen);
void 	  VF_WRITEHTML (handle_t *elem, char *ifname, char *ofname, 
		int iflen, int oflen);
void 	  VF_WRITESHTML (handle_t *elem, char *ifname, char *ofname, 
		int iflen, int oflen);
void 	  VF_WRITEASV (handle_t *elem, char *fname, int flen);
void 	  VF_WRITEBSV (handle_t *elem, char *fname, int flen);
void 	  VF_WRITECSV (handle_t *elem, char *fname, int flen);
void 	  VF_WRITETSV (handle_t *elem, char *fname, int flen);
void 	  VF_SETWARN (int *value);



/** 
 *  Local interface declarations.
 */
static char *sstrip (char *instr, int len);
static void  spad (char *outstr, int len);

static handle_t	handle     = 0;			/* global value		*/
static handle_t *s_group   = (handle_t *) NULL;



/*****************************************************************************
 *
 ****************************************************************************/


/** VF_OPENVOTABLE
 *
 *  @brief		This will create a tree holding the VOTable build from
 *  			an XML file parser or just a blank VOTable.
 *  @param[in] *arg 	the source of the table.
 *  @return	 	The root node of the VOTable.
 */
handle_t
VF_OPENVOTABLE (char *arg, int alen)
{
    char *_arg = sstrip (arg, alen);

    handle = vot_openVOTABLE (_arg);

    if (_arg) free ((char *) _arg);

    return (handle);
}


/** VF_CLOSEVOTABLE
 *
 *  @brief		Destroys the node and all of it's children.
 *  @param[in] vot 	A handle to the Element that you want deleted.
 *  @warning 		Destroys the node and all of it's children.
 */
void
VF_CLOSEVOTABLE (handle_t *vot)
{
    vot_closeVOTABLE (*vot);
}




/*****************************************************************************
 *  Routines to get nodes of a VOTable as a handle.
 ****************************************************************************/

/** VF_GETRESOURCE
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETRESOURCE (handle_t *handle)
{
    return ( vot_getRESOURCE (*handle) );
}


/** VF_GETTABLE
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETTABLE (handle_t *handle)
{
    return ( vot_getTABLE (*handle) );
}


/** VF_GETFIELD
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETFIELD (handle_t *handle)
{
    return ( vot_getFIELD (*handle) );
}


/** VF_GETDATA
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETDATA (handle_t *handle)
{
    return ( vot_getDATA (*handle) );
}


/** VF_GETTABLEDATA
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETTABLEDATA (handle_t *handle)
{
    return ( vot_getTABLEDATA (*handle) );
}


/** VF_GETTR
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETTR (handle_t *handle)
{
    return ( vot_getTR (*handle) );
}


/** VF_GETTD
 * 
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETTD (handle_t *handle)
{
    return ( vot_getTD (*handle) );
}


/** VF_GETBINARY
 * 
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETBINARY (handle_t *handle)
{
    return ( vot_getBINARY (*handle) );
}


/** VF_GETBINARY2
 * 
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETBINARY2 (handle_t *handle)
{
    return ( vot_getBINARY2 (*handle) );
}


/** VF_GETFITS
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETFITS (handle_t *handle)
{
    return ( vot_getFITS (*handle) );
}


/** VF_GETGROUP
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETGROUP (handle_t *handle)
{
    return ( vot_getGROUP (*handle) );
}


/** VF_GETFIELDRef
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETFIELDRef (handle_t *handle)
{
    return ( vot_getFIELDRef (*handle) );
}


/** VF_GETPARAMREF
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETPARAMREF (handle_t *handle)
{
    return ( vot_getPARAMRef (*handle) );
}


/** VF_GETDESCRIPTION
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETDESCRIPTION (handle_t *handle)
{
    return ( vot_getDESCRIPTION (*handle) );
}


/** VF_GETPARAM
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETPARAM (handle_t *handle)
{
    return ( vot_getPARAM (*handle) );
}


/** VF_GETINFO
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETINFO (handle_t *handle)
{
    return ( vot_getINFO (*handle) );
}


/** VF_GETSTREAM
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETSTREAM (handle_t *handle)
{
    return ( vot_getSTREAM (*handle) );
}


/** VF_GETVALUES
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETVALUES (handle_t *handle)
{
    return ( vot_getVALUES (*handle) );
}


/** VF_GETMIN
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETMIN (handle_t *handle)
{
    return ( vot_getMIN (*handle) );
}


/** VF_GETMAX
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETMAX (handle_t *handle)
{
    return ( vot_getMAX (*handle) );
}


/** VF_GETOPTION
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETOPTION (handle_t *handle)
{
    return ( vot_getOPTION (*handle) );
}


/** VF_GETLINK
 * 
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETLINK (handle_t *handle)
{
    return ( vot_getLINK (*handle) );
}


/** VF_GETCOOSYS
 * 
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VF_GETCOOSYS (handle_t *handle)
{
    return ( vot_getCOOSYS (*handle) );
}



/*************************************************************************/

/** VF_GETDATAType
 * 
 *  @brief	Returns the type of the DATA element.
 *  @param[in] data 	A handle to a DATA element
 *  @return	The type as an int.
 */
int
VF_GETDATAType (handle_t *data)
{
    return ( vot_getDATAType (*data) );
}


/** VF_GETDATATypeString
 * 
 *  @brief	Returns the type of the DATA element.
 *  @param[in] data 	A handle to a DATA element
 *  @return	The type as an string.
 */
void
VF_GETDATATYPESTR (handle_t *data, char *type, int *len, int tlen)
{
    char *_val = vot_getDATATypeString (*data);

    memset (type, 0, *len);
    spad ( strncpy (type, _val, *len), *len );
}




/*****************************************************************************
 *  Routines to create new nodes of a VOTable.
 ****************************************************************************/

/** VF_NEWRESOURCE
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWRESOURCE (handle_t *parent_h)
{
    return ( vot_newRESOURCE (*parent_h) );
}


/** VF_NEWTABLE
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWTABLE (handle_t *parent_h)
{
    return ( vot_newTABLE (*parent_h) );
}


/** VF_NEWFIELD
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWFIELD (handle_t *parent_h)
{
    return ( vot_newFIELD (*parent_h) );
}


/** VF_NEWDATA
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWDATA (handle_t *parent_h)
{
    return ( vot_newDATA (*parent_h) );
}


/** VF_NEWTABLEDATA
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWTABLEDATA (handle_t *parent_h)
{
    return ( vot_newTABLEDATA (*parent_h) );
}


/** VF_NEWTR
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWTR (handle_t *parent_h)
{
    return ( vot_newTR (*parent_h) );
}


/** VF_NEWTD
 * 
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWTD (handle_t *parent_h)
{
    return ( vot_newTD (*parent_h) );
}


/** VF_NEWBINARY
 * 
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWBINARY (handle_t *parent_h)
{
    return ( vot_newBINARY (*parent_h) );
}


/** VF_NEWBINARY2
 * 
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWBINARY2 (handle_t *parent_h)
{
    return ( vot_newBINARY2 (*parent_h) );
}


/** VF_NEWFITS
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWFITS (handle_t *parent_h)
{
    return ( vot_newFITS (*parent_h) );
}


/** VF_NEWGROUP
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWGROUP (handle_t *parent_h)
{
    return ( vot_newGROUP (*parent_h) );
}


/** VF_NEWFIELDRef
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWFIELDRef (handle_t *parent_h)
{
    return ( vot_newFIELDRef (*parent_h) );
}


/** VF_NEWPARAMREF
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWPARAMREF (handle_t *parent_h)
{
    return ( vot_newPARAMRef (*parent_h) );
}


/** VF_NEWDESCRIPTION
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWDESCRIPTION (handle_t *parent_h)
{
    return ( vot_newDESCRIPTION (*parent_h) );
}


/** VF_NEWPARAM
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWPARAM (handle_t *parent_h)
{
    return ( vot_newPARAM (*parent_h) );
}


/** VF_NEWINFO
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWINFO (handle_t *parent_h)
{
    return ( vot_newINFO (*parent_h) );
}


/** VF_NEWSTREAM
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWSTREAM (handle_t *parent_h)
{
    return ( vot_newSTREAM (*parent_h) );
}


/** VF_NEWVALUES
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWVALUES (handle_t *parent_h)
{
    return ( vot_newVALUES (*parent_h) );
}


/** VF_NEWMIN
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWMIN (handle_t *parent_h)
{
    return ( vot_newMIN (*parent_h) );
}


/** VF_NEWMAX
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWMAX (handle_t *parent_h)
{
    return ( vot_newMAX (*parent_h) );
}


/** VF_NEWOPTION
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWOPTION (handle_t *parent_h)
{
    return ( vot_newOPTION (*parent_h) );
}


/** VF_NEWLINK
 * 
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWLINK (handle_t *parent_h)
{
    return ( vot_newLINK (*parent_h) );
}


/** VF_NEWCOOSYS
 * 
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VF_NEWCOOSYS (handle_t *parent_h)
{
    return ( vot_newCOOSYS (*parent_h) );
}



/*****************************************************************************
 ****************************************************************************/


/** VF_NEWNODE
 * 
 *  @brief		Creates a new blank unlinked node.
 *  @param[in] parent 	A handle to the Element that you want to add a node to.
 *  @param[in] type 	The type of node you wish to create.
 *  @return 		A handle to the created node.
 */
handle_t
VF_NEWNODE (handle_t *parent, int *type)
{
    return ( vot_newNode (*parent, *type) );
}


/** VF_ATTACHNODE
 *
 *  @brief		Adds a node as a child of parent.
 *  @param[in] parent 	A handle to the Element that you want to add a node to.
 *  @param[in] new 	A handle to the Element that you want to add.
 */
void
VF_ATTACHNODE (handle_t *parent, handle_t *new)
{
    vot_attachNode (*parent, *new);
}


/** VF_FREENODE
 *
 *  @brief		Destroys the node and all of it's children.
 *  @param[in] elem 	A handle to the Element that you want deleted.
 *  @warning 		Destroys the node and all of it's children.
 */
void
VF_FREENODE (handle_t *elem)
{
    vot_freeNode (*elem);
}


/** VF_DELETENODE
 *
 *  @brief		Destroys the node and all of it's children.
 *  @param[in] elem 	A handle to the Element that you want deleted.
 *  @warning 		Destroys the node and all of it's children.
 */
void
VF_DELETENODE (handle_t *elem)
{
    vot_deleteNode (*elem);
}


/** VF_COPYELEMENT
 *
 *  @brief		Adds a node as a child of parent.
 *  @param[in] src 	A handle to the Element to copy.
 *  @param[in] parent 	A handle to the Elements parent.
 *  @return 		A handle of the copy of the structure.
 */
handle_t
VF_COPYELEMENT (handle_t *src, handle_t *parent)
{
    return ( vot_copyElement (*src, *parent) );
}


/*****************************************************************************
 *  Utility methods
 ****************************************************************************/


/** VF_GETNCOLS
 * 
 *  @brief		Return the nuber of columns in the structure.
 *  @param[in]  tdata 	A handle to a TABLEDATA
 *  @return		The number of cols.
 */
int
VF_GETNCOLS (handle_t *tdata)
{
    return ( vot_getNCols (*tdata) );
}


/** VF_GETNROWS
 * 
 *  @brief		Return the nuber of columns in the structure.
 *  @param[in]  tdata 	A handle to a TABLEDATA
 *  @return		The number of cols.
 */
int
VF_GETNROWS (handle_t *tdata)
{
    return ( vot_getNRows (*tdata) );
}


/** VF_GETTABLECELL
 * 
 *  @brief		Return the nuber of contents of a table cell 
 *  @param[in]  tdata 	A handle to a TABLEDATA.
 *  @param[in]  row 	An int for a row.
 *  @param[in]  col 	An int for a col.
 *  @return		The content of the cell.
 */
void
VF_GETTABLECELL (handle_t *tdata, int *row, int *col, char *value, int *maxch)
{
    char *_val = vot_getTableCell (*tdata, (*row - 1), (*col - 1));

    memset (value, 0, *maxch);
    spad ( strncpy (value, _val, *maxch), *maxch);
}


/** VF_GETTABLEINT
 * 
 *  @brief		Return the nuber of contents of a table cell as an int
 *  @param[in]  tdata 	A handle to a TABLEDATA.
 *  @param[in]  row 	An int for a row.
 *  @param[in]  col 	An int for a col.
 *  @return		The content of the cell as an int.
 */
int
VF_GETTABLEINT (handle_t *tdata, int *row, int *col)
{
    return ( atoi (vot_getTableCell (*tdata, (*row - 1), (*col - 1))) );
}


/** VF_GETTABLEREAL
 * 
 *  @brief		Return the nuber of contents of a table cell as an real
 *  @param[in]  tdata 	A handle to a TABLEDATA.
 *  @param[in]  row 	An int for a row.
 *  @param[in]  col 	An int for a col.
 *  @return		The content of the cell as a real.
 */
float
VF_GETTABLEREAL (handle_t *tdata, int *row, int *col)
{
    return ( atof (vot_getTableCell (*tdata, (*row - 1), (*col - 1))) );
}


/** VF_GETLENGTH
 * 
 *  @brief		Return the number of sibling Elements of the same type.
 *  @param[in]  elem 	A handle the Element.
 *  @return		The status of the set.
 */
int
VF_GETLENGTH (handle_t *elem)
{
    return ( vot_getLength (*elem) );
}


/** VF_GETNUMBEROF
 *
 *  @brief		Return the number of sibling Elements of the type.
 *  @param[in]  elem 	A handle the Element.
 *  @param[in]  type 	An int of the type of element you wish to count.
 *  @return		The status of the set.
 */
int
VF_GETNUMBEROF (handle_t *elem, int *type)
{
    return ( vot_getNumberOf (*elem, *type) );
}


/** VF_FINDBYATTR.
 * 
 *  @brief		Return a handle to an Element with the requested attr.
 *  @param[in]  parent 	A handle the parent Element.
 *  @param[in]  name 	A string holding the Value type.
 *  @param[in]  value 	A string holding the Value value.
 *  @return		The handle to the element.
 */
handle_t
VF_FINDBYATTR (handle_t *parent, char *name, char *value, int nlen, int vlen)
{

    return ( vot_findByAttr (*parent, name, value) );
}


/** VF_FINDINGROUP.
 * 
 *  @brief		Return an array of handle_ts of the requested type.
 *  @param[in]  group 	A handle the parent Element.
 *  @param[in]  type 	Value of the type.
 *  @return		An array of handles.
 */
handle_t
VF_FINDINGROUP (handle_t *group, int *type)
{
    s_group = vot_findInGroup (*group, *type);
    return ( (handle_t) *s_group );
}


/** VF_NEXTINGROUP.
 * 
 *  @brief		Return an array of handle_ts of the requested type.
 *  @param[in]  group 	A handle the parent Element.
 *  @param[in]  type 	Value of the type.
 *  @return		An array of handles.
 */
handle_t
VF_NEXTINGROUP (void)
{
    s_group++;
    return ( (handle_t) *s_group );
}


/** VF_GETNEXT
 *  
 *  @brief		Return a handle of the next Element of the same type.
 *  @param[in]  elem 	A handle the Element.
 *  @return		A handle of the next Element of the same type.
 */
handle_t
VF_GETNEXT (handle_t *elem)
{
    return ( vot_getNext (*elem) );
}


/** VF_GETSIBLING.
 * 
 *  @brief		Return a handle of the next Element.
 *  @param[in]  elem 	A handle the Element.
 *  @return		A handle of the next Element.
 */
handle_t
VF_GETSIBLING (handle_t *elem)
{
    return ( vot_getSibling (*elem) );
}


/** VF_GETCHILD
 *
 *  @brief		Return a handle of the child Element.
 *  @param[in]  elem 	A handle the Element.
 *  @return		A handle of the child Element.
 */
handle_t
VF_GETCHILD (handle_t *elem)
{
    return ( vot_getChild (*elem) );
}


/** VF_GETPARENT
 *
 *  @brief		Return a handle of the parent Element.
 *  @param[in]  elem 	A handle the Element.
 *  @return		A handle of the paretn Element.
 */
handle_t
VF_GETPARENT (handle_t *elem)
{
    return ( vot_getParent (*elem) );
}


/** VF_GETCHILDOfTYPE
 *  @brief		Return a handle of the next Element of the same type.
 *  @param[in]  elem 	A handle the Element.
 *  @param[in]  type 	An integer of the Element type for find.
 *  @return		A handle of the Element.
 */
handle_t
VF_CHILDOFTYPE (handle_t *elem, int *type)
{
    return ( vot_getChildOfType (*elem, *type) );
}


/** VF_VALUEOF
 *
 *  @brief 		Return type of the Element.
 *  @param[in]  elem 	A handle the Element.
 *  @return		An integer of the type.
 */
int
VF_VALUEOF (handle_t *elem)
{
    return ( vot_valueOf (*elem) );
}


/** VF_TYPEOF
 *
 *  @brief 		Return type of the Element.
 *  @param[in]  elem 	A handle the Element.
 *  @return		An integer of the type.
 */
int
VF_TYPEOF (handle_t *elem)
{
    return ( vot_typeOf (*elem) );
}


/** VF_SETWARN
 * 
 *  @brief		Set the warning level
 *  @param[in]  value 	Warning level
 *  @return		Nothing
 */
void
VF_SETWARN (int *value)
{
    vot_setWarnings (*value);
}



/****************************************************************************
 *
 ***************************************************************************/

/** VF_SETVALUE
 *
 *  @brief		Return the Value for the ELEMENT.
 *  @param[in]  elem 	A handle the ELEMENT.
 *  @param[in]  value 	A string holding the Value value.
 *  @return 		The status of the set.
 */
int
VF_SETVALUE (handle_t *elem, char *value, int vlen)
{
    int  retval = -1;

    char *_value = sstrip (value, vlen);

    retval = vot_setValue (*elem, _value);

    if (_value) free ((char *) _value);

    return (retval);
}


/** VF_GETVALUE
 *
 *  @brief		Return the Value for the ELEMENT.
 *  @param[in]  elem 	A handle the ELEMENT.
 *  @return 		A string of the value or the Value.
 */
void
VF_GETVALUE (handle_t *elem, char *value, int *maxch, int vlen)
{
    char *_val = vot_getValue (*elem);

    memset (value, 0, *maxch);
    spad ( strncpy (value, _val, *maxch), *maxch);
}


/** VF_GETINTVALUE
 *
 *  @brief		Return the Value for the ELEMENT as an int.
 *  @param[in]  elem 	A handle the ELEMENT.
 *  @return 		An int value
 */
int
VF_GETINTVALUE (handle_t *elem)
{
    return ( atoi (vot_getValue (*elem)) );
}


/** VF_GETREALVALUE
 *
 *  @brief		Return the Value for the ELEMENT.
 *  @param[in]  elem 	A handle the ELEMENT.
 *  @return 		A real value
 */
float
VF_GETREALVALUE (handle_t *elem)
{
    return ( atof (vot_getValue (*elem)) );
}


/** VF_GETATTR
 * 
 *  @brief		Return the attribute for the Element.
 *  @param[in]  elem 	A handle the Element.
 *  @param[in]  attr 	A string holding the attribute name.
 *  @return		A string of the value or the attr.
 */
void
VF_GETATTR (handle_t *elem, char *name, char *val, int *len, int nlen, int vlen)
{
    char *_name  = sstrip (name, nlen);
    char *res  = vot_getAttr (*elem, _name);

    memset (val, 0, *len);

    if (res) {
        int rlen = strlen (res); 		/*  found a value */
        strncpy (val, res, rlen);
        spad (val, vlen);
    }

    free ((char *) _name);
}


/** VF_SETATTR
 * 
 *  @brief		Return the attribute for the Element.
 *  @param[in]  elem 	A handle the Element.
 *  @param[in]  attr 	A string holding the attribute name.
 *  @param[in]  value 	A string holding the attribute value.
 *  @return		The status of the set.
 */
int
VF_SETATTR (handle_t *elem, char *attr, char *value, int alen, int vlen)
{
    int  retval = -1;

    char *_attr  = sstrip (attr, alen);
    char *_value = sstrip (value, vlen);

    retval = vot_setAttr (*elem, _attr, _value);

    if (_attr)  free ((char *) _attr);
    if (_value) free ((char *) _value);

    return (retval);
}


/** VF_WRITEXML
 * 
 *  @brief		Dump the XML to the named file (or stdout).
 *  @param[in]  elem 	A handle the root node
 *  @param[in]  fname 	Output file name
 *  @param[in]  flen 	Length of utput file name
 *  @return		Nothing
 */
void
VF_WRITEXML (handle_t *elem, char *fname, int flen)
{
    char *_fname  = sstrip (fname, flen);

    vot_writeVOTable (*elem, _fname, 0);

    free ((char *) _fname);
}



/** VF_WRITEHTML
 * 
 *  @brief              Print the XML to the named file (or stdout)
 *  @param[in]  elem    A handle the root Element.
 *  @param[in]  fname   Output file name
 */
void
VF_WRITEHTML (handle_t *elem, char *ifname, char *ofname, int iflen, int oflen)
{
    char *_ifname = sstrip (ifname, iflen);
    char *_ofname = sstrip (ofname, oflen);

    vot_writeHTML (*elem, _ifname, _ofname);

    free ((char *) _ifname);
    free ((char *) _ofname);
}



/** VF_WRITESHTML
 * 
 *  @brief              Print the XML to the named file (or stdout)
 *  @param[in]  elem    A handle the root Element.
 *  @param[in]  fname   Output file name
 */
void
VF_WRITESHTML (handle_t *elem, char *ifname, char *ofname, int iflen, int oflen)
{
    char *_ifname = sstrip (ifname, iflen);
    char *_ofname = sstrip (ofname, oflen);

    vot_writeSHTML (*elem, _ifname, _ofname);

    free ((char *) _ifname);
    free ((char *) _ofname);
}




/** VF_WRITEASV
 * 
 *  @brief              Print the XML as Ascii-Separated Values
 *  @param[in]  elem    A handle the root Element.
 *  @param[in]  fname   Output file name
 */
void
VF_WRITEASV (handle_t *elem, char *fname, int flen)
{
    char *_fname = sstrip (fname, flen);

    vot_writeASV (*elem, _fname, 1);

    free ((char *) _fname);
}


/** VF_WRITEBSV
 * 
 *  @brief              Print the XML as Ascii-Separated Values
 *  @param[in]  elem    A handle the root Element.
 *  @param[in]  fname   Output file name
 */
void
VF_WRITEBSV (handle_t *elem, char *fname, int flen)
{
    char *_fname = sstrip (fname, flen);

    vot_writeBSV (*elem, _fname, 1);

    free ((char *) _fname);
}


/** VF_WRITECSV
 * 
 *  @brief              Print the XML as Ascii-Separated Values
 *  @param[in]  elem    A handle the root Element.
 *  @param[in]  fname   Output file name
 */
void
VF_WRITECSV (handle_t *elem, char *fname, int flen)
{
    char *_fname = sstrip (fname, flen);

    vot_writeCSV (*elem, _fname, 1);

    free ((char *) _fname);
}


/** VF_WRITEASV
 * 
 *  @brief              Print the XML as Ascii-Separated Values
 *  @param[in]  elem    A handle the root Element.
 *  @param[in]  fname   Output file name
 */
void
VF_WRITETSV (handle_t *elem, char *fname, int flen)
{
    char *_fname = sstrip (fname, flen);

    vot_writeTSV (*elem, _fname, 1);

    free ((char *) _fname);
}






/****************************************************************************
 *  Private utility procedures
 ****************************************************************************/

/**
 *  sstrip
 *
 *  Utility to trim trailing blanks from string and add a null terminator.
 */
static char *
sstrip (char *instr, int len)
{
    if (len > 0 && instr) {
        char *newstr = calloc (1, len+1);
	int i = len;

        strncpy (newstr, instr, len);

	/* trim trailing blanks */
	for (i=len; newstr[i] == ' ' || newstr[i] == '\0'; i--)	
	    newstr[i] = '\0';
	    
        return (newstr);
    }

    return ((char *) NULL);
}


/**
 *  spad 
 *
 *  Pad a string to length 'len' with blanks, as Fortran requires.
 */
void
spad (char *outstr, int len)
{
    int i=0;
        
    for (i = strlen(outstr); i < len; i++)
        outstr[i] = ' ';
}
