/**
 *  VOTPARSE_SPP.C -- Public interface procedures for the VOT SPP wrapper.
 *
 *  @file       votParse_spp.c
 *  @author     Mike Fitzpatrick and Eric Timmermann
 *  @date       8/03/09
 *
 *  @brief      Public interface procedures for the VOT SPP wrapper.
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


/* SPP Type definitions.
*/
#define XCHAR		short
#define PKCHAR		char
#define XINT		int
#define XEOS		0


/*  SPP Interface Definitions.
 *
 *  SPP compilers on various platforms may append one or more trailing
 *  underscores to symbol names, we'll use macros for the interface names
 *  and use defines to see what the symbol name is.
 */
#ifdef _NO_US_

#define VX_OPENVOTABLE   	vopene
#define VX_CLOSEVOTABLE   	vclose

#define VX_GETRESOURCE   	vgetre
#define VX_GETTABLE   		vgette
#define VX_GETFIELD   		vgetfd
#define VX_GETDATA   		vgetda
#define VX_GETTABLEDATA   	vgetta
#define VX_GETTR   		vgettr
#define VX_GETTD   		vgettd
#define VX_GETBINARY   		vgetby
#define VX_GETBINARY2  		vgetb2
#define VX_GETFITS   		vgetfs
#define VX_GETGROUP   		vgetgp
#define VX_GETFIELDREF   	vgetff
#define VX_GETPARAMREF   	vgetpf
#define VX_GETDESCRIPTION   	vgetdn
#define VX_GETPARAM   		vgetpm
#define VX_GETINFO   		vgetio
#define VX_GETSTREAM   		vgetsm
#define VX_GETVALUES   		vgetvs
#define VX_GETMIN   		vgetmn
#define VX_GETMAX   		vgetmx
#define VX_GETOPTION   		vgeton
#define VX_GETLINK   		vgetlk
#define VX_GETCOOSYS   		vgetcs
#define VX_GETDATATYPE   	vgetde
#define VX_GETDATATYPESTR       vgetdr

#define VX_NEWRESOURCE   	vnetre
#define VX_NEWTABLE   		vnette
#define VX_NEWFIELD   		vnetfd
#define VX_NEWDATA   		vnetda
#define VX_NEWTABLEDATA   	vnetta
#define VX_NEWTR   		vnettr
#define VX_NEWTD   		vnettd
#define VX_NEWBINARY   		vnetby
#define VX_NEWBINARY2  		vnetb2
#define VX_NEWFITS   		vnetfs
#define VX_NEWGROUP   		vnetgp
#define VX_NEWFIELDREF   	vnetff
#define VX_NEWPARAMREF   	vnetpf
#define VX_NEWDESCRIPTION   	vnetdn
#define VX_NEWPARAM   		vnetpm
#define VX_NEWINFO   		vnetio
#define VX_NEWSTREAM   		vnetsm
#define VX_NEWVALUES   		vnetvs
#define VX_NEWMIN   		vnetmn
#define VX_NEWMAX   		vnetmx
#define VX_NEWOPTION   		vneton
#define VX_NEWLINK   		vnetlk
#define VX_NEWCOOSYS   		vnetcs

#define VX_NEWNODE   		vnewne
#define VX_FREENODE   		vfreee
#define VX_ATTACHNODE   	vattae
#define VX_DELETENODE   	vdelee
#define VX_COPYELEMENT   	vcopyt
#define VX_GETNCOLS   		vgncol
#define VX_GETNROWS   		vgnrow
#define VX_GETTABLECELL   	vgstab
#define VX_GETTABLEINT   	vgitab
#define VX_GETTABLEREAL   	vgrtab
#define VX_GETLENGTH   		vgetlh
#define VX_GETNUMBEROF   	vgetnf
#define VX_FINDBYATTR   	vfindr
#define VX_FINDINGROUP   	vfindp
#define VX_NEXTINGROUP   	vnextp
#define VX_GETNEXT   		vgetnt
#define VX_GETSIBLING		vgetsg
#define VX_GETCHILD   		vgetcd
#define VX_GETPARENT   		vgetpt
#define VX_CHILDOFTYPE   	vchile
#define VX_VALUEOF   		vvaluf
#define VX_TYPEOF   		vtypef
#define VX_SETVALUE   		vsetve
#define VX_GETVALUE   		vgsval
#define VX_GETINTVALUE   	vgival
#define VX_GETREALVALUE   	vgrval
#define VX_SETATTR   		vsetar
#define VX_GETATTR   		vgetar
#define VX_WRITEXML   		vwrxml
#define VX_WRITEHTML   		vwrhtl
#define VX_WRITESHTML  		vwrshl
#define VX_WRITEASV   		vwrasv
#define VX_WRITEBSV   		vwrbsv
#define VX_WRITECSV   		vwrcsv
#define VX_WRITETSV   		vwrtsv
#define VX_WRITEFITS		vwrfis
#define VX_SETWARN   		vswarn

#else

#define VX_OPENVOTABLE   	vopene_
#define VX_CLOSEVOTABLE   	vclose_

#define VX_GETRESOURCE   	vgetre_
#define VX_GETTABLE   		vgette_
#define VX_GETFIELD   		vgetfd_
#define VX_GETDATA   		vgetda_
#define VX_GETTABLEDATA   	vgetta_
#define VX_GETTR   		vgettr_
#define VX_GETTD   		vgettd_
#define VX_GETBINARY   		vgetby_
#define VX_GETBINARY2  		vgetb2_
#define VX_GETFITS   		vgetfs_
#define VX_GETGROUP   		vgetgp_
#define VX_GETFIELDREF   	vgetff_
#define VX_GETPARAMREF   	vgetpf_
#define VX_GETDESCRIPTION   	vgetdn_
#define VX_GETPARAM   		vgetpm_
#define VX_GETINFO   		vgetio_
#define VX_GETSTREAM   		vgetsm_
#define VX_GETVALUES   		vgetvs_
#define VX_GETMIN   		vgetmn_
#define VX_GETMAX   		vgetmx_
#define VX_GETOPTION   		vgeton_
#define VX_GETLINK   		vgetlk_
#define VX_GETCOOSYS   		vgetcs_
#define VX_GETDATATYPE   	vgetde_
#define VX_GETDATATYPESTR       vgetdr_

#define VX_NEWRESOURCE   	vnetre_
#define VX_NEWTABLE   		vnette_
#define VX_NEWFIELD   		vnetfd_
#define VX_NEWDATA   		vnetda_
#define VX_NEWTABLEDATA   	vnetta_
#define VX_NEWTR   		vnettr_
#define VX_NEWTD   		vnettd_
#define VX_NEWBINARY   		vnetby_
#define VX_NEWBINARY2  		vnetb2_
#define VX_NEWFITS   		vnetfs_
#define VX_NEWGROUP   		vnetgp_
#define VX_NEWFIELDREF   	vnetff_
#define VX_NEWPARAMREF   	vnetpf_
#define VX_NEWDESCRIPTION   	vnetdn_
#define VX_NEWPARAM   		vnetpm_
#define VX_NEWINFO   		vnetio_
#define VX_NEWSTREAM   		vnetsm_
#define VX_NEWVALUES   		vnetvs_
#define VX_NEWMIN   		vnetmn_
#define VX_NEWMAX   		vnetmx_
#define VX_NEWOPTION   		vneton_
#define VX_NEWLINK   		vnetlk_
#define VX_NEWCOOSYS   		vnetcs_

#define VX_NEWNODE   		vnewne_
#define VX_FREENODE   		vfreee_
#define VX_ATTACHNODE   	vattae_
#define VX_DELETENODE   	vdelee_
#define VX_COPYELEMENT   	vcopyt_
#define VX_GETNCOLS   		vgncol_
#define VX_GETNROWS   		vgnrow_
#define VX_GETTABLECELL   	vgstab_
#define VX_GETTABLEINT   	vgitab_
#define VX_GETTABLEREAL   	vgrtab_
#define VX_GETLENGTH   		vgetlh_
#define VX_GETNUMBEROF   	vgetnf_
#define VX_FINDBYATTR   	vfindr_
#define VX_FINDINGROUP   	vfindp_
#define VX_NEXTINGROUP   	vnextp_
#define VX_GETNEXT   		vgetnt_
#define VX_GETSIBLING		vgetsg_
#define VX_GETCHILD   		vgetcd_
#define VX_GETPARENT   		vgetpt_
#define VX_CHILDOFTYPE   	vchile_
#define VX_VALUEOF   		vvaluf_
#define VX_TYPEOF   		vtypef_
#define VX_SETVALUE   		vsetve_
#define VX_GETVALUE   		vgsval_
#define VX_GETINTVALUE   	vgival_
#define VX_GETREALVALUE   	vgrval_
#define VX_SETATTR   		vsetar_
#define VX_GETATTR   		vgetar_
#define VX_WRITEXML   		vwrxml_
#define VX_WRITEHTML   		vwrhtl_
#define VX_WRITESHTML  		vwrshl_
#define VX_WRITEASV   		vwrasv_
#define VX_WRITEBSV   		vwrbsv_
#define VX_WRITECSV   		vwrcsv_
#define VX_WRITETSV   		vwrtsv_
#define VX_WRITEFITS		vwrfis_
#define VX_SETWARN   		vswarn_

#endif


/**
 *   Public function prototypes.
 */
handle_t  VX_OPENVOTABLE (XCHAR *arg);
void 	  VX_CLOSEVOTABLE (handle_t *vot);

handle_t  VX_GETRESOURCE (handle_t *handle);
handle_t  VX_GETTABLE (handle_t *handle);
handle_t  VX_GETFIELD (handle_t *handle);
handle_t  VX_GETDATA (handle_t *handle);
handle_t  VX_GETTABLEDATA (handle_t *handle);
handle_t  VX_GETTR (handle_t *handle);
handle_t  VX_GETTD (handle_t *handle);
handle_t  VX_GETBINARY (handle_t *handle);
handle_t  VX_GETBINARY2 (handle_t *handle);
handle_t  VX_GETFITS (handle_t *handle);
handle_t  VX_GETGROUP (handle_t *handle);
handle_t  VX_GETFIELDRef (handle_t *handle);
handle_t  VX_GETPARAMREF (handle_t *handle);
handle_t  VX_GETDESCRIPTION (handle_t *handle);
handle_t  VX_GETPARAM (handle_t *handle);
handle_t  VX_GETINFO (handle_t *handle);
handle_t  VX_GETSTREAM (handle_t *handle);
handle_t  VX_GETVALUES (handle_t *handle);
handle_t  VX_GETMIN (handle_t *handle);
handle_t  VX_GETMAX (handle_t *handle);
handle_t  VX_GETOPTION (handle_t *handle);
handle_t  VX_GETLINK (handle_t *handle);
handle_t  VX_GETCOOSYS (handle_t *handle);

handle_t  VX_NEWRESOURCE (handle_t *parent_h);
handle_t  VX_NEWTABLE (handle_t *parent_h);
handle_t  VX_NEWFIELD (handle_t *parent_h);
handle_t  VX_NEWDATA (handle_t *parent_h);
handle_t  VX_NEWTABLEDATA (handle_t *parent_h);
handle_t  VX_NEWTR (handle_t *parent_h);
handle_t  VX_NEWTD (handle_t *parent_h);
handle_t  VX_NEWBINARY (handle_t *parent_h);
handle_t  VX_NEWBINARY2 (handle_t *parent_h);
handle_t  VX_NEWFITS (handle_t *parent_h);
handle_t  VX_NEWGROUP (handle_t *parent_h);
handle_t  VX_NEWFIELDRef (handle_t *parent_h);
handle_t  VX_NEWPARAMREF (handle_t *parent_h);
handle_t  VX_NEWDESCRIPTION (handle_t *parent_h);
handle_t  VX_NEWPARAM (handle_t *parent_h);
handle_t  VX_NEWINFO (handle_t *parent_h);
handle_t  VX_NEWSTREAM (handle_t *parent_h);
handle_t  VX_NEWVALUES (handle_t *parent_h);
handle_t  VX_NEWMIN (handle_t *parent_h);
handle_t  VX_NEWMAX (handle_t *parent_h);
handle_t  VX_NEWOPTION (handle_t *parent_h);
handle_t  VX_NEWLINK (handle_t *parent_h);
handle_t  VX_NEWCOOSYS (handle_t *parent_h);

int 	  VX_GETDATAType (handle_t *data);
void 	  VX_GETDATATYPESTR (handle_t *data, XCHAR *type, int *len);

handle_t  VX_NEWNODE (handle_t *parent, int *type);
void 	  VX_ATTACHNODE (handle_t *parent, handle_t *new);
void 	  VX_FREENODE (handle_t *elem);
void 	  VX_DELETENODE (handle_t *elem);
handle_t  VX_COPYELEMENT (handle_t *src, handle_t *parent);

int 	  VX_GETNCOLS (handle_t *tdata);
int 	  VX_GETNROWS (handle_t *tdata);
void 	  VX_GETTABLECELL (handle_t *tdata, int *row, int *col, XCHAR *value, 
		int *maxch);
int 	  VX_GETTABLEINT (handle_t *tdata, int *row, int *col);
float 	  VX_GETTABLEREAL (handle_t *tdata, int *row, int *col);
int 	  VX_GETLENGTH (handle_t *elem);
int 	  VX_GETNUMBEROF (handle_t *elem, int *type);
handle_t  VX_FINDBYATTR (handle_t *parent, XCHAR *name, XCHAR *value);
handle_t  VX_FINDINGROUP (handle_t *group, int *type);
handle_t  VX_NEXTINGROUP (void);
handle_t  VX_GETNEXT (handle_t *elem);
handle_t  VX_GETSIBLING (handle_t *elem);
handle_t  VX_GETCHILD (handle_t *elem);
handle_t  VX_GETPARENT (handle_t *elem);
handle_t  VX_CHILDOFTYPE (handle_t *elem, int *type);
int 	  VX_VALUEOF (handle_t *elem);
int 	  VX_TYPEOF (handle_t *elem);

int 	  VX_SETVALUE (handle_t *elem, XCHAR *value);
void 	  VX_GETVALUE (handle_t *elem, XCHAR *value, int *maxch);
int 	  VX_GETINTVALUE (handle_t *elem);
float 	  VX_GETREALVALUE (handle_t *elem);
void 	  VX_GETATTR (handle_t *elem, XCHAR *name, XCHAR *val, int *len);
int 	  VX_SETATTR (handle_t *elem, XCHAR *attr, XCHAR *value);
void 	  VX_WRITEXML (handle_t *elem, XCHAR *fname);
void 	  VX_WRITEHTML (handle_t *elem, XCHAR *ifname, XCHAR *ofname);
void 	  VX_WRITESHTML (handle_t *elem, XCHAR *ifname, XCHAR *ofname);
void 	  VX_WRITEASV (handle_t *elem, XCHAR *fname);
void 	  VX_WRITEBSV (handle_t *elem, XCHAR *fname);
void 	  VX_WRITECSV (handle_t *elem, XCHAR *fname);
void 	  VX_WRITETSV (handle_t *elem, XCHAR *fname);
void 	  VX_WRITEFITS (handle_t *elem, XCHAR *fname);
void 	  VX_SETWARN (int *value);




/** 
 *  Local interface declarations.
 */
static PKCHAR *spp2c (XCHAR *instr,  int maxch);
static int     c2spp (PKCHAR *instr, XCHAR *outstr, int maxch);
static int     spplen (XCHAR *str);

static handle_t *s_group   = (handle_t *) NULL;



/*****************************************************************************
 *
 ****************************************************************************/


/** VX_OPENVOTABLE
 *
 *  @brief		This will create a tree holding the VOTable build from
 *  			an XML file parser or just a blank VOTable.
 *  @param[in] *arg 	The source of the table.
 *  @return	 	The root node of the VOTable.
 */
handle_t
VX_OPENVOTABLE (XCHAR *arg)
{
    char *_arg = spp2c (arg, spplen (arg));
    handle_t handle = vot_openVOTABLE (_arg);

    if (_arg) free ((char *) _arg);

    return (handle);
}


/** VX_CLOSEVOTABLE
 *
 *  @brief		Destroys the node and all of it's children.
 *  @param[in] vot 	A handle to the Element that you want deleted.
 *  @warning 		Destroys the node and all of it's children.
 */
void
VX_CLOSEVOTABLE (handle_t *vot)
{
    vot_closeVOTABLE (*vot);
}




/*****************************************************************************
 *  Routines to get nodes of a VOTable as a handle.
 ****************************************************************************/

/** VX_GETRESOURCE
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETRESOURCE (handle_t *handle)
{
    return ( vot_getRESOURCE (*handle) );
}


/** VX_GETTABLE
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETTABLE (handle_t *handle)
{
    return ( vot_getTABLE (*handle) );
}


/** VX_GETFIELD
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETFIELD (handle_t *handle)
{
    return ( vot_getFIELD (*handle) );
}


/** VX_GETDATA
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETDATA (handle_t *handle)
{
    return ( vot_getDATA (*handle) );
}


/** VX_GETTABLEDATA
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETTABLEDATA (handle_t *handle)
{
    return ( vot_getTABLEDATA (*handle) );
}


/** VX_GETTR
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETTR (handle_t *handle)
{
    return ( vot_getTR (*handle) );
}


/** VX_GETTD
 * 
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETTD (handle_t *handle)
{
    return ( vot_getTD (*handle) );
}


/** VX_GETBINARY
 * 
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETBINARY (handle_t *handle)
{
    return ( vot_getBINARY (*handle) );
}


/** VX_GETBINARY2
 * 
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETBINARY2 (handle_t *handle)
{
    return ( vot_getBINARY2 (*handle) );
}


/** VX_GETFITS
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETFITS (handle_t *handle)
{
    return ( vot_getFITS (*handle) );
}


/** VX_GETGROUP
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETGROUP (handle_t *handle)
{
    return ( vot_getGROUP (*handle) );
}


/** VX_GETFIELDRef
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETFIELDRef (handle_t *handle)
{
    return ( vot_getFIELDRef (*handle) );
}


/** VX_GETPARAMREF
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETPARAMREF (handle_t *handle)
{
    return ( vot_getPARAMRef (*handle) );
}


/** VX_GETDESCRIPTION
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETDESCRIPTION (handle_t *handle)
{
    return ( vot_getDESCRIPTION (*handle) );
}


/** VX_GETPARAM
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETPARAM (handle_t *handle)
{
    return ( vot_getPARAM (*handle) );
}


/** VX_GETINFO
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETINFO (handle_t *handle)
{
    return ( vot_getINFO (*handle) );
}


/** VX_GETSTREAM
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETSTREAM (handle_t *handle)
{
    return ( vot_getSTREAM (*handle) );
}


/** VX_GETVALUES
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETVALUES (handle_t *handle)
{
    return ( vot_getVALUES (*handle) );
}


/** VX_GETMIN
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETMIN (handle_t *handle)
{
    return ( vot_getMIN (*handle) );
}


/** VX_GETMAX
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETMAX (handle_t *handle)
{
    return ( vot_getMAX (*handle) );
}


/** VX_GETOPTION
 *
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETOPTION (handle_t *handle)
{
    return ( vot_getOPTION (*handle) );
}


/** VX_GETLINK
 * 
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETLINK (handle_t *handle)
{
    return ( vot_getLINK (*handle) );
}


/** VX_GETCOOSYS
 * 
 *  @brief		Gets the node of a specified type.
 *  @param[in] handle 	A handle to the Element will you wish to search from.
 *  @return 		A handle to the found node. Zero otherwise.
 */
handle_t
VX_GETCOOSYS (handle_t *handle)
{
    return ( vot_getCOOSYS (*handle) );
}



/*************************************************************************/

/** VX_GETDATAType
 * 
 *  @brief		Returns the type of the DATA element.
 *  @param[in] data 	A handle to a DATA element
 *  @return		The type as an int.
 */
int
VX_GETDATAType (handle_t *data)
{
    return ( vot_getDATAType (*data) );
}


/** VX_GETDATATypeString
 * 
 *  @brief		Returns the type of the DATA element.
 *  @param[in] data 	A handle to a DATA element
 *  @return		The type as an string.
 */
void
VX_GETDATATYPESTR (handle_t *data, XCHAR *type, int *len)
{
    char *_val = vot_getDATATypeString (*data);

    if (_val)
	(void) c2spp (_val, type, *len);
}



/*****************************************************************************
 *  Routines to create new nodes of a VOTable.
 ****************************************************************************/

/** VX_NEWRESOURCE
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWRESOURCE (handle_t *parent_h)
{
    return ( vot_newRESOURCE (*parent_h) );
}


/** VX_NEWTABLE
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWTABLE (handle_t *parent_h)
{
    return ( vot_newTABLE (*parent_h) );
}


/** VX_NEWFIELD
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWFIELD (handle_t *parent_h)
{
    return ( vot_newFIELD (*parent_h) );
}


/** VX_NEWDATA
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWDATA (handle_t *parent_h)
{
    return ( vot_newDATA (*parent_h) );
}


/** VX_NEWTABLEDATA
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWTABLEDATA (handle_t *parent_h)
{
    return ( vot_newTABLEDATA (*parent_h) );
}


/** VX_NEWTR
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWTR (handle_t *parent_h)
{
    return ( vot_newTR (*parent_h) );
}


/** VX_NEWTD
 * 
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWTD (handle_t *parent_h)
{
    return ( vot_newTD (*parent_h) );
}


/** VX_NEWBINARY
 * 
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWBINARY (handle_t *parent_h)
{
    return ( vot_newBINARY (*parent_h) );
}


/** VX_NEWBINARY2
 * 
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWBINARY2 (handle_t *parent_h)
{
    return ( vot_newBINARY2 (*parent_h) );
}


/** VX_NEWFITS
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWFITS (handle_t *parent_h)
{
    return ( vot_newFITS (*parent_h) );
}


/** VX_NEWGROUP
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWGROUP (handle_t *parent_h)
{
    return ( vot_newGROUP (*parent_h) );
}


/** VX_NEWFIELDRef
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWFIELDRef (handle_t *parent_h)
{
    return ( vot_newFIELDRef (*parent_h) );
}


/** VX_NEWPARAMREF
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWPARAMREF (handle_t *parent_h)
{
    return ( vot_newPARAMRef (*parent_h) );
}


/** VX_NEWDESCRIPTION
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWDESCRIPTION (handle_t *parent_h)
{
    return ( vot_newDESCRIPTION (*parent_h) );
}


/** VX_NEWPARAM
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWPARAM (handle_t *parent_h)
{
    return ( vot_newPARAM (*parent_h) );
}


/** VX_NEWINFO
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWINFO (handle_t *parent_h)
{
    return ( vot_newINFO (*parent_h) );
}


/** VX_NEWSTREAM
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWSTREAM (handle_t *parent_h)
{
    return ( vot_newSTREAM (*parent_h) );
}


/** VX_NEWVALUES
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWVALUES (handle_t *parent_h)
{
    return ( vot_newVALUES (*parent_h) );
}


/** VX_NEWMIN
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWMIN (handle_t *parent_h)
{
    return ( vot_newMIN (*parent_h) );
}


/** VX_NEWMAX
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWMAX (handle_t *parent_h)
{
    return ( vot_newMAX (*parent_h) );
}


/** VX_NEWOPTION
 *
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWOPTION (handle_t *parent_h)
{
    return ( vot_newOPTION (*parent_h) );
}


/** VX_NEWLINK
 * 
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWLINK (handle_t *parent_h)
{
    return ( vot_newLINK (*parent_h) );
}


/** VX_NEWCOOSYS
 * 
 *  @brief		   Create the node of a specified type.
 *  @param[in] parent_h    A handle to the parent node. from.
 *  @return 		   A handle to the new node. Zero otherwise.
 */
handle_t
VX_NEWCOOSYS (handle_t *parent_h)
{
    return ( vot_newCOOSYS (*parent_h) );
}



/*****************************************************************************
 ****************************************************************************/


/** VX_NEWNODE
 * 
 *  @brief		Creates a new blank unlinked node.
 *  @param[in] parent 	A handle to the Element that you want to add a node to.
 *  @param[in] type 	The type of node you wish to create.
 *  @return 		A handle to the created node.
 */
handle_t
VX_NEWNODE (handle_t *parent, int *type)
{
    return ( vot_newNode (*parent, *type) );
}


/** VX_ATTACHNODE
 *
 *  @brief		Adds a node as a child of parent.
 *  @param[in] parent 	A handle to the Element that you want to add a node to.
 *  @param[in] new 	A handle to the Element that you want to add.
 */
void
VX_ATTACHNODE (handle_t *parent, handle_t *new)
{
    vot_attachNode (*parent, *new);
}


/** VX_FREENODE
 *
 *  @brief		Destroys the node and all of it's children.
 *  @param[in] elem 	A handle to the Element that you want deleted.
 *  @warning 		Destroys the node and all of it's children.
 */
void
VX_FREENODE (handle_t *elem)
{
    vot_freeNode (*elem);
}


/** VX_DELETENODE
 *
 *  @brief		Destroys the node and all of it's children.
 *  @param[in] elem 	A handle to the Element that you want deleted.
 *  @warning 		Destroys the node and all of it's children.
 */
void
VX_DELETENODE (handle_t *elem)
{
    vot_deleteNode (*elem);
}


/** VX_COPYELEMENT
 *
 *  @brief		Adds a node as a child of parent.
 *  @param[in] src 	A handle to the Element to copy.
 *  @param[in] parent 	A handle to the Elements parent.
 *  @return 		A handle of the copy of the structure.
 */
handle_t
VX_COPYELEMENT (handle_t *src, handle_t *parent)
{
    return ( vot_copyElement (*src, *parent) );
}


/*****************************************************************************
 *  Utility methods
 ****************************************************************************/


/** VX_GETNCOLS
 * 
 *  @brief		Return the nuber of columns in the structure.
 *  @param[in]  tdata 	A handle to a TABLEDATA
 *  @return		The number of cols.
 */
int
VX_GETNCOLS (handle_t *tdata)
{
    return ( vot_getNCols (*tdata) );
}


/** VX_GETNROWS
 * 
 *  @brief		Return the nuber of columns in the structure.
 *  @param[in]  tdata 	A handle to a TABLEDATA
 *  @return		The number of cols.
 */
int
VX_GETNROWS (handle_t *tdata)
{
    return ( vot_getNRows (*tdata) );
}


/** VX_GETTABLECELL
 * 
 *  @brief		Return the nuber of contents of a table cell 
 *  @param[in]  tdata 	A handle to a TABLEDATA.
 *  @param[in]  row 	An int for a row.
 *  @param[in]  col 	An int for a col.
 *  @return		The content of the cell.
 */
void
VX_GETTABLECELL (handle_t *tdata, int *row, int *col, XCHAR *value, int *maxch)
{
    char *_val = vot_getTableCell (*tdata, *row, *col);

    if (_val)
	(void) c2spp (_val, value, *maxch);
}


/** VX_GETTABLEINT
 * 
 *  @brief		Return the nuber of contents of a table cell as an int
 *  @param[in]  tdata 	A handle to a TABLEDATA.
 *  @param[in]  row 	An int for a row.
 *  @param[in]  col 	An int for a col.
 *  @return		The content of the cell as an int.
 */
int
VX_GETTABLEINT (handle_t *tdata, int *row, int *col)
{
    return ( atoi (vot_getTableCell (*tdata, *row, *col)) );
}


/** VX_GETTABLEREAL
 * 
 *  @brief		Return the nuber of contents of a table cell as an real
 *  @param[in]  tdata 	A handle to a TABLEDATA.
 *  @param[in]  row 	An int for a row.
 *  @param[in]  col 	An int for a col.
 *  @return		The content of the cell as a real.
 */
float
VX_GETTABLEREAL (handle_t *tdata, int *row, int *col)
{
    return ( atof (vot_getTableCell (*tdata, *row, *col)) );
}


/** VX_GETLENGTH
 * 
 *  @brief		Return the number of sibling Elements of the same type.
 *  @param[in]  elem 	A handle the Element.
 *  @return		The status of the set.
 */
int
VX_GETLENGTH (handle_t *elem)
{
    return ( vot_getLength (*elem) );
}


/** VX_GETNUMBEROF
 *
 *  @brief		Return the number of sibling Elements of the type.
 *  @param[in]  elem 	A handle the Element.
 *  @param[in]  type 	An int of the type of element you wish to count.
 *  @return		The status of the set.
 */
int
VX_GETNUMBEROF (handle_t *elem, int *type)
{
    return ( vot_getNumberOf (*elem, *type) );
}


/** VX_COLBYATTR.
 * 
 *  @brief		Return column number of requested attribute.
 *  @param[in]  tab 	A handle the parent <TABLE> element.
 *  @param[in]  attr 	A string holding the attribute name
 *  @param[in]  name 	A string holding the first-choice attribute value
 *  @param[in]  alt 	A string holding the alternate attribute value
 *  @return		The handle to the element.
 */
int
VX_COLBYATTR (handle_t *parent, XCHAR *attr, XCHAR *name, XCHAR *alt)
{
    char *_attr = spp2c (attr, spplen (attr));
    char *_name = spp2c (name, spplen (name));
    char *_alt = spp2c (alt, spplen (alt));

    int col = vot_colByAttr (*parent, _attr, _name, _alt);

    free ((char *) _attr);
    free ((char *) _name);
    free ((char *) _alt);

    return ( col );
}


/** VX_COLBYNAME.
 * 
 *  @brief		Return column number of requested attribute.
 *  @param[in]  tab 	A handle the parent <TABLE> element.
 *  @param[in]  name 	A string holding the first-choice attribute value
 *  @param[in]  value 	A string holding the alternate attribute value
 *  @return		The handle to the element.
 */
int
VX_COLBYNAME (handle_t *parent, XCHAR *name, XCHAR *alt)
{
    char *_name = spp2c (name, spplen (name));
    char *_alt = spp2c (alt, spplen (alt));

    int col = vot_colByName (*parent, _name, _alt);

    free ((char *) _name);
    free ((char *) _alt);

    return ( col );
}


/** VX_COLBYUCD.
 * 
 *  @brief		Return column number of requested attribute.
 *  @param[in]  tab 	A handle the parent <TABLE> element.
 *  @param[in]  name 	A string holding the first-choice attribute value
 *  @param[in]  value 	A string holding the alternate attribute value
 *  @return		The handle to the element.
 */
int
VX_COLBYUCD (handle_t *parent, XCHAR *name, XCHAR *alt)
{
    char *_name = spp2c (name, spplen (name));
    char *_alt = spp2c (alt, spplen (alt));

    int col = vot_colByUCD (*parent, _name, _alt);

    free ((char *) _name);
    free ((char *) _alt);

    return ( col );
}


/** VX_COLBYID.
 * 
 *  @brief		Return column number of requested attribute.
 *  @param[in]  tab 	A handle the parent <TABLE> element.
 *  @param[in]  name 	A string holding the first-choice attribute value
 *  @param[in]  value 	A string holding the alternate attribute value
 *  @return		The handle to the element.
 */
int
VX_COLBYID (handle_t *parent, XCHAR *name, XCHAR *alt)
{
    char *_name = spp2c (name, spplen (name));
    char *_alt = spp2c (alt, spplen (alt));

    int col = vot_colByID (*parent, _name, _alt);

    free ((char *) _name);
    free ((char *) _alt);

    return ( col );
}


/** VX_FINDBYATTR.
 * 
 *  @brief		Return a handle to an Element with the requested attr.
 *  @param[in]  parent 	A handle the parent Element.
 *  @param[in]  name 	A string holding the Value type.
 *  @param[in]  value 	A string holding the Value value.
 *  @return		The handle to the element.
 */
handle_t
VX_FINDBYATTR (handle_t *parent, XCHAR *name, XCHAR *value)
{
    char *_name = spp2c (name, spplen (name));
    char *_value = spp2c (value, spplen (value));

    int handle = vot_findByAttr (*parent, _name, _value);

    free ((char *) _name);
    free ((char *) _value);

    return ( handle );
}


/** VX_FINDINGROUP.
 * 
 *  @brief		Return an array of handle_ts of the requested type.
 *  @param[in]  group 	A handle the parent Element.
 *  @param[in]  type 	Value of the type.
 *  @return		An array of handles.
 */
handle_t
VX_FINDINGROUP (handle_t *group, int *type)
{
    s_group = vot_findInGroup (*group, *type);
    return ( (handle_t) *s_group );
}


/** VX_NEXTINGROUP.
 * 
 *  @brief		Return an array of handle_ts of the requested type.
 *  @param[in]  group 	A handle the parent Element.
 *  @param[in]  type 	Value of the type.
 *  @return		An array of handles.
 */
handle_t
VX_NEXTINGROUP (void)
{
    s_group++;
    return ( (handle_t) *s_group );
}


/** VX_GETNEXT
 *  
 *  @brief		Return a handle of the next Element of the same type.
 *  @param[in]  elem 	A handle the Element.
 *  @return		A handle of the next Element of the same type.
 */
handle_t
VX_GETNEXT (handle_t *elem)
{
    return ( vot_getNext (*elem) );
}


/** VX_GETSIBLING.
 * 
 *  @brief		Return a handle of the next Element.
 *  @param[in]  elem 	A handle the Element.
 *  @return		A handle of the next Element.
 */
handle_t
VX_GETSIBLING (handle_t *elem)
{
    return ( vot_getSibling (*elem) );
}


/** VX_GETCHILD
 *
 *  @brief		Return a handle of the child Element.
 *  @param[in]  elem 	A handle the Element.
 *  @return		A handle of the child Element.
 */
handle_t
VX_GETCHILD (handle_t *elem)
{
    return ( vot_getChild (*elem) );
}


/** VX_GETPARENT
 *
 *  @brief		Return a handle of the parent Element.
 *  @param[in]  elem 	A handle the Element.
 *  @return		A handle of the paretn Element.
 */
handle_t
VX_GETPARENT (handle_t *elem)
{
    return ( vot_getParent (*elem) );
}


/** VX_GETCHILDOfTYPE
 *  @brief		Return a handle of the next Element of the same type.
 *  @param[in]  elem 	A handle the Element.
 *  @param[in]  type 	An integer of the Element type for find.
 *  @return		A handle of the Element.
 */
handle_t
VX_CHILDOFTYPE (handle_t *elem, int *type)
{
    return ( vot_getChildOfType (*elem, *type) );
}


/** VX_VALUEOF
 *
 *  @brief 		Return type of the Element.
 *  @param[in]  elem 	A handle the Element.
 *  @return		An integer of the type.
 */
int
VX_VALUEOF (handle_t *elem)
{
    return ( vot_valueOf (*elem) );
}


/** VX_TYPEOF
 *
 *  @brief 		Return type of the Element.
 *  @param[in]  elem 	A handle the Element.
 *  @return		An integer of the type.
 */
int
VX_TYPEOF (handle_t *elem)
{
    return ( vot_typeOf (*elem) );
}


/** VX_SETWARN
 *
 *  @brief 		Set warnings level.
 *  @param[in]  elem 	Warning level
 *  @return		Nothing
 */
void
VX_SETWARN (int *value)
{
    vot_setWarnings (*value);
}


/****************************************************************************
 *
 ***************************************************************************/

/** VX_SETVALUE
 *
 *  @brief		Return the Value for the ELEMENT.
 *  @param[in]  elem 	A handle the ELEMENT.
 *  @param[in]  value 	A string holding the Value value.
 *  @return 		The status of the set.
 */
int
VX_SETVALUE (handle_t *elem, XCHAR *value)
{
    char *_value = spp2c (value, spplen(value));
    int retval = vot_setValue (*elem, _value);

    if (_value) free ((char *) _value);

    return (retval);
}


/** VX_GETVALUE
 *
 *  @brief		Return the Value for the ELEMENT.
 *  @param[in]  elem 	A handle the ELEMENT.
 *  @return 		A string of the value or the Value.
 */
void
VX_GETVALUE (handle_t *elem, XCHAR *value, int *maxch)
{
    char *_val = vot_getValue (*elem);

    (void) c2spp (_val, value, *maxch);
}


/** VX_GETINTVALUE
 *
 *  @brief		Return the Value for the ELEMENT as an int.
 *  @param[in]  elem 	A handle the ELEMENT.
 *  @return 		An int value
 */
int
VX_GETINTVALUE (handle_t *elem)
{
    return ( atoi (vot_getValue (*elem)) );
}


/** VX_GETREALVALUE
 *
 *  @brief		Return the Value for the ELEMENT.
 *  @param[in]  elem 	A handle the ELEMENT.
 *  @return 		A real value
 */
float
VX_GETREALVALUE (handle_t *elem)
{
    return ( atof (vot_getValue (*elem)) );
}


/** VX_GETATTR
 * 
 *  @brief		Return the attribute for the Element.
 *  @param[in]  elem 	A handle the Element.
 *  @param[in]  attr 	A string holding the attribute name.
 *  @return		A string of the value or the attr.
 */
void
VX_GETATTR (handle_t *elem, XCHAR *name, XCHAR *val, int *len)
{
    char *_name  = spp2c (name, spplen(name));
    char *res  = vot_getAttr (*elem, _name);

    if (res)
        c2spp (res, val, *len);

    if (_name) free ((char *) _name);
}


/** VX_SETATTR
 * 
 *  @brief		Return the attribute for the Element.
 *  @param[in]  elem 	A handle the Element.
 *  @param[in]  attr 	A string holding the attribute name.
 *  @param[in]  value 	A string holding the attribute value.
 *  @return		The status of the set.
 */
int
VX_SETATTR (handle_t *elem, XCHAR *attr, XCHAR *value)
{
    char *_attr  = spp2c (attr, spplen(attr));
    char *_value = spp2c (value, spplen(value));

    int retval = vot_setAttr (*elem, _attr, _value);

    if (_attr)  free ((char *) _attr);
    if (_value) free ((char *) _value);

    return (retval);
}


/** VX_WRITEXML
 * 
 *  @brief		Print the XML to the named file (or stdout)
 *  @param[in]  elem 	A handle the root Element.
 *  @param[in]  fname 	Output file name
 */
void
VX_WRITEXML (handle_t *elem, XCHAR *fname)
{
    char *_fname = spp2c (fname, spplen (fname));

    vot_writeVOTable (*elem, _fname, 0);

    free ((char *) _fname);
}


/** VX_WRITEHTML
 * 
 *  @brief		Print the XML to the named file (or stdout)
 *  @param[in]  elem 	A handle the root Element.
 *  @param[in]  fname 	Output file name
 */
void
VX_WRITEHTML (handle_t *elem, XCHAR *ifname, XCHAR *ofname)
{
    char *_ifname = spp2c (ifname, spplen (ifname));
    char *_ofname = spp2c (ofname, spplen (ofname));

    vot_writeHTML (*elem, _ifname, _ofname);

    free ((char *) _ifname);
    free ((char *) _ofname);
}


/** VX_WRITESHTML
 * 
 *  @brief		Print the XML to the named file (or stdout)
 *  @param[in]  elem 	A handle the root Element.
 *  @param[in]  fname 	Output file name
 */
void
VX_WRITESHTML (handle_t *elem, XCHAR *ifname, XCHAR *ofname)
{
    char *_ifname = spp2c (ifname, spplen (ifname));
    char *_ofname = spp2c (ofname, spplen (ofname));

    vot_writeSHTML (*elem, _ifname, _ofname);

    free ((char *) _ifname);
    free ((char *) _ofname);
}


/** VX_WRITEASV
 * 
 *  @brief		Print the XML as Ascii-Separated Values
 *  @param[in]  elem 	A handle the root Element.
 *  @param[in]  fname 	Output file name
 */
void
VX_WRITEASV (handle_t *elem, XCHAR *fname)
{
    char *_fname = spp2c (fname, spplen (fname));

    vot_writeASV (*elem, _fname, 1);

    free ((char *) _fname);
}


/** VX_WRITEBSV
 * 
 *  @brief		Print the XML as Bar-Separated Values
 *  @param[in]  elem 	A handle the root Element.
 *  @param[in]  fname 	Output file name
 */
void
VX_WRITEBSV (handle_t *elem, XCHAR *fname)
{
    char *_fname = spp2c (fname, spplen (fname));

    vot_writeBSV (*elem, _fname, 1);

    free ((char *) _fname);
}


/** VX_WRITECSV
 * 
 *  @brief		Print the XML as Comma-Separated Values
 *  @param[in]  elem 	A handle the root Element.
 *  @param[in]  fname 	Output file name
 */
void
VX_WRITECSV (handle_t *elem, XCHAR *fname)
{
    char *_fname = spp2c (fname, spplen (fname));

    vot_writeCSV (*elem, _fname, 1);

    free ((char *) _fname);
}


/** VX_WRITETSV
 * 
 *  @brief		Print the XML as Tab-Separated Values
 *  @param[in]  elem 	A handle the root Element.
 *  @param[in]  fname 	Output file name
 */
void
VX_WRITETSV (handle_t *elem, XCHAR *fname)
{
    char *_fname = spp2c (fname, spplen (fname));

    vot_writeTSV (*elem, _fname, 1);

    free ((char *) _fname);
}


/** VX_WRITEFITS
 *
 *  @brief		Write the XML as FITS file
 *  @param[in]  elem 	A handle the root Element.
 *  @param[in]  fname 	Output file name
 */
void
VX_WRITEFITS (handle_t *elem, XCHAR *fname)
{
    char *_fname = spp2c (fname, spplen (fname));

    vot_writeFITS (*elem, _fname);

    free ((char *) _fname);
}




/****************************************************************************
 *  Private utility procedures
 ****************************************************************************/


/*  SPP Name mapping macros.  SPP procedure names are mappad as the first-5
**  plus the last character of a name minus any underscores.  This should
**  be done such that a unique 6-character name is produced for each SPP
**  symbol.  In these definitions the SPP code may use the long form of the
**  name in the code, the mapping is done automatically and so we need the
**  macros here so the symbol entered in the library is actually the short
**  name.
*/



static char *
spp2c (XCHAR *instr, int maxch)
{
    XCHAR  *ip = instr;
    char   *outstr = (char *) calloc (1, maxch+1);
    char   *op = (char *) outstr;
    int      n = maxch;

    while ((*op++ = (char)*ip++) != (char)XEOS && --n >= 0)
        ;
    *--op = (char) XEOS;

    return (outstr);
}


static int
c2spp (PKCHAR *instr, XCHAR *outstr, int maxch)
{
    char   *ip = (char *)instr;
    XCHAR  *op = outstr;
    int      len= 0, n = 0;


    /* Is is necessary to determine the length of the string in order to
     * be able to unpack the string in place, i.e., from right to left.
     */
    for (n=0;  n  < maxch;  n++)
        outstr[n] = (XCHAR) XEOS;
    for (n=0;  *ip;  n++)
        ip++;
/*
    n -= 1;
*/
    len = (n < maxch) ? n : maxch;
    op[n] = (XCHAR) XEOS;

    for (ip = (char *)instr;  --n >= 0;  )
        op[n] = ip[n];
    op[maxch] = (XCHAR) 0;

    return (len);
}


static int
spplen (XCHAR *str)
{
    int len = 0;

    for (len=0; str[len] != (XCHAR) XEOS; len++)
	;

    return (len);
}
