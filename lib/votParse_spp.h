##
#  VOTPARSE_SPP.H  -- Include file for the SPP libvotable interface.
#
#  @file 	votParse_spp.h
#  @author 	M. Fitzpatrick, 
#  @date 	4/16/2011
#
#  @brief 	Include file for the SPP libvotable interface.
#
##


# Define UCD defaults for well-known VOTable columns.

define DEF_ACREF_UCD    "VOX:Image_AccessReference"
define DEF_FORMAT_UCD   "VOX:Image_Format"


#  VOTable Summary structure.  Our purpose here is to save some of the
#  commonly referenced elements from the parsing to simplify the applications
#  code.  Many times we'll just want to skip straight to the table data,
#  this gives us handles to the key nodes in case we need to get at more
#  detailed parts of the document.  For nodes such as <FIELD>, we store the
#  handle to the first element in the list and use the interface iterators
#  to loop.

define  SZ_VOT_STRUCT   11

define  VOT_ROOT        Memi[$1  ]              # root VOTable handle
define  VOT_RES         Memi[$1+1]              # RESOURCE handle
define  VOT_TAB         Memi[$1+2]              # TABLE handle
define  VOT_DATA        Memi[$1+3]              # DATA handle
define  VOT_TDATA       Memi[$1+4]              # TABLEDATA handle

define  VOT_INFO        Memi[$1+5]              # INFO handle
define  VOT_PARAM       Memi[$1+6]              # PARAM handle
define  VOT_FIELD       Memi[$1+7]              # FIELD handle

define  VOT_NRES        Memi[$1+8]              # No. of RESOURCES
define  VOT_NROWS       Memi[$1+9]              # No. of TABLEDATA rows
define  VOT_NCOLS       Memi[$1+10]             # No. of TABLEDATA cols



#  Procedure declarations.  In the SPP code we wish to allow readable names,
#  but must map these to the 6 chars produced in the compiled code.

define vx_openVOTABLE   	vopene
define vx_closeVOTABLE   	vclose

define vx_getRESOURCE   	vgetre
define vx_getTABLE   		vgette
define vx_getFIELD   		vgetfd
define vx_getDATA   		vgetda
define vx_getTABLEDATA   	vgetta
define vx_getTR   		vgettr
define vx_getTD   		vgettd
define vx_getBINARY   		vgetby
define vx_getFITS   		vgetfs
define vx_getGROUP   		vgetgp
define vx_getFIELDREF   	vgetff
define vx_getPARAMREF   	vgetpf
define vx_getDESCRIPTION   	vgetdn
define vx_getPARAM   		vgetpm
define vx_getINFO   		vgetio
define vx_getSTREAM   		vgetsm
define vx_getVALUES   		vgetvs
define vx_getMIN   		vgetmn
define vx_getMAX   		vgetmx
define vx_getOPTION   		vgeton
define vx_getLINK   		vgetlk
define vx_getCOOSYS   		vgetcs

define vx_getDATAType   	vgetde
define vx_getDATATypeStr        vgetdr

define vx_newNode   		vnewne
define vx_freeNode   		vfreee
define vx_attachNode   		vattae
define vx_deleteNode   		vdelee
define vx_copyElement   	vcopyt
define vx_getNCols   		vgncol
define vx_getNRows   		vgnrow
define vx_getTableCell   	vgstab
define vx_getTableInt   	vgitab
define vx_getTableReal   	vgrtab
define vx_getLength   		vgetlh
define vx_getNumberOF   	vgetnf

define vx_colByAttr   		vcbatr
define vx_colByName   		vcbnam
define vx_colByUCD   		vcbucd
define vx_colByID   		vcbyid

define vx_findByAttr   		vfindr
define vx_findInGroup   	vfindp
define vx_nextInGroup   	vnextp

define vx_getNext   		vgetnt
define vx_getSibling		vgetsg
define vx_getChild   		vgetcd
define vx_getParent   		vgetpt
define vx_ChildOfType   	vchile
define vx_valueOf   		vvaluf
define vx_typeOf   		vtypef
define vx_setValue   		vsetve
define vx_getValue   		vgsval
define vx_getIntValue   	vgival
define vx_getRealValue   	vgrval
define vx_setAttr   		vsetar
define vx_getAttr   		vgetar
define vx_writeXML   		vwrxml
define vx_writeHTML   		vwrhtl
define vx_writeSHTML  		vwrshl
define vx_writeASV   		vwrasv
define vx_writeBSV   		vwrbsv
define vx_writeCSV   		vwrcsv
define vx_writeTSV   		vwrtsv
define vx_writeFITS		vwrfis
