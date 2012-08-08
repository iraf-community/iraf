# FMIO.H -- File manager interface definitions (private to FMIO).

define  FMIO_MAGIC      28006           # "fm"
define  FMIO_VERSION    101             # current interface version number
define  DEF_PAGESIZE    512             # default page size, bytes
define  DEF_MAXLFILES   128             # default max lfiles / datafile
define  DEF_MAXPTPAGES  256             # default max page table pages
define  DEF_OPTBUFNP    4               # default npages in lfile FIO buffer
define  DEF_MAXBUFNP    0               # default npages max buffer size
define  DEF_BIGBUFNP    16              # large buffer for lfile copies
define  DEF_DFHDRLEN    4096            # default DF header buflen (su)
define  DEF_FCACHESIZE  8               # default open files in file cache
define  DEF_PMLEN       64              # default lfile pagemap array length
define  INC_PMLEN       128             # default page table buflen (pte)
define  SZ_DFNAME       63              # datafile name
define  SZ_ERROPSTR     63              # operand string, for posted errors
define  SYNC_INTERVAL   300             # interval between automatic syncs
define  PT_LFILE        0               # lfile in which page table is stored

# Main FMIO descriptor.
define  LEN_FMDES       150
# GEN
define  FM_MAGIC        Memi[$1+0]      # set once descriptor is initialized
define  FM_ACTIVE       Memi[$1+1]      # set once descriptor is initialized
define  FM_CHAN         Memi[$1+2]      # host channel of datafile
define  FM_MODE         Memi[$1+3]      # access mode of datafile
define  FM_DFVERSION    Memi[$1+4]      # datafile version number
define  FM_SZBPAGE      Memi[$1+5]      # datafile page size, bytes
define  FM_NLFILES      Memi[$1+6]      # number of lfiles in datafile
define  FM_DATASTART    Memi[$1+7]      # file offset of first data page
define  FM_DEVBLKSIZE   Memi[$1+8]      # device block size
define  FM_OPTBUFSIZE   Memi[$1+9]      # optimum FIO buffer size
define  FM_MAXBUFSIZE   Memi[$1+10]     # maximum FIO buffer size
define  FM_SZFCACHE     Memi[$1+11]     # number of LF in file cache
define  FM_LSYNCTIME    Memi[$1+12]     # time of last sync
define  FM_DHMODIFIED   Memi[$1+13]     # datafile header has been modified
# FT
define  FM_FTOFF        Memi[$1+15]     # offset of stored file table (su)
define  FM_FTLASTNF     Memi[$1+16]     # last new file allocated (runtime)
define  FM_FTABLE       Memi[$1+17]     # pointer to in-core file table
# PTI
define  FM_PTIOFF       Memi[$1+20]     # offset of stored PTI (su)
define  FM_PTILEN       Memi[$1+21]     # page table index length (allocated)
define  FM_PTINPTI      Memi[$1+22]     # number of page table pages
define  FM_PTINDEX      Memi[$1+23]     # pointer to page table index
# PT
define  FM_PTLEN        Memi[$1+25]     # number of allocated PTE entries
define  FM_PTNPTE       Memi[$1+26]     # number of PTEs (data pages) in-core
define  FM_PTLUPTE      Memi[$1+27]     # last updated PTE on disk
define  FM_PTABLE       Memi[$1+28]     # pointer to page table data
# LFC
define  FM_FCACHE       Memi[$1+30]     # pointer to file cache descriptor
define  FM_ERRCODE      Memi[$1+31]     # opcode for posting errors
define  FM_ERROPSTR     Memc[P2C($1+32)]# error operand string
define  FM_DFNAME       Memc[P2C($1+96)]# datafile name, for error messages

# File table entry (FTE) during datafile access.
define  LEN_FTE         8               # length of file table entry (ints)
define  LF_FM           Memi[$1]        # backpointer to FMIO descriptor
define  LF_FSIZE        Memi[$1+1]      # file size, bytes
define  LF_FLAGS        Memi[$1+2]      # file bitflags
define  LF_STATUS       Memi[$1+3]      # status word for async i/o
define  LF_LTSIZE       Memi[$1+4]      # logical size of last transfer
define  LF_NPAGES       Memi[$1+5]      # number of pages in file page table
define  LF_PAGEMAP      Memi[$1+6]      # pointer to pagemap array
define  LF_PMLEN        Memi[$1+7]      # length of pagemap array

# FTE bitflags.
define	LFF_SAVE		007B	# flags saved while datafile is closed
define  LFF_ALLOCATED		001B	# lfile slot has been allocated
define  LFF_DELETED             002B    # set if file is deleted
define  LFF_TEXTFILE            004B    # set if file is a text file
define  LFF_IOINPROGRESS        010B    # set when i/o is in progress
define  LFF_LOCKOUT             020B    # set when i/o is in progress

# --------------
# Physical datafile file format.

# Datafile file header.
define  LEN_DHSTRUCT    12
define  DH_MAGIC        Memi[$1]        # magic value identifying data format
define  DH_DFVERSION    Memi[$1+1]      # FMIO version used to write datafile
define  DH_SZBPAGE      Memi[$1+2]      # datafile page size, bytes
define  DH_NLFILES      Memi[$1+3]      # number of lfiles in datafile
define  DH_FTOFF        Memi[$1+4]      # offset of file table in datafile
define  DH_FTLASTNF     Memi[$1+5]      # last new file allocated (runtime)
define  DH_PTIOFF       Memi[$1+6]      # offset of stored page table index
define  DH_PTILEN       Memi[$1+7]      # page table index length (allocated)
define  DH_PTINPTI      Memi[$1+8]      # number of page table pages
define  DH_PTLEN        Memi[$1+9]      # number of allocated PTE entries
define  DH_PTNPTE       Memi[$1+10]     # number of PTEs (data pages)
define  DH_DATASTART    Memi[$1+11]     # file offset of first data page

# File table entry (FTE) on disk.
define  LEN_FTEX        2               # length of file table entry (ints)
define  FT_FSIZE        Memi[$1]        # file size, bytes
define  FT_FLAGS        Memi[$1+1]      # file bitflags
