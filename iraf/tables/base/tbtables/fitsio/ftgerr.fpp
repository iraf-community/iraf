C------------------------------------------------------------------------------
        subroutine ftgerr(errnum,text)

C       Return a descriptive error message corresponding to the error number

C       errnum i  input symbolic error code presumably returned by another
C                 FITSIO subroutine
C       text   C*30  Descriptive error message

        integer errnum
        character*(*) text

C       nerror specifies the maxinum number of different error messages
        integer nerror
        parameter (nerror=100)
        character*30 errors(nerror)
        character*30 er1(10),er2(10),er3(10),er4(10),er5(10),er6(10)
        character*30 er7(10),er8(10),er9(10),er10(10)
        integer i,errcod(nerror)
        save errors

C       we equivalence the big array to several smaller ones, so that
C       the DATA statements will not have too many continuation lines.
        equivalence (errors(1), er1(1))
        equivalence (errors(11),er2(1))
        equivalence (errors(21),er3(1))
        equivalence (errors(31),er4(1))
        equivalence (errors(41),er5(1))
        equivalence (errors(51),er6(1))
        equivalence (errors(61),er7(1))
        equivalence (errors(71),er8(1))
        equivalence (errors(81),er9(1))
        equivalence (errors(91),er10(1))

        data errcod/0,101,102,103,104,105,106,107,108,109,110,111,
     &  201,202,203,204,205,206,207,208,209,211,212,213,214,215,216,
     &  217,218,221,222,223,224,225,226,227,228,229,230,231,232,
     &  241,251,252,261,262,
     &  302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,
     &  317,318,319,    401,402,403,404,405,406,407,408,409,411,112,
     &  210,233,220,219,301,320,321,322,263,323,113,114,234,253,254,
     &  255,412,235,236,501,502,503,504,505,237/

        data er1/
     & 'OK, no error',
     & 'Bad logical unit number',
     & 'Too many FITS files opened',
     & 'File not found; not opened',
     & 'Error opening existing file',
     & 'Error creating new FITS file',
     & 'Error writing to FITS file',
     & 'EOF while reading FITS file',
     & 'Error reading FITS file',
     & 'Bad blocking factor (1-28800)'/

        data er2/
     & 'Error closing FITS file',
     & 'Too many columns in table',
     & 'No room in header for keyword',
     & 'Specified keyword not found',  
     & 'Bad keyword record number', 
     & 'Keyword value field is blank',
     & 'Missing quote in string value',
     & 'Could not construct NAMEnnn',
     & 'Bad character in header record',
     & 'Keywords out of order?'/

        data er3/
     & 'Bad nnn value in NAMEnnn',
     & 'Illegal BITPIX keyword value',
     & 'Illegal NAXIS keyword value',
     & 'Illegal NAXISnnn keyword value',
     & 'Illegal PCOUNT keyword value',
     & 'Illegal GCOUNT keyword value',
     & 'Illegal TFIELDS keyword value',
     & 'Illegal NAXIS1 keyword value',
     & 'Illegal NAXIS2 keyword value',
     & 'SIMPLE keyword not found'/

        data er4/
     & 'BITPIX keyword not found',
     & 'NAXIS  keyword not found',
     & 'NAXISnnn keyword(s) not found',
     & 'XTENSION keyword not found',
     & 'CHDU is not an ASCII table',
     & 'CHDU is not a binary table', 
     & 'PCOUNT keyword not found',
     & 'GCOUNT keyword not found',
     & 'TFIELDS keyword not found',
     & 'TBCOLnnn keywords not found'/

        data er5/
     & 'TFORMnnn keywords not found',
     & 'Row width not = field widths',
     & 'Unknown extension type',
     & 'Unknown FITS record type',
     & 'Cannot parse TFORM keyword',
     & 'Unknown TFORM datatype code',
     & 'Column number out of range',
     & 'Data structure not defined',
     & 'Negative file record number',
     & 'HDU start location is unknown'/

        data er6/
     & 'Requested no. of bytes < 0',
     & 'Illegal first row number',
     & 'Illegal first element number',
     & 'Bad TFORM for Character I/O',
     & 'Bad TFORM for Logical I/O',
     & 'Invalid ASCII table TFORM code',
     & 'Invalid BINTABLE TFORM code',
     & 'Error making formated string',
     & 'Null value is undefined',
     & 'Internal read error of string'/

        data er7/
     & 'Illegal logical column value',
     & 'Bad TFORM for descriptor I/O',
     & 'Variable array has 0 length',
     & 'End-of-rec in var. len. array',
     & 'Int to Char conversion error',
     & 'Real to Char conversion error',
     & 'Illegal Char to Int conversion',
     & 'Illegal Logical keyword value',
     & 'Illegal Char to R*4 conversion',
     & 'Illegal Char to R*8 conversion'/

        data er8/
     & 'Char to Int conversion error',
     & 'Char to Real conversion error',
     & 'Char to R*8 conversion error',
     & 'Illegal no. of decimal places',
     & 'Cannot modify a READONLY file',
     & 'END header keyword not found',
     & 'CHDU is not an IMAGE extension',
     & 'Illegal SIMPLE keyword value',
     & 'Column name (TTYPE) not found',
     & 'Out of bounds HDU number'/

        data er9/
     & 'Bad no. of array dimensions',
     & 'Max pixel less than min pixel',
     & 'Illegal BSCALE or TSCALn = 0',
     & 'Could not parse TDIMn keyword',
     & 'Axis length less than 1',
     & 'Incompatible FITSIO version',
     & 'All LUNs have been allocated',
     & 'TBCOLn value out of range',
     & 'END keyword value not blank ',
     & 'Header fill area not blank'/

        data er10/
     & 'Data fill area invalid',
     & 'Data type conversion overflow',
     & 'CHDU must be a table/bintable',
     & 'Column is too wide for table',
     & 'celestial angle too large',
     & 'bad celestial coordinate',
     & 'error in celestial coord calc',
     & 'unsupported projection',
     & 'missing celestial coord keywrd',
     & 'column name not unique'/

C       find the matching error code number
        do 10 i=1,nerror
                if (errnum .eq. errcod(i))then
                        text=errors(i)
                        return
                end if
10      continue

        text='Unknown FITSIO status code'
        end
