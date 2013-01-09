C--------------------------------------------------------------------------
        subroutine ftas2c(array,nchar)

C       convert characters in the array from ASCII codes to
C       the machine's native character coding sequence
 
C       array   c  array of characters to be converted (in place)
C       nchar   i  number of characters to convert

        character*(*) array
        integer nchar,i

        integer ebcd1(128),ebcd2(128),ebcdic(256)
        equivalence(ebcd1(1),ebcdic(1))
        equivalence(ebcd2(1),ebcdic(129))
        integer compid
        common/ftcpid/compid

C       The following look-up table gives the EBCDIC character code for
C       the corresponding ASCII code.  The conversion is not universally
C       established, so some sites may need to modify this table.
C       (The table has been broken into 2 arrays to reduce the number of
C       continuation lines in a single statement).
 
        data ebcd1/0,1,2,3,55,45,46,47,22,5,37,11,12,13,14,15,16,17,
     &  18,19,60,61,50,38,24,25,63,39,28,29,30,31,64,79,127,123,91,108,
     &  80,125,77,93,92,78,107,96,75,97,240,241,242,243,244,245,246,
     &  247,248,249,122,94,76,126,110,111,124,193,194,195,196,197,
     &  198,199,200,201,209,210,211,212,213,214,215,216,217,226,227,
     &  228,229,230,231,232,233,74,224,90,95,109,121,129,130,131,132,
     &  133,134,135,136,137,145,146,147,148,149,150,151,152,153,162,
     &  163,164,165,166,167,168,169,192,106,208,161,7/
 
        data ebcd2/32,33,34,35,36,21,
     &  6,23,40,41,42,43,44,9,10,27,48,49,26,51,52,53,54,8,56,57,58,59,
     &  4,20,62,225,65,66,67,68,69,70,71,72,73,81,82,83,84,85,86,87,88,
     &  89,98,99,100,101,102,103,104,105,112,113,114,115,116,117,118,
     &  119,120,128,138,139,140,141,142,143,144,154,155,156,157,158,159,
     &  160,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,
     &  185,186,187,188,189,190,191,202,203,204,205,206,207,218,219,220,
     &  221,222,223,234,235,236,237,238,239,250,251,252,253,254,255/
 
C       this conversion is only necessary on IBM mainframes (compid=4)
C       This executable statement was originally located before the
C       data statements, and it was moved here by PEH on 19 June 1998.
        if (compid .ne. 4)return
 
        do 10 i=1,nchar
C               find the internal equivalent of the character 
                array(i:i)=char(ebcdic(ichar(array(i:i))+1))
10      continue
        end
