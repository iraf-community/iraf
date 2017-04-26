C
C                                                                               
C +-----------------------------------------------------------------+           
C |                                                                 |           
C |                Copyright (C) 1986 by UCAR                       |           
C |        University Corporation for Atmospheric Research          |           
C |                    All Rights Reserved                          |           
C |                                                                 |           
C |                 NCARGRAPHICS  Version 1.00                      |           
C |                                                                 |           
C +-----------------------------------------------------------------+           
C                                                                               
C                                                                               
c +noao: block data hfinit changed to run time initialization
c     BLOCKDATA HFINIT
        subroutine hfinit
C
      COMMON /HAFTO3/ XLT        ,YBT        ,SIDE       ,EXT,
     1                IOFFM      ,ALPH       ,MXLEV   ,NCRTG ,
     2                NCRTF      ,IL(135)
      COMMON /HAFTO4/ NPTMAX     ,NPOINT  ,XPNT(50)  ,YPNT(50)
C
C  INITIALIZATION OF INTERNAL PARAMETERS
C
c     DATA   XLT,  YBT,SIDE,EXT,IOFFM,ALPH,MXLEV,NCRTG,NCRTF/
c    1     0.102,0.102,.805,.25,    0, 1.6,   16,    8, 1024/
c
c +noao:  following flag added to prevent initializing more than once
        logical first
        SAVE
        data first /.true./
        if (.not. first) then
            return
        endif
        first = .false.

c +noao: call to utilbd added to make sure those parameters set by getusv
c        have been set before they are retrieved.
         call utilbd
c -noao
        XLT   = 0.102
        YBT   = 0.102
        SIDE  = .805
        EXT   = .25
        IOFFM = 0
        ALPH  = 1.6
        MXLEV = 16
        NCRTG = 8
        NCRTF = 1024
c
c     DATA IL(1),IL(2),IL(3),IL(4),IL(5),IL(6),IL(7),IL(8),IL(9),IL(10),
c    1IL(11),IL(12),IL(13),IL(14),IL(15),IL(16),IL(17),IL(18),IL(19),
c    2IL(20),IL(21),IL(22),IL(23),IL(24),IL(25),IL(26),IL(27),IL(28),
c    3IL(29),IL(30),IL(31),IL(32),IL(33),IL(34),IL(35),IL(36),IL(37),
c    4IL(38),IL(39),IL(40),IL(41),IL(42),IL(43),IL(44)/
c    5    5,11,
c    6    4, 8,12,
c    7    3, 6,10,13,
c    8    2, 5, 8,11,14,
c    9    1, 4, 7, 9,12,15,
c    +    1, 4, 6, 8,10,12,15,
c    1    1, 3, 5, 7, 9,11,13,15,
c    2     1, 3, 4, 6, 8, 10, 12, 13, 15/
c
       IL(1)  =  5
       IL(2)  = 11
       IL(3)  =  4
       IL(4)  =  8
       IL(5)  = 12
       IL(6)  =  3
       IL(7)  =  6
       IL(8)  = 10
       IL(9)  = 13
       IL(10) =  2
       IL(11) =  5 
       IL(12) =  8
       IL(13) = 11
       IL(14) = 14
       IL(15) =  1
       IL(16) =  4
       IL(17) =  7
       IL(18) =  9
       IL(19) = 12
       IL(20) = 15
       IL(21) =  1
       IL(22) =  4
       IL(23) =  6
       IL(24) =  8
       IL(25) = 10
       IL(26) = 12
       IL(27) = 15
       IL(28) =  1
       IL(29) =  3
       IL(30) =  5
       IL(31) =  7
       IL(32) =  9
       IL(33) = 11
       IL(34) = 13
       IL(35) = 15
       IL(36) =  1
       IL(37) =  3
       IL(38) =  4
       IL(39) =  6
       IL(40) =  8
       IL(41) = 10
       IL(42) = 12
       IL(43) = 13
       IL(44) = 15
c
c     DATA IL(45),IL(46),
c    1IL(47),IL(48),IL(49),IL(50),IL(51),IL(52),IL(53),IL(54),IL(55),
c    2IL(56),IL(57),IL(58),IL(59),IL(60),IL(61),IL(62),IL(63),IL(64),
c    3IL(65),IL(66),IL(67),IL(68),IL(69),IL(70),IL(71),IL(72),IL(73),
c    4IL(74),IL(75),IL(76),IL(77),IL(78),IL(79),IL(80),IL(81),IL(82),
c    5IL(83),IL(84),IL(85),IL(86),IL(87),IL(88),IL(89),IL(90)/
c    6    1, 3, 4, 6, 7, 9,10,12,13,15,
c    7    1, 2, 3, 5, 6, 8,10,11,13,14,15,
c    8    1, 2, 3, 5, 6, 7, 9,10,11,13,14,15,
c    9    1, 2, 3, 4, 6, 7, 8, 9, 10, 12, 13, 14, 15/
c
       IL(45) =  1
       IL(46) =  3
       IL(47) =  4
       IL(48) =  6 
       IL(49) =  7 
       IL(50) =  9
       IL(51) = 10
       IL(52) = 12
       IL(53) = 13
       IL(54) = 15 
       IL(55) =  1 
       IL(56) =  2 
       IL(57) =  3 
       IL(58) =  5 
       IL(59) =  6 
       IL(60) =  8
       IL(61) = 10
       IL(62) = 11
       IL(63) = 13
       IL(64) = 14
       IL(65) = 15 
       IL(66) =  1 
       IL(67) =  2 
       IL(68) =  3
       IL(69) =  5 
       IL(70) =  6 
       IL(71) =  7 
       IL(72) =  9
       IL(73) = 10
       IL(74) = 11
       IL(75) = 13
       IL(76) = 14
       IL(77) = 15 
       IL(78) =  1 
       IL(79) =  2 
       IL(80) =  3 
       IL(81) =  4 
       IL(82) =  6 
       IL(83) =  7 
       IL(84) =  8 
       IL(85) =  9 
       IL(86) = 10 
       IL(87) = 12 
       IL(88) = 13 
       IL(89) = 14 
       IL(90) = 15
c
c     DATA IL(91),
c    1IL(92),IL(93),IL(94),IL(95),IL(96),IL(97),IL(98),IL(99),IL(100),
c    2IL(101),IL(102),IL(103),IL(104),IL(105),IL(106),IL(107),IL(108),
c    3IL(109),IL(110),IL(111),IL(112),IL(113),IL(114),IL(115),IL(116),
c    4IL(117),IL(118),IL(119),IL(120),IL(121),IL(122),IL(123),IL(124),
c    5IL(125),IL(126),IL(127),IL(128),IL(129),IL(130),IL(131),IL(132),
c    6IL(133),IL(134),IL(135)/
c    7    1, 2, 3, 4, 5, 6, 7, 9,10,11,12,13,14,15,
c    8    1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,
c    9    0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15/
c
       IL(91) =  1 
       IL(92) = 2 
       IL(93) = 3 
       IL(94) = 4 
       IL(95) = 5 
       IL(96) = 6 
       IL(97) = 7 
       IL(98) = 9
       IL(99) = 10
       IL(100) = 11
       IL(101) = 12
       IL(102) = 13
       IL(103) = 14
       IL(104) = 15 
       IL(105) = 1 
       IL(106) = 2 
       IL(107) = 3 
       IL(108) = 4 
       IL(109) = 5 
       IL(110) = 6 
       IL(111) = 7 
       IL(112) = 8 
       IL(113) = 9
       IL(114) = 10
       IL(115) = 11
       IL(116) = 12
       IL(117) = 13
       IL(118) = 14
       IL(119) = 15 
       IL(120) = 0 
       IL(121) = 1
       IL(122) = 2 
       IL(123) = 3 
       IL(124) = 4 
       IL(125) = 5 
       IL(126) = 6 
       IL(127) = 7 
       IL(128) = 8 
       IL(129) = 9
       IL(130) = 10
       IL(131) = 11
       IL(132) = 12
       IL(133) = 13
       IL(134) = 14
       IL(135) = 15
c
C SIZE OF THE COORDINATE BUFFERING ARRAYS FOR POINTS BUFFERING.
c     DATA  NPTMAX/50/
      NPTMAX = 50
c -noao
      END
