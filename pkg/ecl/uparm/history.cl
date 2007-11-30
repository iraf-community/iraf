  1 =substr ("foobar",1,3)
  2 =substr ("foobar",0,2)
  3 =triml("   goo")
  4 =triml("ddddgoo","d")
  5 loho
  6 logo
  1 =dalConeSvc ("foo",12.,13.,0.1)
  2 =dalConeSvc ("foo",12.,13.,3.21)
  3 logo
  1 =dalConeSvc("foo",12,13,2)
  2 d_trace
  3 =dalConeSvc("foo",12,13,2)
  4 x =dalConeSvc("foo",12,13,2)
  5 =x
  6 logo
  1 = dalConeSvc ("foo",12,13,14)
  2 logo
  1 =dalConeSvc("foo",12,13,2)
  2 logo
  1 =dalConeSvc("foo",12,13,2)
  2 logo
  1 =dalConeSvc("foo",12,13,2)
  2 =dalConeSvc("foo",3.14,2.717,0.04)
  3 logo
  1 =initVOClient()
  2 logo
  1  =initVOClient()
  2 logo
  1 closeVOClient(1)
  2 =closeVOClient(1)
  3 logo
  1 =initVOClient()
  2 =initVOClient()
  3 logo
  1 lpar cone1
  2 task cone1 = home$cone1.cl
  3 lpar cone1
  4 cl < cone1.cl
  5 vi cone1.cl
  6 cl < cone1.cl
  7 cone1
  8 lpar cone1
  9 task cone1 = home$cone1.cl
 10 lpar cone1
 11 vi cone1.cl
 12 cat cone1.cl
 13 int result, rec, attr, nattr, count
 14 real    ra  = 12.0
 15 real    dec = 12.0
 16 real    sr  = 0.1
 17 string  service ="http://chart.stsci.edu/GSCVO/GSC22VO.jsp?"
 18     if (initVOClient() < 0) 
            error ("Error initializing VO Client interface")
    ;
 19     if (initVOClient() < 0) 
            error ("Error initializing VO Client interface")
    ;
 20     result = dalConeSvc (service, ra, dec, sr)
 21 result = dalConeSvc (service, ra, dec, sr)
 22 =result
 23 logo
  1 lpar cone1
  2 cone1
  3 logo
  1 d_trace
  2 cone1
  3 =service
  4 string service = "foo"
  5 =service
  6 logo
  1 cone1
  2 logo
  1 cone1
  2 logo
  1 cone1
  2 d_trace
  3 cone1
  4 logo
  1 cone1
  2 logo
  1 cone1
  2 printf ("foo %d\n", 5)
  3 vi cone1.cl
  4 cone1
  5 logo
  1 cone1
  2 vi cone1.cl
  3 cone1
  4 !v
  5 vi cone1.cl
  6 cone1
  7 logo
  1 cone1
  2 logo
  1 =regresolver ("USNO-B1")
  2 =regResolver ("USNO-B1")
  3 logo
  1 =regResolver ("USNO-B1")
  2 logo
  1 =regResolver ("USNO-B1")
  2 logo
  1 =regResolver("USNO-B1")
  2 logo
  1 =regResolver("USNO-B1")
  2 logo
  1 =regResolver ("USNO-B1")
  2 logo
  1 =regResolver("USNO-B1")
  2 =regResolver ("USNO-B1", "Description")
  3 logo
  1 =regResolver("USNO-B1")
  2 logo
  1 =regResolver ("USNO-B1")
  2 logo
  1 =regResolver ("USNO-B1")
  2 log
  1 =regResolver ("USNO-B1")
  2 =regResolver ("USNO-B1", "skynode")
  3 =regResolver ("USNO-B1", "cone")
  4 logo
  1 =regResovler ("USNO-B1", "cone")
  2 =regResolver ("USNO-B1", "cone")
  3 logo
  1 =regResolver ("USNO-B1", "cone")
  2 logo
  1 =regResolver ("usno-b1","cone")
  2 !v
  3 vi voclient.c
  4 logo
  1 =regResolver ("USNO-B1","cone")
  2 =nresolved()
  3 =regResolver ("USNO-B1")
  4 =nresovled()
  5 =nresolved()
  6 =regResolver ("USNO-B1", "cone", "ServiceType")
  7 =regResolver("usno-b1","skynode","Name")
  8 =regResolver ("usno-b1","cone","Name")
  9 =regResolver("USNO-B1","cone","Identifier")
 10 =regResolver ("usno-b1","skynode","Identifier")
 11 =regResolver ("USNO-B1","skynode")
 12 =regResolver ("usno-b1")
 13 =regResolver ("usno-b1","cone","foo")
 14 =regResolver ("usno-b1","","Identifier")
 15 logo
  1 =regResolver ("usno-b1","cone","foo")
  2 logo
  1 =regResolver ("usno-b1","","foo")
  2 =nresolved()
  3 logo
  1 =regResolver ("usno-b1","","foo")
  2 logo
  1 =regResolver ("usno-b1","","foo")
  2 =nresolved()
  3 logo
  1 reset nvo = home$../nvo/
  2 task nvo.pkg = nvo$nvo.cl
  3 nvo
  4 dir ..
  5 dir ../nvo/
  6 reset nvo = /Users/fitz/nvoss/nvo/
  7 nvo
  8 =regResolver ("USNO-B1")
  9 conecaller (regResolver ("USNO-B1"), 0., 0., 0.1)
 10 conecaller (regResolver ("GSC2.2"), 0., 0., 0.1)
 11 conecaller (regResolver ("GSC2.2"), 0., 0., 0.1) | fields (STDIN,"2,3")
 12 lpar fields
 13 conecaller (regResolver ("GSC2.2"), 0., 0., 0.1) | fields STDIN 2,3
 14 conecaller (regResolver ("GSC2.2"), 0., 0., 0.1)
 15 conecaller (regResolver ("GSC2.2"), 0., 0., 0.1)  > /tmp/xx
 16 conecaller (regResolver ("GSC2.2"), 0., 0., 0.1, > "/tmp/xx")
 17 vi /tmp/xx
 18 logo
  1 nvo
  2 vocdctl stop
  3 =regResolver ("GSC2.2","cone", "Subject")
  4 =regResolver ("GSC2.2","cone", "Instrument")
  5 =nresolved()
  6 logo
  1 =regResolver ("USNO","","",-1)
  2 =nresolved()
  3 logo
  1 =regResolver ("USNO","","",-1)
  2 logo
  1  =regResolver ("USNO","","",-1)
  2 logo
  1 logo
  1  =regResolver ("USNO","","",-1)
  2 =regResovler ("GSC2.2","","",-1)
  3 =regResolver ("GSC2.2","","",-1)
  4 =regResolver ("GSC2.2","cone","",-1)
  5 =nresolved()
  6 logo
  1 =regResolver ("GSC2.2","cone","ServiceURL",-1)
  2 logo
  1 =regResolver ("USNO","","",-1)
  2 =regResolver ("USNO","","",-1, > "/tmp/_res")
  3 =regResolver ("USNO","","",-1) > /tmp/_res
  4 list = regResolver ("USNO","","",-1)
  5 =list
  6 line = regResolver ("USNO","","",-1)
  7 =line
  8 logo
  1 line = regResolver ("USNO","","",-1)
  2 while (fscan (line, s1) != EOF) {
       print (s1)
    }
  3 list = regResolver ("USNO","","",-1)
  4 while (fscan (list,s1) != EOF) {
        print (s1)
    }
  5 del /tmp/_res
  6 print (regResolver ("USNO","","",-1), >& "/tmp/_res")
  7 list = "/tmp/_res"
  8 while (fscan (list,s1) != EOF) {
       print (s1)
    }
  9 ty /tmp/_res
 10 logo
  1 del /tmp/_res
  2 print (regResolver ("USNO","","",-1), > "/tmp/_res")
  3 list = "/tmp/_res"
  4 while (fscan (list, s1) != EOF) {
        print (s1)
    }
  5  print (regResolver ("USNO","","Name",-1))
  6 print (regResolver ("USNO","","Identifier",-1))
  7 logo
  1 print (regResolver("USNO","","Name",-1))
  2 logo
  1 print (regResolver("USNO","","",-1))
  2 cone
  3 nvo
  4 cone (regResovler("USNO-B1","cone"),0.,0.,0.1)
  5 cone (regResolver("USNO-B1","cone"),0.,0.,0.1)
  6 regResolver("USNO","cone")
  7 =regResolver("USNO","cone")
  8 flpr 0
  9 =regResolver("USNO","cone")
 10 logo
  1 =regResolver("USNO","cone")
  2 logo
  1 =regResolver("USNO","cone")
  2 nvo
  3 conecall (regResolver("USNO-B1","cone"),0.,0.,0.1)
  4 ls /tmp
  5 ls /tmp/
  6 pwd
  7 cd ../nvo/src
  8 vi conecaller.cl
  9 conecall (regResolver("USNO-B1","cone"),0.,0.,0.1)
 10 vi conecaller.cl
 11 ls
 12 vi siapcall.cl
 13 vi siapcaller.cl
 14 vi conecaller.cl
 15 ls
 16 ls *.cl
 17 conecall (regResolver("GSC2.2","cone"),0.,0.,0.1)
 18 conecall (regResolver("GSC2.2","cone"),0.,0.,0.1) | fields STDIN 2,3
 19 vi conecaller.cl
 20 logo
  1 nvo
  2 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.1)
  3 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.1) | fields STDIN 2,3
  4 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.1) | fields STDIN 1
  5 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.1) |& fields STDIN 1
  6 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.1, > "/tmp/xx2")
  7 vi /tmp/xx2
  8 fields /tmp/xx2 2,3
  9 logo
 10 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05) | trans '"' ' '
 11 lpar trans
 12 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05) | trans '""' ' '
 13 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05) | trans '"""' ' '
 14 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05) | trans STDIN '"' ' '
 15 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05) | trans STDIN '\"' ' '
 16 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05) | trans STDIN \" del+
 17 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05) | trans STDIN """" del+
 18 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05) | trans STDIN """" unlearn fields
 19 unlearn fields
 20 flpr 0
 21 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05) | trans STDIN """" del+
 22 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05)
 23 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05, >& "/tmp/xx3")
 24 vi /tmp/xx3
 25 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05)
 26 vi /tmp/vot.xml
 27 stilts ("tcopy","ifmt=votable", "ofmt=ascii", 
    "/tmp/vot.xml", "/tmp/zz.txt")
 28 vi /tmp/zz.txt
 29 del /tmp/zz.txt
 30 del /tmp/_res*
 31 del /tmp/xx*
 32 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05, >& "/tmp/xx")
 33 vi /tmp/xx
 34 flpr 0
 35 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05, >& "/tmp/xx")
 36 del /tmp/xx
 37 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05, >& "/tmp/xx")
 38 vi /tmp/xx
 39 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05) | fields STDIN 2,3
 40 flpr 0
 41 del /tmp/cone*
 42 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05) | fields STDIN 2,3
 43 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05)
 44 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05) | fields STDIN 2
 45 flpr 0
 46 lpar type
 47 ?system
 48 lpar concat
 49 flpr 0
 50 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05) | fields STDIN 2
 51 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05) | fields STDIN 2-6
 52 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05)
 53 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05) |& fields STDIN 2-4
 54 lpar fields
 55 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05) |& fields STDIN 2-4
 56 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.05) |& fields STDIN 2,3
 57 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.25) |& fields STDIN 2,3
 58 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.25) |& fields STDIN 2,3 | graph STDIN
 59 conecaller (regResolver("GSC2.2","cone"),0.,0.,0.25) |& fields STDIN 2,3
 60 conecaller (regResolver("GSC2.2","cone"),10.,10.,0.25) |& fields STDIN 2,3
 61 conecaller (regResolver("GSC2.2","cone"),10.,10.,0.25) |& fields STDIN 2,3 | graph STDIN point+
 62 lpar graph
 63 conecaller (regResolver("GSC2.2","cone"),10.,10.,0.25) |& fields STDIN 2,3 | graph STDIN point+ xformat='%H" yformat="%h"
 64 conecaller (regResolver("GSC2.2","cone"),10.,10.,0.25) |& fields STDIN 2,3 | graph STDIN point+ xformat="%H" yformat="%h"
 65 conecaller (regResolver("GSC2.2","cone"),10.,10.,0.25) |& fields STDIN 2,3 | graph STDIN point+ xformat="%12H" majrx=4 yformat="%12h"
 66 conecaller (regResolver("GSC2.2","cone"),10.,10.,0.25) |& fields STDIN 2,3 | graph STDIN point+ xformat="%13H" majrx=4 yformat="%12h"
 67 conecaller (regResolver("GSC2.2","cone"),10.,10.,0.25) |& fields STDIN 2,3 | graph STDIN point+ xformat="%13h" majrx=4 yformat="%12h"
 68 conecaller (regResolver("GSC2.2","cone"),10.,10.,0.25) |& fields STDIN 2,3 | graph STDIN point+ xformat="%13h" yformat="%12h"
 69 conecaller (regResolver("GSC2.2","cone"),6:23:45.6.,10:12:13.,0.25) |& fields STDIN 2,3 | graph STDIN point+ xformat="%13h" yformat="%12h"
 70 conecaller (regResolver("GSC2.2","cone"),6:23:45.6,10:12:13.,0.25) |& fields STDIN 2,3 | graph STDIN point+ xformat="%13h" yformat="%12h"
 71 del /tmp/con*
 72 ?
 73 sesame m23
 74 conecaller (regResolver("GSC2.2","cone"),sesame.ra,sesame.dec,0.25) | fields STDIN 2,3 | graph STDIN point+ xformat='%h' yformat='%h'
 75 sesame m23
 76 lpar sesame
 77 sesame
 78 unlearn sesame
 79 ?
 80 sesame
 81 lpar sesame
 82 sesame m51 verb+
 83 flpr 0
 84 sesame m51 verb+
 85 lpar sesame
 86 conecaller (regResolver("GSC2.2","cone"),sesame.ra,sesame.dec,0.25) | fields STDIN 2,3 | graph STDIN point+ xformat='%h' yformat='%h'
 87 conecaller (regResolver("GSC2.2","cone"),sesame.ra,sesame.dec,0.25) | fields STDIN 2,3 | graph STDIN point+ xformat='%H' yformat='%h'
 88 lpar graph
 89 s1 = "foo\nbar\n"
 90 list = s1
 91 =list
 92 vi zzgraph
 93 !chmod 755 zzgraph
 94 !zzgraph m31
 95 !./zzgraph m31
 96 ls /Users/fitz/nvoss/vocl/vocl.e
 97 vi zzgraph
 98 !./zzgraph ngc188
 99 sesame ngc188
100 conecaller (regResolver("GSC2.2","cone"),sesame.ra,sesame.dec,0.25) | fields STDIN 2,3 | graph STDIN point+ xformat='%H' yformat='%h'
101 flpr 0
102 flpr 0
103 flpr 0
104 conecaller (regResolver("GSC2.2","cone"),sesame.ra,sesame.dec,0.25) | fields STDIN 2,3 | graph STDIN point+ xformat='%H' yformat='%h'
105 flpr 0
106 flpr 0
107 dir nvobin$
108 bye
109 logo
  1 =regResolver("USNO-B1","cone","Date",-1)
  2 logo
  1 =regResolver ("GSC2.2","cone","Subject")
  2 =regResolver ("GSC2.2","cone","Instrument")
  3 =regResolver ("GSC2.2","cone","ContentLevel")
  4 =regResolver ("GSC2.2","cone","Description")
  5 =regResolver ("GSC2.2","cone","Title")
  6 list = "foo\nbar\ngoof"
  7 =list
  8 lpar cl
  9 lpar field
 10 dir
 11 !uname -a
 12 cd ../nvo/src
 13 vi conecaller.cl
 14 cd ../nvo/src
 15 vi conecaller.cl
 16 vi siapcaller.cl
 17 vi conecaller.cl
 18 lpar match
 19 vi conecaller.cl
 20 lpar type
 21 vi conecaller.cl
 22 vi conecaller.cl
 23 vi conecaller.cl
 24 vi conecaller.cl
 25 vi siapcaller.cl
 26 vi conecaller.cl
 27 ls
 28 lpar cl
 29 logo
  1 =regResolver ("USNO-B1","cone","Name,Type")
  2 logo
  1 =regResolver ("USNO-B1","cone","Name,Type")
  2 logo
  1 =regResolver ("USNO-B1","cone","Name,Type")
  2 logo
  1 =regResolver ("USNO-B1","Title,Identifier")
  2 !v
  3 logo
  1 =regResolver ("USNO-B1","Title,Identifier")
  2 logo
  1 =regResolver("USNO-B1","cone","Title,Identifier")
  2 logo
  1 =regResolver("USNO","","Title,Identifier",-1)
  2 print (regResolver("USNO","","Title,ShortName",-1))
  3 logo
  1 print (regResolver("USNO","","Title,ShortName",-1))
  2 print (regResolver("USNO","","Identifier,ShortName",-1))
  3 print (regResolver("USNO","","ShortName,Identifier",-1))
  4 logo
  1 print (regResolver ("HST","","ShortName,Title",-1))
  2 print (regResolver ("USNO","","ShortName,Title",-1))
  3 print (regResolver ("GSC","","ShortName,Title",-1))
  4 logo
  1 print (regResolver ("USNO","","ShortName,Title",-1))
  2 logo
  1 print (regResolver ("USNO","","ShortName,Title",-1))
  2 logo
  1 print (regResolver ("USNO-B1","","ServiceType,Title",-1))
  2 logo
  1 print (regResolver("HST","","ShortName,Title",-1)
  2 logo
  1 print (regResolver("HST","","ShortName,Title",-1)
  2 logo
  1 list = "foo\nbar\ngoof\n"
  2 =list
  3 logo
  1 list = "foo\nbar"
  2 =list
  3 logo
  1 list = "foo\nbar"
  2 =list
  3 !v
  4 logo
  1 list = "foo\nbar"
  2 =list
  3 =list
  4 list = "foo1\nfoo2\nfoo3\n"
  5 =list
  6 =list
  7 =list
  8 list = "foo1\nfoo2\nfoo3\n\n"
  9 =list
 10  =list
 11  =list
 12  =list
 13 list = "foo1\nfoo2\nfoo3\n"
 14  =list
 15  =list
 16  =list
 17 logo
  1 list = "foo\nbar\n"
  2 =list
  3 =list
  4 list = "foo\nbar\n\n"
  5 =list
  6 =list
  7 =list
  8 logo
  1 list = "foo\nbar\n"
  2 =list
  3 =list
  4 logo
  1 list = "foo\nbar\n"
  2 =list
  3 =list
  4 =list
  5 list = "foo\nbar"
  6 =list
  7 =list
  8 logo
  1 list = regResolver ("HST","","ServiceType",-1)
  2 while (fscan (list, s1) != EOF) {
       print ("type = " // s1)
    }
  3 list = ""
  4 while (fscan (regResolver ("HST","","Title",-1), s1) != EOF) {
  5 while (fscan (regResolver("HST","","Title",-1), s1) != EOF) {
  6 print (regResolver ("HST","","Title",-1))
  7 list = regResolver ("HST","","Title",-1)
  8 while (fscan (list, line) != EOF) {
        print (line)
    }
  9 list = regResolver ("HST","","Title",-1)
 10 =list
 11  =list
 12  =list
 13  =list
 14  =list
 15  =list
 16  =list
 17  =list
 18  =list
 19  =list
 20 logo
  1 list = regResolver ("HST","","Title",-1)
  2 =list
  3  =list
  4  =list
  5  =list
  6  =list
  7  =list
  8  =list
  9  =list
 10  =list
 11 logo
  1 list = regResolver ("HST","","Title",-1)
  2 =list
  3 logo
  1 list = "foo\nbar\n"
  2 =list
  3 struct *lline
  4 logo
  1 unlearn cl
  2 lpar cl
  3 dir *.par
  4 struct lline
  5 lline.p_maxval=2049
  6 lline.p_max=2049
  7 lline.p_len=2049
  8 lline.p_lenval=2049
  9 logo
  1 struct lline len=2048
  2 struct lline { len=2048 }
  3 d_on
  4 =lline
  5 d_p
  6 d_of
  7 dir
  8 d_off
  9 lline = regResolver ("HST","","Title",-1)
 10 =lline
 11 lline = regResolver ("HST","","Title",-1)
 12 while (fscan (lline, s1) != EOF) {
       print (s1)
    }
 13 logo
  1 list = regResolver ("HST","","Title",-1)
  2 while (fscan (list, s1) != EOF) {
    print (s1)
    }
  3 logo
  1 s1 = regResolver ("HST","","Title",-1)
  2 print (s1)
  3 print (regResolver ("HST","","Title",-1))
  4 logo
  1 struct *llist {len=102400}
  2 llist = regResolver ("HST","","Title",-1)
  3 while (fscan (llist, line) != EOF) {
       print (line)
    }
  4 =llist
  5 logo
  1 struct *ll {len=4096}
  2 ll = regResolver ("HST","","ShortName",-1)
  3 logo
  1 struct *ll {len=4096}
  2 ll = regResolver ("HST","","ShortName",-1)
  3 while (fscan (ll, s1) != EOF) {
       print ("ShortName:  " // s1)
    }
  4 list = regResolver ("HST","","ShortName",-1)
  5 while (fscan (list, s1) != EOF) {
    print ("ShortName:  " // s1)
    }
  6 logo
  1 d_trace
  2 list = "dev$hosts"
  3 =list
  4 =list
  5 =list
  6 =list
  7 =list
  8 =list
  9 =list
 10 =list
 11 =list
 12 =list
 13 =list
 14 =list
 15 =list
 16 =list
 17 =list
 18 =list
 19 =list
 20 =list
 21 =list
 22 =list
 23 =list
 24 =list
 25 =list
 26 =list
 27 =list
 28 =list
 29 =list
 30 =list
 31 =list
 32 =list
 33 =list
 34 =list
 35 =list
 36 =list
 37 logo
  1 list = "dev$hosts"
  2 list.p_len = 4096
  3 list.len = 4096
  4 list.p_lenval = 4096
  5 list.p_length = 4096
  6 =list
  7 logo
  1 print (regResolver("HST","","Description",-1))
  2 logo
  1 print (regResolver("HST","","Description",-1))
  2 logo
  1 print (regResolver("HST","","Description",-1))
  2 logo
  1 print (regResolver("HST","","Description",-1))
  2 logo
  1 =regResolver ("GSC2.2")
  2 =nresolved
  3 =nresolved9)
  4 =nresolved()
  5 =regResolver ("GSC2.2","","ServiceType",-1)
  6 =regResolver("USNO-B1","","ServiceType",-1)
  7 print (regResolver ("USNO-B1","","ServiceType",-1))
  8 =regResolver ("USNO-B1","skynode")
  9 logo
  1 string foo[10]
  2 foo = regResolver ("USNO-B1","","ShortName",-1)
  3 =foo[0]
  4 =foo[1]
  5 logo
  1 print(regResolver("USNO","","Title,Subject,ShortName",-1))
  2 logo
  1 =regSvcSearch ("cone", "cool stars", YES)
  2 logo
  1 =regSvcSearch ("cone","cool stars",NO)
  2 logo
  1 i = regSvcSearch ("cone","cool stars",NO)
  2 =regResultCount (i)
  3 logo
  1 i = regSvcSearch ("cone","cool stars",NO)
  2 =regResCount(i)
  3 logo
  1 cl < z
  2 logo
  1 cl < z
  2 logo
  1 cl < z
  2 logo
  1 cl < z
  2 ty z
  3 =regValue (res, "ShortName",0)
  4 vi z
  5 cl < z
  6 !v
  7 vi z
  8 cl < z
  9 !v
 10 vi z
 11 cl < z
 12 logo
  1 cl < z1
  2 ls
  3 cl < z2
  4 vi z2
  5 cl < z2
  6 logo
  1 cl < z2
  2 vi z2
  3 cl < z2
  4 vi z2
  5 !v
  6 vi z2
  7 vi z
  8 cl < z2
  9 vi z2
 10 vi z
 11 cl < z2
 12 ty z2
 13 int res
 14 res = regSearch ("ngc 188", NO)
 15 =regResCount (res)
 16 vi z2
 17 cl < z2
 18 vi z2
 19 cl < z2
 20 !v
 21 vi z2
 22 !v
 23 vi z2
 24 cl < z2
 25 !v
 26 vi z2
 27 cl < z2
 28 vi z2
 29 cl < z2
 30 vi z2
 31 cl < z2
 32 logo
  1 cl < z2
  2 logo
  1 cl < z2
  2 vocdctl stop
  3 nvo
  4 vocdctl stop
  5 vocdctl start
  6 cl < z2
  7 logo
  1 cl < z2
  2 vi z2
  3 logo
  1 vi z2
  2 cl < z2
  3 !v
  4 vi z2
  5 cl < z2
  6 ?util
  7 ?lists
  8 lpar table
  9 phelp table
 10 vi z2
 11 cl < z2
 12 lpar table
 13 vi z2
 14 cl < z2
 15 lpar table
 16 vi z2
 17 cl < z2
 18 vi z2
 19 cl < z2
 20 vi z2
 21 cl < z2
 22 vi z2
 23 cl < z2
 24 !c
 25 vi z2
 26 cl < z2
 27 vi z2
 28 mv z2 zzreg
 29 ty zzreg
 30 cl < zzreg
 31 vi zzreg
 32 ty zzreg
 33 cl < zzreg
 34 ls
 35 logo
  1 ?
  2 nvo
  3 lapr skybot
  4 lpar skybot
  5 skybot 0 0 600
  6 skybot 10 0 600
  7 skybot 10 0 600 epoch=2454545.
  8 logo
  1 nvo
  2 skybot 0 0 900 epoch=2454545
  3 =regResolver("USNO-B1","","Title,ServiceType",-1)
  4  =regResolver("HST","","Title,ServiceType",-1)
  5 print regResolver("USNO-B1","","Title,ServiceType",-1))
  6 print (regResolver("USNO-B1","","Title,ServiceType",-1))
  7 print (regResolver("HST","","Title,ServiceType",-1))
  8 pwd
  9 logo
  1 nvo
  2 =regResolver ("J/AJ/126/1526")
  3 logo
  1 =regResolver("USNO-B1")
  2 =regResolver ("USNO-B1","cone","ServiceURL",-1)
  3 =regResolver ("J/AJ/126/1526","","ServiceURL",-1)
  4 lpar rawcal
  5 nvo
  6 lpar rawcaller
  7 rawcaller regResolver("J/AJ/126/1526","","ServiceURL",-1)
  8 ls
  9 logo
  1 =regResolver ("GSC2.2")
  2 logo
  1 =regResolver ("GSC2.2")
  2 logo
  1 =regResolver ("GSC2.2")
  2 logo
  1 =regResolver ("GSC2.2")
  2 logo
  1 =regResolver ("GSC2.2")
  2 logo
  1 =regResolver ("GSC2.2")
  2 logo
  1 =regResolver ("GSC2.2")
  2 =regResolver("J/AJ/126/1526")
  3 rawcaller (regResolver("J/AJ/126/1526"))
  4 nvo
  5 rawcaller (regResolver("J/AJ/126/1526"))
  6 pwd
  7 cd ../nvo/src
  8 dpar raw
  9 dpar raw > dpraw
 10 logo
  1 nvo
  2 rawcaller (regResolver("J/AJ/126/1526"))
  3 flpr 0
  4 flpr 0
  5 rawcaller (regResolver("J/AJ/126/1526"))
  6 prc
  7 logo
  1 nvo
  2 lpar rawc
  3 rawcaller (regResolver("J/AJ/126/1526"))
  4 prc
  5 dalclient
  6 rawc
  7 flpr 0
  8 rawc
  9 lpar raw
 10 epar raw
 11 flpr 0
 12 rawc
 13 pwd
 14 cd ../nvo/src
 15 vi rawc
 16 d_trace
 17 rawc
 18 flpr 0
 19 d_trace
 20 rawc
 21 flpr 0
 22 lpar dalcl
 23 prc
 24 flpr 0
 25 prc dalcl
 26 prc
 27 dir nvobin$x_nvo.e l+
 28 flpr 0
 29 prc
 30 dir nvobin$x_nvo.e l+
 31 rawc
 32 logo
  1 =rawCaller ("http://iraf.noao.edu/")
  2 nvoo
  3 nvo
  4 rawcaller ("http://iraf.noao.edu")
  5 flpr 0
  6 rawcaller (regResolver ("J/AJ/126/1526"))
  7 ls /tmp/raw*
  8 pwd
  9 cd ../nvo/src
 10 vi rawcaller.cl
 11 rawcaller (regResolver ("J/AJ/126/1526"))
 12 ls /tmp
 13 ls /tmp/
 14 vi /tmp/raw*
 15 pwd
 16 del /tmp/raw*,/tmp/vot*
 17 flpr 0
 18 rawcaller (regResolver ("J/AJ/126/1526"))
 19 ls /tmp
 20 ls /tmp/
 21 vi /tmp/raw8783g.xml
 22 logo
  1 nvo
  2 raw (regResolver("J/AJ/126/1526"))
  3 cl < z2
  4 pwd
  5 cl < zzreg
  6 raw (regResolver("J/PASP/116/1012/"))
  7 lpar skybot
  8 flpr 0
  9 skybot 0 0 900 epoch=2454545.
 10 ls
 11 vi zzreg
 12 vl < zzreg
 13 cl < zzreg
 14 vi zzreg
 15 cl < zzreg
 16 vi zzreg
 17 cl < zzreg
 18 vi zzreg
 19 cl < zzreg
 20 vi zzref
 21 vi zzreg
 22 cl < zzreg
 23 logo
  1 restartVOClient
  2 =restartVOClient
  3 =initVOClient
  4 restartVOClient
  5 =initVOClient
  6 =regResolver("HST")
  7 logo
  1 =initVOClient
  2 =regResolver("USNO")
  3 logo
  1 =initVOClient
  2 =closeVOClient
  3 =regQuery
  4 initVOClient()
  5 =initVOClient()
  6 =restartVOClient()
  7 =closeVOClient()
  8 =closeVOClient(1)
  9 nvo
 10 lpar cone
 11 cone insys="ecliptic 1900"
 12 cone insys="ecliptic 1900"
 13 cone insys="ecliptic 1900"
 14 logo
  1 nvo
  2 cone
  3 =regResolver("SDSSDR4","cone")
  4 -regResolver("SDSS","cone")
  5 =regResolver("SDSS","cone")
  6 cone
  7 flpr 0
  8 cone
  9 flpr 0
 10 cone
 11 logo
  1 nvo
  2 regmetalist
  3 bye
  4 nvo
  5 regmetalist
  6 regmetalist
  7 phelp fxcor
  8 lpar registry
  9 registry "cool stars"
 10 logo
  1 nvo
  2 registry "ngc 188"
  3 registry "ngc 188" verb-
  4 registry "ngc 188" verb-
  5 registry "ngc 188" verb-
  6 registry "ngc 188" verb-
  7 print ("foo,bar") | translit ("STDIN",",","\n")
  8 print ("foo,bar") | translit ("STDIN",",","\\n")
  9 print ("foo,bar") | translit ("STDIN",",","\\n",del-)
 10 print ("foo,bar") | translit ("STDIN",",","\n",del-)
 11 print ("foo, bar") | translit ("STDIN",",","\n",del-)
 12 print ("foo, bar") | translit ("STDIN",", ","\n",del-)
 13 registry "ngc 188" verb+
 14 registry "ngc 188" verb+ fields="Title,Description"
 15 regmeta
 16 regmetalist
 17 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 18 registry "ngc 188" verb+ fields="Title,Facility,Instrument" meta+
 19 ?
 20 ?util
 21 ?lists
 22 phelp columns
 23 ?lists
 24 phelp table
 25 refer print
 26 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 27 bye
 28 nvo
 29 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 30 lpar pretty_string
 31 lpar prettystring
 32 bye
 33 pwd
 34 cd ../nvo/src
 35 vi prettystring.cl
 36 nvo
 37 lpar pretty
 38 lpar prettystr
 39 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 40 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 41 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 42 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 43 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 44 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 45 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 46 =stridx ("foo bar"," ")
 47 =stridx (" ", "foo bar")
 48 =stridx (" ","foobar")
 49 s1 = "foo bar"
 50 =substr(s1,3,3)
 51 =substr (s1,3,5)
 52 =iswhite (" ")
 53 ?lang
 54 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 55 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 56 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 57 registry "sdss" verb+ fields="Title,Facility,Instrument"
 58 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 59 flpr 0
 60 flpr 0
 61 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 62 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 63 lpar cl
 64 lpar cl
 65 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 66 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 67 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 68 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 69 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 70 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 71 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 72 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 73 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 74 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 75 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 76 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 77 registry "ngc 188" verb+ fields="Title,Facility,Instrument"
 78 registry "ngc 188" verb+ fields="Title,Description,Subject"
 79 registry "ngc 188" verb+ fields="Title,Description,Subject"
 80 registry "ngc 188" verb+ fields="Title,Description,Subject"
 81 registry "ngc 188" verb+ fields="Title,Description,Subject"
 82 registry "ngc 188" verb- fields="Title,Description,Subject"
 83 logo
 49 print ("foo "" bar") | sed ("-e", "s/"""//g")
 50 print ("foo "" bar") | sed ("-e", 's/"//g')
 51 print ("foo "" bar") | sed ("-e", "s/\""//g")
 52 print ("foo "" bar") | sed ("-e", "s/\042//g")
 53 print ("foo "" bar") | sed ("-e", "s/\040//g")
 54 print ("foo "" bar") | sed ("-e", "s/\042//g")
 55 print ("foo "" bar") | sed ("-e", "s/\012//g")
 56 print ("foo "" bar") | sed ("-e", "s:\042::g")
 57 print ("foo "" bar") | sed ("-e", "s:[\"]::g")
 58 d_trace
 59 print ("foo "" bar") | sed ("-e", "s:\042::g")
 60 print ("foo "" bar") | sed ("-e", "s:\\042::g")
 61 print ("foo "" bar") | sed ("-e", "s:\\"::g")
 62 print ("foo "" bar") | sed ("-e", "s:\\""::g")
 63 d_trace
 64 cone insys="ecliptic 1950"
 65 cone insys="ecliptic 1950"
 66 cone insys="ecliptic 1950"
 67 print ("foo "" bar") | sed ("-e", "s:\\""::g")
 68 print ("foo "" "" bar") | sed ("-e", "s:\\""::g")
 69 print ("foo "" "" bar")
 70 print ("foo "" "" bar") |& sed ("-e", "s:\\""::g")
 71 print ("foo "" "" bar") |& sed ("-e", "s:\\""::g")
 72 cone insys="ecliptic 1950"
 73 cone insys="ecliptic 1950"
 74 cone insys="ecliptic 1950"
 75 cone insys="ecliptic 1950"
 76 cone insys="ecliptic 1950"
 77 cone insys="ecliptic 1950"
 78 cone insys="ecliptic 1950"
 79 cone insys="ecliptic 1950"
 80 cone insys="ecliptic 1950"
 81 cone insys="ecliptic 1950"
 82 cone insys="ecliptic 1950"
 83 cone insys="ecliptic 1950"
 84 cone insys="ecliptic 1950"
 85 cone insys="ecliptic 1950"
 86 cone insys="ecliptic 1950"
 87 cone insys="ecliptic 1950"
 88 cone insys="ecliptic 1950"
 89 cone
 90 cone
 91 cone
 92 cone
 93 cone
 94 print ("foo "" "" bar") |& sed ("-e", "s:\\""::g")
 95 cone
 96 cone
 97 cone
 98 cone
 99 cone
100 cone
101 cone
102 cone
103 cone
104 cone
105 cone
106 cone
107 cone
108 cone
109 d_trace
110 d_trace
111 d_trace
112 cone
113 cone
114 flpr 0
115 cone
116 cone
117 cone
118 d_trace
119 cone
120 cone
121 flpr 0
122 cone
123 cone
124 cone
125 cone
126 pwd
127 flpr 0
128 ?
129 bye
130 nvo
131 registry "vizier cone"
132 registry "vizier
133 registry "vizier"
134 registry "cool stars"
135 registry "carbon stars"
136 unlearn registry
137 registry "carbon stars"
138 =substr("foo",6,7)
139 =subsr("foo",4,4)
140 =substr("foo",4,4)
141 registry "carbon stars"
142 registry "carbon stars"
143 d_trace
144 registry "carbon stars"
145 d_trace
146 registry "carbon stars"
147 registry "carbon stars"
148 logo
  1 nvo
  2 registry "carbon stars"
  3 =substr ("foo",1,20)
  4 registry "carbon stars"
  5 registry "carbon stars"
  6 registry "carbon stars"
  7 registry "carbon stars"
  8 registry "carbon stars"
  9 flpr 0
 10 registry "carbon stars"
 11 =closeVOClient()
 12 =restartVOClient()
 13 lofo
 14 logo
  1 nvo
  2 registry "carbon stars"
  3 registry "carbon stars"
  4 flpr 0
  5 unlearn registry
  6 registry "carbon stars"
  7 registry "carbon stars" verb-
  8 unlearn prettystr
  9 unlearn regis
 10 flpr 0
 11 registry "carbon stars" verb+
 12 registry "ngc 188" verb+
 13 registry "ngc 188" verb+
 14 unlearn regis
 15 registry "ngc 188" verb+
 16 registry "ngc 188" verb+ fields="Title,CoverageSpectral,Subject"
 17 registry "ngc 188" verb+ fields="Title,CoverageSpectral,Subject"
 18 unlearn registry
 19 registry "ngc 188" verb+ fields="Title,CoverageSpectral,Subject"
 20 registry recnum=14
 21 lpar registry
 22 =regValue (registry.rptr, "Title", 13)
 23 registry "" recnum=14
 24 registry  recnum=14
 25 regmetal all+
 26 regmetalist all+
 27 unlearn regmetalist
 28 task regmetalist = nvosrc$regmetalist.cl
 29 regmeta all+
 30 registry  recnum=14
 31 registry "ngc 188" verb+ fields="Title,CoverageSpectral,ServiceURL,Subject"
 32 logo
  1 nvo
  2 registry "ngc 188" verb+
  3 registry "ngc 188" verb+ fields=all
  4 registry "ngc 188" verb+ fields=all
  5 registry "ngc 188" verb+ fields=all
  6 registry "ngc 188" verb+ fields=all
  7 =regResolver ("J/PASP/116/1012/","","",-1)
  8 logo
  1 nvo
  2 registry "ngc 188"
  3 registry "ngc 188" verb-
  4 lpar viz
  5 lpar viz
  6 epar viz
  7 ls
  8 vizier
  9 vizier
 10 vizier
 11 vizier
 12 lpar raw
 13 vizier
 14 dir
 15 page foo.3
 16 registry vizier
 17 ls
 18 del foo*
 19 cd ../nvo/src
 20 ls
 21 =regResolved ("gsc2.2","cone")
 22 =regResolver ("gsc2.2","cone")
 23 cone (regResolver("GSC2.2","cone"), 0., 0., 0.25)
 24 cone (regResolver("GSC2.2","cone"), 0., 0., 0.05)
 25 siap (regResolver("GSC2.2","cone"), 0., 0., 0.05, 0.05)
 26 siap (regResolver("dss","siap"), 0., 0., 0.05, 0.05)
 27 i = dalSiapSvc (regResolver ("dss","siap"), 0.,0.,0.2,0.2)
 28 logo
  1 i = dalSiapSvc (regResolver ("dss","siap"), 0.,0.,0.2,0.2)
  2 logo
  1 i = dalSiapSvc (regResolver ("dss","siap"), 0.,0.,0.2,0.2,"image/fits")
  2 =regResolver ("dss","siap")
  3 logo
  1 logo
  1 i = dalSiapSvc (regResolver ("dss","siap"), 0.,0.,0.2)
  2 logo
  1 =dalConeSvc (regResolver("GSC2.2","cone"), 0, 0, 0.1)
  2 =regResolver("GSC2.2","cone")
  3 logo
  1 =dalConeSvc (regResolver("GSC2.2","cone"), 0, 0, 0.1)
  2 =regResolver("GSC2.2","cone")
  3 =nresolved()
  4 logo
  1 logo
  1 =dalConeSvc (regResolver("GSC2.2","cone"), 0, 0, 0.1)
  2 =regResolver("GSC2.2","cone")
  3 logo
  1 =dalConeSvc (regResolver("GSC2.2","cone"), 0, 0, 0.1)
  2 s1 = regResolver("GSC2.2","cone")
  3 =dalConeSvc (s1,10,20,0.03)
  4 nvo
  5 =substr (substr("foo bar",3,5),1.2)
  6 =substr (substr("foo bar",3,5),1,2)
  7 s1 = regResolver ("dss","siap")
  8 i = dalSiapSvc (s1,0,0,0.25)
  9 =i
 10 =dalRecordCount(i)
 11 s2 = dalStrAttr (i, "ImageAccessReference")
 12 =restartVOClient()
 13  s1 = regResolver ("dss","siap")
 14 flpr 0
 15 s1 = regResolver ("dss","siap")
 16 logo
  1  s1 = regResolver ("dss","siap")
  2 i = dalSiapSvc (s1, 0,0, 0.2)
  3 i = dalSiapSvc (s1, 0.,0., 0.2)
  4 j = dalGetRecord (i,0)
  5 =dalStrAttr (j,"AccessReference")
  6 s2 = dalStrAttr (j,"AccessReference")
  7 = dalDataset (j, s2, "foo.fits")
  8 ls
  9 =j
 10 =s2
 11 =dalDataset (j,s2,"foo.fits")
 12 ls
 13 vi zzsia
 14 logo
  1 ty zzsia
  2 int qres
  3 del foo.fits verify-
  4 s1 = regResolver ("dss","sia")
  5 qres = dalSiapSvc (s1, 10.0, 10.0, 0.2)
  6 qres = dalSiapSvc (s1, 10.0, 10.0, 0.2)
  7 =dalGetData (qres, 0, "foo.fits")
  8 =s1
  9 logo
  1 cl < zzsia
  2 ty zzsia
  3 vi zzsia
  4 logo
  1 cl < zzsia
  2 ty zzsia
  3 logo
  1 cl < zzsia
  2 ls
  3 logo
  1 logo
  1 cl < zzsia
  2 logo
  1 cl < zzsia
  2 ls
  3 ls foo.fits
  4 logo
  1 cl < zzsia
  2 logo
  1 ls
  2 cl < zzsia
  3 =qres
  4 =dalGetData (15333103, 0, "foo.fits")
  5 logo
  1 logo
  1 nvo
  2 registry usno-b1 fields="Title,ServiceURL" verb+
  3 registry usno-b1 fields="Title,ServiceURL" verb+
  4 registry recnum=6
  5 registry recnum=6
  6 registry recnum=6
  7 unlearn registry
  8 registry gsc2.2 fields=all verb+
  9 unlearn registry
 10 registry gsc2.2 fields=all verb+
 11 registry gsc2.2 fields=all verb+
 12 registry gsc2.2 fields=all verb+
 13 registry recnum=2
 14 registry record=2
 15 registry record=2
 16 registry usno
 17 registry usno
 18 unlearn registry
 19 registry gsc2.2 fields=all verb+
 20 registry usno
 21 registry usno verb-
 22 registry usno verb- bandpass="Infrared"
 23 registry "gsc2" verb- bandpass="Infrared"
 24 registry "gsc2" verb- bandpass="Infrared"
 25 registry "gsc2" verb- bandpass="Infrared"
 26 registry "gsc2" verb- bandpass="Infrared"
 27 registry "gsc2" verb- bandpass="Infrared"
 28 registry "gsc2" verb- bandpass="Infrared"
 29 registry "gsc2" verb- bandpass="Infrared"
 30 registry "gsc2" verb- bandpass="Infrared"
 31 registry "gsc2" verb- bandpass="Infrared"
 32 !echo $DISPLAY
 33 registry "gsc2" verb- bandpass="Infrared"
 34 logo
  1 =validObj(123)
  2 =validateObj (123)
  3 logo
  1 =validObj (123)
  2 logo
  1 =validObj (123)
  2 if (validObj (123)) {
       print ("valid")
    } else {
       print ("invalid")
    }
  3 d_trace
  4 s1 = substr ("foobar",1,3)
  5 s1 = regResolver ("USNO")
  6 i = dalConeSvc (s1,11.,12.,0.2)
  7  s1 = substr ("foobar",1,3)
  8 i = dalConeSvc (s1,11.,12.,0.2)
  9 s1 = substr ("foobar",1,4)
 10  s1 = regResolver ("USNO")
 11 ?
 12 d_trace
 13 nvo
 14 registry "" svc=siap bandpass=x-ray
 15 logo
  1 nvo
  2 registry "" svc=siap bandpass=x-ray
  3 registry "" svc=siap bandpass=x-ray verb-
  4 registry record=10 verb+
  5 ?
  6 =regResolver ("UNSNO","","",-1)
  7 =regResolver ("USNO","","",-1)
  8 print (regResolver ("USNO","","",-1))
  9 print (regResolver ("USNO","","Title,Type,ServiceURL",-1))
 10 cone (regResolver("USNO-B1"), 0., 0., 0.25)
 11 registry "ngc 188" verb-
 12 ?
 13 sesame m31
 14 sesame m31 verb+
 15 lpar sesame
 16 lpar skybot
 17 skybot
 18 logo
  1 = dalConeSvc (regResolver ("USNO-B1","cone"), 10., 12., 0.25)
  2 d_trace
  3 = dalConeSvc (regResolver ("USNO-B1","cone"), 10., 12., 0.25)
  4 d_off
  5 i = dalConeSvc (regResolver ("USNO-B1","cone"), 10., 12., 0.25)
  6 logo
  1 = dalConeSvc (regResolver ("USNO-B1","cone"), 10., 12., 0.25)
  2 logo
  1 = dalConeSvc (regResolver ("USNO-B1","cone"), 10., 12., 0.25)
  2 =validObj (15656887)
  3 logo
  1 =dalGetData(dalSiapSvc(regResolver ("DSS","siap"), 12., 12., 0.1), 0, "foo.fits")
  2 =regResolver("DSS","","ServiceType,Title",-1)
  3  =regResolver("DSS","","ServiceType,Title",-1)
  4  =dalGetData(
  5 =dalGetData( \
           dalSiapSvc( \
  6 logo
  1 cl < zzsia
  2 logo
  1 cl < zzsia
  2 logo
  1 cl  < zzsia
  2 logo
  1 cl < zzsia
  2 logo
  1 cl < zzsia
  2 ls
  3 vi zzsia2
  4 cl < zzsia2
  5 ls
  6 m foo.fits
  7 !less foo.fits
  8 vi zzsia2
  9 cl < zzsia2
 10 imhead doo
 11 imhead foo[0]
 12 display foo[0] 1
 13 ty zzsia2
 14 vi zzsia2
 15 cl < zzsia2
 16 nvo
 17 cl < zzsia2
 18 flpr 0
 19 vi zzsia2
 20 reset imclobber = yes
 21 vi zzsia2
 22 cl < zzsia2
 23 !v
 24 vi zzsia2
 25 cl < zzsia2
 26 vi zzsia2
 27 logo
  1 nvo
  2 cl < zzsia2
  3 ?
  4 registry "" svc=siap bandpass=x-ray verb-
  5 registry "" svc=siap bandpass=x-ray verb-logo
  6 logo
  1 nvo
  2 registry "" svc=siap bandpass=x-ray verb-
  3 registry record=10
  4 registry "ngc 188"
  5 registry "ngc 188" verb-
  6 ?
  7 vizier "ivo://CDS/VizieR/J/PASP/116/1012/cat1"
  8 ty foo
  9 cl < zzsia2
 10 registry "ngc 188" verb-
 11 registry rec=17
 12 registry "" svc=sia bandpass=x-ray
 13 ?
 14 skybot
 15 sesame M1 verb+
 16 lpar tvmark
 17 ls
 18 cl < zzmcat
 19 cl < zzmcat
 20 logo
  1 nvo
  2 cl < zzmcat
  3 cat /tmp/foo
  4 tvmark 1 ""
  5 vi /tmp/foo1
  6 tvmark 1 /tmp/foo1 commands=/tmp/foo
  7 vi /tmp/foo
  8 tvmark 1 /tmp/foo1 commands=/tmp/foo
  9 vi /tmp/foo
 10 tvmark 1 "" commands=/tmp/foo txsize=5
 11 vi /tmp/foo
 12 tvmark 1 "" commands=/tmp/foo txsize=5 color=209
 13 tvmark 1 "" commands=/tmp/foo txsize=5 color=207
 14 logo
  1 nvo
  2 cl < zzmcat
  3 cl < zzmcat
  4 logo
  1 nvo
  2 registry "" svc=siap bandpass=xray
  3 logo
  1 nvo
  2 registry "" svc=sia bandpass=x-ray verb-
  3 registry record=10
  4 registry "ngc 188" verb-
  5 registry record=15
  6 registry record=16
  7 ?
  8 skybot
  9 logo
  1 task $mcat = home$examples/mcat.cl
  2 vi examples/mcat.cl
  3 lpar mcat
  4 vi examples/mcat.cl
  5 mcat
  6 task $mcat = /Users/fitz/nvoss/vocl/examples/mcat.cl
  7 mcat
  8 vi examples/mcat.cl
  9 mcat
 10 nvo
 11 mcat
 12 ls
 13 less foo
 14 del foo
 15 imstat foo.fits
 16 imstat foo.fits[0]
 17 vi examples/mcat.cl
 18 mv examples/mcat.cl examples/messier.cl
 19 ls examples
 20 ls
 21 fitsut
 22 fxhead foo
 23 logo
  1 nvo
  2 ty zzsia
  3 del foo.fits
  4 s1 = regResolver ("dss","sia")
  5 int qres
  6 lpar cl
  7 ty zzsia
  8 qres = dalSiapSvc (s1, 10.0, 20.0, 0.2)
  9 =dalGetStr (qres, "AccessReference", 0)
 10 =dalRecordCount(qres)
 11 =dalGetDbl(qres,"RA",0)
 12 =dalGetInt(qres,"Naxes",0)
 13 =dalGetStr(qres,"Naxes",0)
 14 =qres
 15 =dalGetStr (qres, "Title", 0)
 16 =dalGetStr (qres, "Instrument", 0)
 17 =dalGetStr (qres,"foo",0)
 18 =s1
 19 siap (s1,10.,20.,0.2)
 20 =validateObject (qres)
 21 =validObj(qres)
 22 =dalGetStr(qres,"crval",0)
 23 =dalGetStr(qres,"Scale",0)
 24 =dalGetStr(qres,width,0)
 25 =dalGetStr(qres,"width",0)
 26 =dalGetStr(qres,"RA",0)
 27 =dalGetDbl(qres,"RA",0)
 28 logo
  1 nvo
  2 cl  < zzsia
  3 vi zzsia
  4 cl<zzsia
  5 !v
  6 vi zzsia
  7 cl < zzsia
  8 =dalGetStr(qres,"foo",0)
  9 d_trace
 10 cl < zzsia
 11 vi zzsia
 12 d_trace
 13 cl < zzsia
 14 =qres
 15 vi zzsia
 16 cl < zzsia
 17 vi zzsia
 18 cl < zzsia
 19 ty zzsia
 20 int qres
 21 del foo.fits verify-
 22 s1 = regResolver ("dss","sia")
 23 qres = dalSiapSvc (s1, 10.0, 20.0, 0.2)
 24 =dalGetStr (qres,"foo",0)
 25 logo
  1 ty zzsia
  2 int qres
  3 del foo.fits verify-
  4 s1 = regResolver ("dss","sia")
  5 qres = dalSiapSvc (s1, 10.0, 20.0, 0.2)
  6 =dalGetStr(qres,"foo",0)
  7 logo
  1 cl < zzsia
  2 logo
  1 nvo
  2 cl < zzsia
  3 logo
  1 nvo
  2 cl < zzsia
  3 pu
  4 logo
  1 nvo
  2 cl < zzsia
  3 vi zzsia
  4 logo
  1 nvo
  2 cl < zzsia
  3 logo
  1 nvo
  2 cl < zzsia
  3 lslogo
  4 logo
  1 nvo
  2 cl < zzsia
  3 vi zsia
  4 vi zzsia
  5 cl < zzsia
  6 !v
  7 vi zzsia
  8 cl < zzsia
  9 !v
 10 vi zzsia
 11 log
  1 nvo
  2 cl < zzsia
  3 vi zzsia
  4 d_trace
  5 cl < zzsia
  6 vi zzsia
  7 cl < zzsia
  8 vi zzsia
  9 d_tracce
 10 cl < zzsia
 11 sesame m51
 12 lpar sesame
 13 d_trace
 14 sesame ngc188
 15 lpar sesame
 16 logo
  1 ls
  2 d_trace
  3 cl < zzsia3
  4 nvo
  5 cl < zzsia3
  6 vi zzsia3
  7 cl < zzsia3
  8 vi zzsia3
  9 cl < zzsia3
 10 d_trace
 11 cl < zzsia3
 12 !v
 13 vi zzsia3
 14 cl < zzsia3
 15 ls
 16 registry "" svc=sia
 17 vi zzsia3
 18 registry "" svc=sia verb-
 19 vi zzsia3
 20 cl < zzsia3
 21 imstat foo.fits[[0]
 22 imstat foo.fits[0]
 23 vi zzsia3
 24 cl < zzsia3
 25 display foo[0]
 26 ty zzsia3
 27 logo
  1 type getData("http://iraf.net", "foo")
  2 logo
  1 type getData("http://iraf.net", "foo")
  2 ty foo
  3 logo
  1 type getData("http://iraf.net", "foo")
  2 logo
  1 type getData("http://iraf.net", "foo")
  2 logo
  1 =initVOClient()
  2 type getData("http://iraf.net", "foo")
  3 logo
  1 =initVOClient()
  2 type getData("http://iraf.net", "foo")
  3 ty foo
  4 ls
  5 del foo
  6 logo
  1 =initVOClient()
  2 type getData("http://iraf.net", "foo")
  3 type getData("http://iraf.noao.edu", "foo")
  4 logo
  1 =initVOClient()
  2 type getData("http://iraf.net", "foo")
  3 type getData("http://iraf.net", "foo")
  4 logo
  1 =initVOClient()
  2 type getData("http://iraf.noao.edu")
  3 type getData("http://iraf.noao.edu")
  4 dir uparm$
  5 edit uparm$url_file
  6 type getData("http://iraf.noao.edu")
  7 ty zzsia3
  8 ls examples
  9 cat examples/messier.cl
 10 logo
  1 nvo
  2 registry "" svc=sia bandpass=infrared
  3 registry "" svc=sia bandpass=infrared verb-
  4 registry IRAF verb-
  5 registry IRAS verb-
  6 registry IRAS verb- svc=cone
  7 registry record=5
  8 =imaccess ("dev$pix")
  9 =imaccess ("foo")
 10 sesame foo
 11 lpar sesame
 12 sesame foo v+
 13 flpr 0
 14 sesame foo v+
 15 flpr 0
 16 sesame foo v+
 17 flpr
 18 flpr
 19 sesame foo v+
 20 lpar sesame
 21 bye
 22 nvo
 23 wcsinfo dev$pix
 24 lpar wcsin
 25 lpar wcsinfo
 26 =min (2.3, 3.4)
 27 = 15. / 60.
 28 registry "" svc=cone bandpass="X-ray" verb-
 29 registry "" svc=cone bandpass="Infrared" verb-
 30 lpar datascope
 31 lpar datascope
 32 datascope svc_type=foo
 33 datascope dev$pix svc_type=sia bandpass=Infrared
 34 datascope dev$pix svc_type=sia bandpass=Infrared
 35 datascope dev$pix svc_type=sia bandpass=Infrared
 36 datascope dev$pix svc_type=sia bandpass=Infrared
 37 unlearn registry
 38 datascope dev$pix svc_type=sia bandpass=Infrared
 39 [A
 40 datascope dev$pix svc_type=sia bandpass=Infrared
 41 lpar wcsinfo
 42 wcsinfo dev$pix
 43 lpar wcsinfo
 44 wcsinfo dev$wpix
 45 lpar wcsinfo
 46 imhead dev$pix
 47 imhead dev$pix l+
 48 flpr 0
 49 datascope dev$wpix svc_type=sia bandpass=Infrared
 50 datascope dev$wpix svc_type=sia bandpass=Infrared
 51 logo
  1 nvo
  2 datascope dev$wpix svc=sia bandpass=x-ray
  3 flpr
  4 logo
  1 nvo
  2 epar datascope
  3 datascope (mode='h')
  4 datascope (mode='h')
  5 datascope (mode='h')
  6 datascope (mode='h')
  7 datascope (mode='h')
  8 datascope (mode='h')
  9 =restartVOClient()
 10 logo
  1 nvo
  2 lpar datascope
  3 datascope dev$wpix svc=sia bandpass=x-ray
  4 epar datasc
  5 logo
  1 nvo
  2 datascope
  3 logo
  1 nvo
  2 epar datascope
  3 datascope (mode='h')
  4 logo
  1 nvo
  2 lpar data
  3 datascope
  4 datascope
  5 logo
  1 nvo
  2 datascope
  3 logo
  1 nvo
  2 datascope
  3 logo
  1 nvo
  2 datascope
  3 logo
  1 nvo
  2 nvo
  3 datasca
  4 datascope
  5 lpar cone
  6 wcsinfo dev$wpix
  7 lpar wcsinf
  8 lpar wcsinfo
  9 conecall (regResolver("A1","cone"), 201.94596, 47.4538, 0.142)
 10 logo
  1 nvo
  2 conecall (regResolver("A1","cone"), 201.94596, 47.4538, 0.142)
  3 logo
  1 nvo
  2 datascope
  3 logo
  1 nvo
  2 conecall (regResolver("A1","cone"), 201.94596, 47.4538, 0.142)
  3 lpar dalclient
  4 logo
  1 nvo
  2 conecall (regResolver("A1","cone"), 201.94596, 47.4538, 0.142)
  3 lpar dalclient
  4 conecall (regResolver("A1","cone"), 201.94596, 47.4538, 0.142)
  5 conecall (regResolver("A1","cone"), 201.94596, 47.4538, 0.142)
  6 conecall (regResolver("SDSS","cone"), 201.94596, 47.4538, 0.142)
  7 conecall (regResolver("SDSSDR3","cone"), 201.94596, 47.4538, 0.142)
  8 conecall (regResolver("GSC2.2","cone"), 201.94596, 47.4538, 0.142)
  9 conecall (regResolver("GSC2.2","cone"), 201.94596, 47.4538, 0.142) | count
 10 conecall (regResolver("SDSS","cone"), 201.94596, 47.4538, 0.142) | count
 11 conecall (regResolver("SDSS","cone"), 201.94596, 47.4538, 0.142) |& count
 12 =dalRecordCount(dalConeSvc(regResolver("SDSS","cone"), 201.94596, 47.4538, 0.142))
 13 =dalRecordCount(dalConeSvc(regResolver("GSC2.2","cone"), 201.94596, 47.4538, 0.142))
 14 ls
 15 cd nvo$src
 16 ls
 17 m registry.cl
 18 less registry.cl
 19 =regResolver ("GSC2.2","","ServiceType,Identifier")
 20 logo
  1 nvo
  2 datascope dev$wpix service="GSC2.2,DSS"
  3 datascope dev$wpix service="GSC2.2,DSS"
  4 datascope dev$wpix service="GSC2.2,DSS"
  5 datascope dev$wpix service="GSC2.2,DSS"
  6 datascope dev$wpix service="GSC2.2,DSS"
  7 datascope dev$wpix service="GSC2.2,DSS"
  8 datascope dev$wpix service="GSC2.2,DSS"
  9 logo
  1 nvo
  2 datascope dev$wpix service="GSC2.2,DSS"
  3 logo
  1 nvo
  2 datascope dev$wpix service="GSC2.2,DSS"
  3 datascope dev$wpix service="GSC2.2,DSS,2mass,chandra"
  4 logo
  1 nvo
  2 datascope dev$wpix service="GSC2.2,DSS,FIRST"
  3 logo
  1 nvo
  2 datascope dev$wpix service="GSC2.2,USNO-B1,USNO-A2"
  3 datascope dev$wpix service="GSC2.2,USNO-B1,DSS2"
  4 logo
  1 nvo
  2  datascope dev$wpix service="GSC2.2,USNO-B1,DSS2,FIRST"
  3 datascope dev$wpix service="GSC2.2,USNO-B1,DSS2,FIRST,IRASPSC"
  4 datascope dev$wpix service="cone" bandpass="infrared"
  5 datascope dev$wpix service="cone" bandpass="x-ray"
  6 datascope dev$wpix service="sia" bandpass="optical"
  7 datascope dev$wpix service="siap" bandpass="optical"
  8 datascope dev$wpix service="tabularskyservice"
  9 registry "" svc=tabular bandpass=infrared verb-
 10 registry "" svc=tabular bandpass=infrared verb-
 11 =closeVOClient
 12 =closeVOClient()
 13 =closeVOClient(1)
 14 =initVOClient()
 15 registry "" svc=tabular bandpass=infrared verb-
 16 logo
  1 type getData(http://iraf.noao.edu)
  2 type getData("http://iraf.noao.edu")
  3 logo
  1 type getData("http://iraf.net")
  2 nvo
  3 registry noao
  4 type getData("http://iraf.net")
  5 logo
  1 type getData("http://iraf.net")
  2 logo
  1 type getData("http://iraf.net")
  2 logo
  1 type getData("http://iraf.net")
  2 logo
  1 vocinit
  2 vocstop
  3 vocreset
  4 type getData("http://iraf.net")
  5 logo
  1 nvo
  2 cd examples
  3 ls
  4 task $messier = home$examples/messier.cl
  5 vi messier.cl
  6 messier
  7 dir
  8 vocinit
  9 task $messier = /Users/fitz/nvoss/vocl/examples/
 10 messier
 11 vocreset
 12 messier
 13 logo
  1 nvo
  2 datascope dev$wpix service="gsc2.2,2mass,chandra,first"
  3 =13:27:47
  4 =13:27:47 * 15.
  5 = 47:27:13
  6 =regResolver("noao","","ShortName,ServiceUrl",-1)
  7 print (regResolver("noao","","ShortName,ServiceUrl",-1))
  8 logo
  1 print (regResolver("noao","","ShortName,ServiceUrl",-1))
  2 print (regResolver("noao","","ShortName,Identifier",-1))
  3 print (regResolver("noao","","ShortName,ServiceType,Identifier",-1))
  4 print (regResolver("noao"))
  5 =regResolver("noao")
  6 mm
  7 vocreset
  8 vocreset
  9 logo
  1 vocreset
  2 vocstop
  3 vocstop
  4 vocinit
  5 registry "" svc=sia bandpass=x-ray
  6 nvo
  7 registry "" svc=sia bandpass=x-ray
  8 registry record=10
  9 registry record=10 verb+
 10 registry record=10
 11 registry record=10
 12 logo
  1 nvo
  2 registry iraf
  3 registry record=12
  4 logo
  1 nvo
  2 registry iraf
  3 registry record=10
  4 logo
  1 nvo
  2 registry iraf
  3 registry record=10
  4 ?
  5 lpar rawcaller
  6 logo
  1 nvo
  2 registry iraf
  3 registry record=10
  4 logo
  1 nvo
  2 registry iras
  3 registry record=10
  4 registry iraf
  5 registry record=10
  6 logo
  1 nvo
  2 registry iraf
  3 registry record=10
  4 logo
  1 nvo
  2 registry iraf
  3 registry record=10
  4 lpar cone
  5 pwd
  6 cd nvosrc$
  7 m conecaller.cl
  8 less conecaller.cl
  9 m tabout.cl
 10 less tabout.cl
 11 less conecaller.cl
 12 path
 13 cd ../doc
 14 help conecaller.hlp fi+
 15 help conecaller.hlp fi+
 16 less conecaller.cl
 17 cd ../src
 18 less conecaller.cl
 19 lpar conec
 20 help ../doc/conecaller.hlp fi+
 21 help ../doc/sesame.hlp fi+
 22 help ../doc/conecaller.hlp fi+
 23 logo
  1 nvo
  2 unlearn sesame cone
  3 sesame m51
  4 cone regResolver("GSC2.2","cone") sesame.ra sesame.dec 0.2
  5 vi tabout.cl
  6 pwd
  7 cd nvosrc$
  8 vi tabout.cl
  9 logo
  1 nvo
  2 registry vizier svc=cone
  3 registry vizier svc=tabular
  4 =regResolver("vizier","","ShortName,Identifier",-1)
  5 logo
  1 nvo
  2 cd nvo$doc
  3 dir
  4 registry vizier
  5 registry record=0 verb+
  6 registry record=0 verb-
  7 registry record=1
  8 cone regResolver("Radio Catalogs","cone") image=dev$wpix
  9 vocreset
 10 vocreset
 11 logo
  1 nvo
  2 lpar registry
  3 registry fields=all
  4 cd nvo$src
  5 less registry.cl
  6 registry "ngc 188" verb-
  7 logo
  1 nvo
  2 registry "" sql="Creator like '%smith%'" verb-
  3 registry "" sql="Creator like '%hubble%'" verb-
  4 registry "" sql="Creator like '%hubble%'" verb- qres=0
  5 registry "" sql="Creator like '%monet%'" verb- qres=0
  6 unlearn registry
  7 registry "" sql="Creator like '%monet%'" verb- qres=0
  8 registry "" sql="Creator like '%monet%'" verb- qres=0 record=-1
  9 cd nvo$src
 10 vi registry.cl
 11 registry "" sql="Creator like '%monet%'" verb- qres=0 record=-1
 12 registry "" sql="Creator like '%Monet%'" verb- qres=0 record=-1
 13 vi reg
 14 vi registry.cl
 15 logo
  1 nvo
  2 registry "" sql="Creator like '%monet%'" verb- qres=0 record=-1
  3 registry "" sql="CurationCreatorName like '%monet%'" verb- qres=0 record=-1
  4 registry "" sql="CurationCreatorName like '%Smith%'" verb- qres=0 record=-1
  5 cd ../doc
  6 cd ../nvo/doc
  7 help registry.hlp fi+
  8 pwd
  9 cd ../src
 10 vi registry.cl
 11 registry "ngc%188"
 12 logo
  1 nvo
  2 registry "ngc 188"
  3 registry "ngc188"
  4 registry "M87"
  5 registry "M 87"
  6 registry "M%87"
  7 registry "M31"
  8 registry "M%31"
  9 cd nvo$src
 10 logo
  1 nvo
  2 registry "M%82"
  3 registry "" record=54
  4 registry "ngc 188" svc=siap
  5 registry "ngc 188" svc=cone
  6 registry "active galaxies"
  7 registry "active galaxy"
  8 registry "active galax%"
  9 registry "active gal%"
 10 registry "active galac"
 11 registry "active galac%"
 12 registry "active galac%" svc=cone
 13 registry "active galax%" svc=cone
 14 registry "active gala%" svc=cone
 15 registry "active galaxies" svc=cone
 16 registry "active galax" svc=cone
 17 registry "active galax"
 18 registry "active galax" sub+
 19 registry "active galax" verb+
 20 registry "" record=3
 21 registry "" record=2
 22 logo
  1 nvo
  2 registry "active galax" verb-
  3 registry "" record=4
  4 registry "" record=0
  5 registry record=0
  6 registry hst svc=cone
  7 registry rec=7
  8 registry acs svc=cone
  9 registry rec=12
 10 registry rec=1
 11 registry 2mass
 12 registry 2mass svc=cone
 13 =regResolver("2mass","cone")
 14 registry "" svc=cone bandpass=x-ray verb+
 15 registry record=170
 16 registry xmm
 17 =regResolver("XMM","cone")
 18 =regResolver("ivo://nasa.heasarc/xmmmaster")
 19 registry "" svc=cone bandpass=x-ray verb+
 20 =regSvcSearch("cone","xmm",no)
 21 =regResultCount (3704201)
 22 =regResCount(3704201)
 23 cd nvo$doc
 24 help registry.hlp fi+
 25 help registry.hlp fi+
 26 help registry.hlp fi+
 27 help registry.hlp fi+
 28 help registry.hlp fi+
 29 help registry.hlp fi+ | lprint
 30 print (regResolver("usno-b1","cone","",-1))
 31 print (regResolver("usno-b1","cone",,-1))
 32 print (regResolver("usno-b1","cone","",-1))
 33 print (regResolver("usno-b1","","",-1))
 34 print (regResolver("usno-b1","","ServiceType,ShortName,Identifier",-1))
 35 registry "" svc=cone bandpass=x-ray verb+
 36 registry record=170
 37 registry record=0 | page
 38 lpar sort
 39 registry record=0 | sort col=2
 40 lpar sort
 41 registry record=0 header- | sort col=2
 42 print (regResolver("USNO-B1","","ServiceType,Identifier",-1))
 43 = regResolver("USNO-B1","","ServiceType,Identifier",-1)
 44 cone regResolver("USNO-B1","cone") 12:34:56.7 76:54:32.1 0.2
 45 cone regResolver("USNO-B1","cone") 12:34:56.7 76:54:32.1 0.03
 46 print (regResolver("GSC2.2","","ServiceType,Identifier",-1))
 47 help registry.hlp fi+
 48 lapr vizier
 49 lpar vizier
 50 lpar raw
 51 lapr datascope
 52 lpar datascope
 53 softools
 54 mkhelpdb
 55 help registry
 56 logo
  1 help registry
  2 logo
  1 nvo
  2  datascope dev$wpix service=cone bandpass=infrared filter+ verb+
  1 nvo
  2 datascope dev$wpix service=siap bandpass=x-ray filter+
  3 datascope M54 service=siap bandpass-x-ray filter+
  4 datascope M54 service=siap bandpass=x-ray filter+
  5 sesame M54 verb+
  6 lpar sesame
  7 unlearn sesame
  8 sesame m54
  9 lpar sesame
 10 unlearn sesame
 11 sesame ngc1193
 12 lpar sesame
 13 datascope M54 service=siap bandpass=x-ray filter+
 14 datascope M54 service=siap bandpass=x-ray filter+ head+
 15 datascope M54 service=siap bandpass=x-ray filter+ verb+
 16 sesame M54
 17 lpar sesame
 18 siap regResolver ("Chandra","siap") sesame.ra sesame.dec 0.25
 19 siap regResolver("Chandra","siap") sesame.ra sesame.dec 0.25
 20 int siap = dalSiapSvc (regResolver("Chandra","siap",ses.ra,ses.dec,0.25)
    int siap = dalSiapSvc(regResolver("Chandra","siap",ses.ra,ses.dec,0.25)
    int siap = dalSiapSvc (regResolver("Chandra","siap",ses.ra,ses.dec,0.25)
    int siap = dalSiapSvc(regResolver("Chandra","siap"),ses.ra,ses.dec,0.25)
 21 nvo
 22 int siap
 23 siap = dalSiapSvc(regResolver("Chandra","siap"),ses.ra,ses.dec,0.25)
 24 i = dalResCount(siap)
 25 i = dalRecordCount(siap)
 26 for (j=0; j < i; j=j+1) {
        stat = dalGetData (siap, j, "chandra"//j//".fits")
    }
 27 int count, stat
 28 siap = dalSiapSvc(regResolver("Chandra","siap"),ses.ra,ses.dec,0.25)
 29 logo
  1 nvo
  2 cd nvo$src
  3 ls
  4 del dpar dpraw console.o
  5 del dpar,dpraw,console.o
  6 datascopt M54 svc=cone bandpass=optical filter+
  7 datascope M54 svc=cone bandpass=optical filter+
  8 datascope M54 service=cone bandpass=optical filter+
  9 datascope M54 service=siap bandpass=x-ray verb+ filter+
 10 lapr prettystr
 11 lpar prettystr
 12 datascope M54 service=siap bandpass=x-ray verb+ filter+
 13 datascope M54 service=siap bandpass=x-ray verb+ filter+
 14 datascope M54 service=siap bandpass=x-ray verb+ filter+
 15 logo
  1 nvo
  2 cd nvo
  3 ls
  4 cd src
  5 ls
  6 !wc datascope.cl
  7 ty datascope.cl
  8 pwd
  9 ls
 10 wc *.cl
 11 ty siapcaller.cl
 12 ;s
 13 ls
 14 phelp datascope
 15 type getData("http://iraf.net")
 16 pwd
 17 cd
 18 cd ../nvoss
 19 cd ../vocl/examples
 20 cd
 21 cd ..
 22 ls
 23 cd /Users/fitz/nvoss/vocl/examples
 24 ls
 25 ty zzsia3
 26 phelp cone
 27 ?nvo
 28 registry "ngc 188" svc=cone
 29 registry record=4
 30 sesame "Sextans B" verb_+
 31 sesame "Sextans B" verb+
 32 sesame tadpole
 33 lpar sesame
 34 pwd
 35 vi siabrowser.cl
 36 task siabrowser = /Users/fits/nvoss/vocl/examples/siabrowser.cl
 37 cd ..
 38 ls
 39 cd ..
 40 ls
 41 cd TALKS
 42 ls
 43 vi siabrowser.cl
 44 vi siabrowser.cl
 45 vi siabrowser.cl
 46 vi siabrowser.cl
 47 logo
  1 nvo
  2 siab m45 0.05
  3 logo
  1 nvo
  2 siab m45 0.05
  3 logo
  1 nvo
  2 siab m34 0.03
  3 logo
  1 nvo
  2 siab m23 0.03
  3 logo
  1 nvo
  2 siab m34 0.03
  3 logo
  1 nvo
  2 siab m78 0.02
  3 logo
  1 nvo
  2 siab m34 0.03
  3 siabrowser m89 0.25
  4 logo
  1 nvo
  2 siabrow m64 0.25
  3 siabrow tadpole 0.25
  4 logo
  1 nvo
  2 help
  3 logo
  1 nvo
  2 lpar datascope
  3 datascope M54 svc=cone bandpass=radio
  4 datascope M54 service=cone bandpass=radio
  5 datascope M54 service=cone bandpass=optical
  6 logo
  1 nvo
  2 ds9 &
  3 phelp cone
  4 !ds9 &
  5 siabrowser M23 0.2
  6 ls
  7 cd
  8 ls
  9 cd ../TALKS
 10 cd ..
 11 ls
 12 cd home$
 13 dir
 14 logo
  1 nvo
  2 cd examples
  3 vi zzsia3
  4 cl  < zzsia3
  5 logo
  1 ls
  2 cl < examples/zzsia3
  3 nvo
  4 cl < examples/zzsia3
  5 ls -l foo.fits
  6 logo
  1 nvo
  2 cl < zzsia3
  3 cl < examples/zzsia3
  4 ls
  5 imstat foo.fits[0]
  6 !od -c foo.fits | m
  7 del foo.fits
  8 logo
  1 nvo
  2 cl < examples/zzsia3
  3 !od -c foo.fits | m
  4 logo
  1 nvo
  2 cl < examples/zzsia3
  3 logo
  1 nvo
  2 help
  3 =regResolver("HST","cone","ShortName,Title",-1)
  4 registry HST service=cone
  5 registry HST svc=cone
  6 =regResolver("HST","cone")
  7 =regResolver("ivo://archive.stsci.edu/hst")
  8 cone regResolver("HST","cone") image=dev$wpix
  9 cone regResolver("ngc","cone") image=dev$wpix
 10 logo
  1 logo
 32 skybot epoch=2452585.224
 33 hselect dev$wpix date-obs yes | scan (s1)
 34 =s1
 35 hselect dev$wpix date-bs yes | scan (s1)
 36 =s1
 37 string date
 38 hselect dev$wpix date-bs yes | scan (date)
 39 =date
 40 if (date == "") {
       print ("null")
    }
    ;
 41 if (date == "") {
       print ("null")
    }
    ;
 42 hselect dev$wpix date-bs yes | scan (date)
 43 if (date == "") {
       print ("null")
    }
    ;
 44 if (date == "") {
       print ("null")
    }
    ;
 45 lpar skybot
 46 lpar tvmark
 47 lapr fields
 48 lpar fields
 49 phelp tvmark
 50 bye
 51 bye
 52 nvo
 53 ls ..
 54 imskybot ../ecliptic
 55 imskybot ../ecliptic
 56 astutil
 57 imskybot ../ecliptic
 58 lpar astcals
 59 lpar astcalc
 60 imskybot ../ecliptic
 61 imskybot ../ecliptic
 62 imskybot ../ecliptic
 63 imskybot ../ecliptic
 64 imskybot ../ecliptic
 65 imskybot ../ecliptic
 66 lpar display
 67 imskybot ../ecliptic
 68 imskybot ../ecliptic
 69 lapr wcsctran
 70 lpar wcsctran
 71 phelp wcsctran
 72 imskybot ../ecliptic
 73 imskybot ../ecliptic
 74 imskybot ../ecliptic
 75 lapr wcsctran
 76 lpar wcsctran
 77 lpar skybot
 78 lpar skybot
 79 imskybot ../ecliptic
 80 lapr tvmark
 81 lapr tvmark
 82 lpar tvmark
 83 lpar tvmark
 84 imskybot ../ecliptic
 85 imskybot ../ecliptic
 86 imskybot ../ecliptic
 87 imskybot ../ecliptic
 88 reset stdimage = imt2048
 89 imskybot ../ecliptic
 90 registry jet
 91 registry jet inter+
 92 vizier
 93 lpar vizier
 94 vizier otype=votable
 95 imskybot ../ecliptic
 96 imskybot ../ecliptic
 97 skybot
 98 skybot
 99 imskybot ../ecliptic
100 imskybot ../ecliptic
101 imskybot ../ecliptic
102 lpar tvmark
103 imskybot ../ecliptic
104 imskybot ../ecliptic
105 imskybot ../ecliptic
106 imskybot ../ecliptic
107 imskybot ../ecliptic\
108 imskybot ../ecliptic\
109 imskybot ../ecliptic
110 vizier otype=votable
111 vizier | page
112 sesame 4c12.03 verb+
113 lpar vizier
114 lpar head
115 registry jets
116 unlearn registry
117 registry jets
118 registry jets interact+
119 registry jets interact+
120 unlearn registry
121 registry jets bandpass=optical interact+
122 registry jets bandpass=optical interact+
123 registry jets bandpass=optical interact+
124 registry jets bandpass=optical interact+
125 registry jets bandpass=x-ray interact+
126 unlearn registry
127 registry jets bandpass=x-ray interact+
128 registry jets bandpass=infrared interact+
129 registry "AGN jets" bandpass=optical interact+
130 registry "AGN jets" bandpass=optical interact+
131 logo
  1 nvo
  2 unlearn registry
  3 registry "radio galax" bandpass=radio interact+
  4 registry "AGN" bandpass=radio interact+
  5 registry "ngc 188"  interact+
  6 logo
  1 nvo
  2 registry "abell cluster" svc=cone interact+
  3 cone regResolver("ABELL","cone") 195 28 5
  4 cone regResolver("ABELL","cone") 195 28 5 otype=votable
  5 cone regResolver("ABELL","cone") 195 28 5 otype=votable
  6 cone regResolver("ABELL","cone") 195 28 15 otype=votable
  7 cone regResolver("ABELL","cone") 195. 28. 15. otype=votable
  8 =regResolver("ABELL","cone")
  9 registry "abell clusters" svc=cone inter+
 10 cone regResolver("ivo://nasa.heasarc/abell","cone") 195 28 5
 11 cone regResolver("ivo://nasa.heasarc/abell","cone") 195 28 15
 12 cone regResolver("ivo://nasa.heasarc/abell","cone") 195 28 45
 13 cone regResolver("ivo://nasa.heasarc/abell","cone") 195 28 5 otype=votable
 14 =regResolver("ivo://nasa.heasarc/abell","cone")
 15 cone trim(regResolver("ivo://nasa.heasarc/abell","cone")) 195 28 5 otype=votable
 16 logo
  1 =regResolver("ivo://nasa.heasarc/abell","cone")
  2 logo
  1 =regResolver("ivo://nasa.heasarc/abell","cone")
  2 registry "abell cluster" interact+
  3 nvo
  4 registry "abell cluster" interact+
  5 logo
  1 nvo
  2 registry "abell cluster" interact+
  3 =regResolver("heasarc/abell")
  4 registry "abell cluster" svc=cone band=optical inter+
  5 logo
  1 nvo
  2 registry "abell cluster" svc=cone inter+
  3 ?
  4 skybot
  5 phelp siap
  6 logo
  1 logo
  1 =regResolver("USNO-B1","","ServiceType,Title",-1)
  2 =regResolver("USNO-B1","","ShortName,ServiceType,Title",-1)
  3 print (regResolver("USNO-B1","","ServiceType,Title",-1))
  4 logo
  1 =regResolver("ABELL","cone")
  2 logo
  1 =regResolver("ABELL","cone")
  2 logo
  1 =regResolver("ABELL","cone")
  2 logo
  1 =regResolver("ABELL","cone")
  2 logo
  1 =regResolver("ABELL","cone")
  2 logo
  1 =regResolver("ABELL","cone")
  2 logo
  1 =regResolver("ABELL","cone")
  2 logo
  1 =regResolver("ABELL","cone")
  2 logo
  1 =regResolver("ABELL","cone")
  2 logo
  1 nvo
  2 registry "abell cluster" svc=cone inter+
  3 logo
  1 no
  2 nvo
  3 =regResolver("ABELL","cone")
  4 logo
  1 =regResolver("ABELL","cone")
  2 logo
  1 =regResolver("ABELL","cone")
  2 no
  3 nvo
  4 registry "abell cluster" svc=cone inter+
  5 logo
  1 =regResolver("ABELL","cone")
  2 logo
  1 =regResolver("ABELL","cone")
  2 logo
  1 nvo
  2 registry "abell cluseter" svc=cone inter+
  3 registry "abell cluster" svc=cone inter+
  4 registry "" svc=skynode
  5 logo
  1 nvo
  2 registry "abell cluster
  3 registry "abell cluster"
  4 logo
  1 nvo
  2 registry "abell cluster" inter+
  3 registry poss svc=cone
  4 registry poss svc=skynode
  5 cd /tmp
  6 ls
  7 copy getData("http://iraf.net/ftp/nvoss/siabrowser.cl") siabrowser.cl
  8 ls
  9 vocinit
 10 vocinit
 11 vocstop
 12 vocinit
 13 lgo
 14 logo
  1 vocinit console
  2 logo
  1 vocinit console
  2 vocstop
  3 logo
  1 nvo
  2 registry chandra svc=cone
  3 registry chandra svc=cone inter+
  4 registry chandra svc=cone inter+
  5 =cl.ukey
  6 registry chandra svc=cone inter+
  7 registry chandra svc=cone inter+
  8 registry chandra svc=cone inter+
  9 registry chandra svc=cone inter+
 10 registry chandra svc=cone inter+
 11 registry chandra svc=cone inter+
 12 string ch
 13 ch = cl.ukey
 14 =ch
 15 if (ch == "\015") {
       print ("NL")
    }
    ;
 16 if (ch == "\015") {
       print ("NL")
    }
    ;
 17 = (ch == "foo")
 18 =(ch == "\015")
 19 =(ch=='\015')
 20 i = ch
 21 lpar cl
 22 =streq(ch,"\n")
 23 =substr(ch,1,1)
 24 =substr(ch,1,4)
 25 registry chandra svc=cone inter+
 26 =ch
 27 =(substr(ch,1,4) == "\015")
 28 registry chandra svc=cone inter+
 29 =(substr(ch,1,4) == "\\015")
 30 registry chandra svc=cone inter+
 31 registry chandra svc=cone inter+
 32 datascope dev$wpix service=cone bandpass=infrared filter+
 33 logo
  1 nvo
  2 datascope dev$wpix service=cone bandpass=infrared filter-
  3 logo
  1 nvo
  2 datascope dev$wpix service=cone bandpass=infrared filter+
  3 logo
  1 nvo
  2 datascope dev$wpix service=cone bandpass=infrared filter+
  3 logo
  1 nvo
  2 datascope dev$wpix service=cone bandpass=infrared filter+
  3 datascope dev$wpix service=cone bandpass=infrared filter+ verb+
  4 datascope dev$wpix service=cone bandpass=infrared filter- verb+
  5 registry "" svc=cone bandpass=infrared
  6 registry "" svc=cone bandpass=infrared inter+
  7 logo
  1 nvo
  2 datascope dev$wpix service=cone bandpass=infrared filter+
  3 logo
  1 nvo
  2 datascope dev$wpix service=cone bandpass=infrared filter+
  3 =regResolver("ADIL")
  4 =regResolver("ADIL","","ServiceUrl",-1)
  5 =regResolver("ADIL","","ServiceType,Title",-1)
  6 datascope dev$wpix service=cone bandpass=infrared filter+
  7 =regResolver("Spitzer")
  8 =regResovler("ivo://nasa.heasarc/spitzmastr")
  9 =regResolver("ivo://nasa.heasarc/spitzmastr")
 10 logo
  1 nvo
  2 datascope dev$wpix service=cone bandpass=infrared filter+
  3 logo
  1 nvo
  2 datascope dev$wpix service=cone bandpass=infrared filter+
  3 logo
  1 nvo
  2 datascope dev$wpix service=cone bandpass=infrared filter+
  3 logo
  1 nvo
  2 datascope dev$wpix service=cone bandpass=infrared filter+
  3 logo
  1 nvo
  2 datascope dev$wpix service=cone bandpass=infrared filter+
  3 logo
  1 nvo
  2 datascope dev$wpix service=cone bandpass=infrared filter+
  3 logo
  1 nvo
  2 =regResolver("ADIL")
  3 datascope dev$wpix service=cone bandpass=infrared filter+
  4 logo
  1 nvo
  2 datascope dev$wpix service=cone bandpass=infrared filter+
  3 registry "" svc=siap bandpass=infrared
  4 registry "" svc=siap bandpass=x-ray
  5 registry "" svc=siap bandpass=x-ray inter+
  6 sesame Persson's\ Star
  7 sesame "Persson's Star"
  8 lpar sesame
  9 registry "" svc=siap bandpass=radio
 10 registry "" svc=siap bandpass=radio inter+
 11 =regResolver("FIRST","siap")
 12 siap regResolver("FIRST","siap") 12:29:06
 13 int siap
 14 siap = dalSiapSvc(regResolver("FIRST","siap"),12:29:06,2:03:08.5,0.2)
 15 =dalRecordCount(siap)
 16 siap = dalSiapSvc(regResolver("FIRST","siap"),12:29:06,2:03:08.5,0.25)
 17 =dalRecordCount(siap)
 18 siap = dalSiapSvc(regResolver("Chandra","siap"),12:29:06,2:03:08.5,0.25)
 19 print (dalRecordCount(siap))
 20 sesame horsehead v+
 21 siap regResolver("2MASS","siap") sesame.ra sesame.dec 0.2
 22 siap = dalSiapSvc (regResolver("2MASS","siap"),ses.ra,ses.dec,0.2,0.2)
 23 print (dalRecordCount(siap))
 24 =dalGetData(siap,0,"horse_j.fits")
 25 !ds9 ^
 26 !ds9 &
 27 display horse_j.fits
 28 fitsutil
 29 fxhead horse_j.fits
 30 !od -c horse_j.fits | m
 31 ls -l horse_j.fits
 32 =dalGetData(siap,1,"horse_k.fits")
 33 display horse_k.fits 1
 34 s1 =dalGetData(siap,2,"horse_h.fits")
 35 display horse_h.fits 1
 36 !xgterm &
 37 s1 =dalGetData(siap,3,"horse_h.fits")
 38 pwd
 39 ls
 40 mkdir demo
 41 cd demo
 42 imcopy getData("http://iraf.net/nvoss/cluster.fits") cluster.fits
 43 imcopy getData("http://iraf.net/nvoss/cluster.fits") cluster.fits
 44 copy getData("http://iraf.net/nvoss/cluster.fits") cluster.fits
 45 imhead cluster.fits
 46 reset stdimage = imt2048
 47 display cluster 1
 48 registry "SDSS" svc=siap bandpass=optical
 49 siap regResolver("DSSR","siap") (15.*17:12:32) 64:03:15 0.16667
 50 cl< z
 51 =regResolver("DSS2B","siap")
 52 cl < z
 53 ls
 54 ls
 55 cl < z
 56 cl < z
 57 cl < z
 58 ls
 59 d_trace
 60 siap  = dalSiapSvc(regResolver("DSS2B","siap"),ra,dec,0.16667,0.16667)
 61 siap  = dalSiapSvc(regResolver("DSS2B","siap"),ra,dec,0.16667,0.16667)
 62 real  ra, dec
 63 ra    = (17:12:32 * 15)
 64 dec   = 64:03:15
 65 siap  = dalSiapSvc(regResolver("DSS2B","siap"),ra,dec,0.16667,0.16667)
 66 s1=dalGetData(siap,1,"gal.fits")
 67 =siap
 68 s1=dalGetData(siap,0,"gal.fits")
 69 ls
 70 fxheader gal
 71 display gal.fits[0]
 72 reset stdimage = imt800
 73 display gal.fits[0]
 74 reset imclobber=yes
 75 cl < z
 76 cl < z
 77 siap  = dalSiapSvc(regResolver("XMM-Newton","siap"),ra,dec,0.16667,0.16667)
 78 count = dalRecordCount(siap)
 79 siap  = dalSiapSvc(regResolver("XMM-Newton","siap"),ra,dec,0.16667,0.16667)
 80 registry XMM
 81 registry XMM sc=siap
 82 registry XMM svc=siap
 83 registry XMM svc=siap inter+
 84 vi z
 85 siap  = dalSiapSvc(regResolver("XMM-Newton","siap"),ra,dec,0.16667,0.16667)
 86 =siap
 87 siap  = dalSiapSvc(regResolver("GOOD-HST","siap"),ra,dec,0.16667,0.16667)
 88 siap  = dalSiapSvc(regResolver("XMM","siap"),ra,dec,0.16667,0.16667)
 89 registry XMM
 90 registry XMM svc=siap inter+
 91 registry FIRST svc=siap inter+
 92 siap  = dalSiapSvc(regResolver("FIRST","siap"),ra,dec,0.16667,0.16667)
 93 count = dalRecordCount(siap)
 94 logo
141 cl < z
142 ty c4
143 cl < z
144 ty c4
145 !less ned.txt
146 cl < z
147 cl < z
148 ty c2
149 count c2
150 lpar count
151 lpar wcsctran
152 cl < z
153 registry simbad
154 registry simbad svc=cone inter+
155 cone regResolver("Simbad","cone") ra dec 0.1
156 sesame abell2255 v+
157 sesame abell2235 v+
158 cl < z
159 cl < z
160 cl < z
161 ls
162 less ned.txt
163 match G_lens ned.txt
164 lpar match
165 match G_Lens ned.txt
166 match G_Lens ned.txt | count
167 cl < z
168 =regResolver("SDSSDR2","siap")
169 cl < z
170 cl < z
171 cl < z
172 cl < z
173 lpar tvmark
174 task abell = abell.cl
175 lpar abell
176 lpar abel
177 abell 2245
178 abell 2245
179 lpar wcsctran
180 unlearn wcsctran
181 lpar wcsctran
182 phelp wcsctran
183 abell 1234
184 abell 1234
185 abell 1234
186 abell 1234
187 abell 2255
188 abell 2255
189 abell 2255
190 abell 2255
191 abell 2255
192 epar tvmark
193 unlearn tvmark
194 phelp tvmark
195 abell 2255
196 abell 2235
197 lpar color
198 lpar tvmark
199 lapr count
200 lpar count
201 abell 2123
202 abell 2255
203 abell 2255
204 vocreset
205 abell 2255
206 abell 2255
207 abell 2255
208 abell 2255
209 abell 2255
210 abell 2255
211 lpar tvmark
212 cl < z
213 ty z
214 abell 2255
215 abell 2255
216 abell 2255
217 ls
218 del pos*,getIm*,c*
219 ls
220 lpar tvmark
221 phelp tvmark
222 abell 2255
223 abell 2255
224 gflush
225 abell 2255
226 abell 2155 size=0.25
227 abell 2255 size=0.25
228 abell 2255 size=0.35
229 imhead gal
230 imhead gal[0]
231 abell 2255 size=0.35
232 abell 2255 size=0.35
233 lpar wcslab
234 task chart = chart.cl
235 chart ngc1765
236 lpar sesame
237 sesame ngc1765 verb+
238 vocreset
239 vocstop
240 logo
  1 cd demo
  2 nvo
  3 task chart = chart.cl
  4 chart m62 size=0.25
  5 chart m23 size=0.2
  6 chart abell1689
  7 chart abell2255
  8 chart ngc188
  9 chart M87
 10 gflush
 11 flpr
 12 chart M87
 13 lpar wcslab
 14 lpar wlpars
 15 chart M93
 16 chart M31
 17 chart M31 size=0.5
 18 chart abell2255
 19 task fchart = fchart.cl
 20 lpar fchart
 21 fchart image=dev$wpix
 22 fchart image=dev$wpix
 23 ls
 24 del c1
 25 fchart image=dev$wpix
 26 fchart image=dev$wpix
 27 ls /tmp/chart*
 28 fchart image=dev$wpix
 29 fchart image=dev$wpix
 30 fchart image=dev$wpix
 31 fchart m51
 32 fchart m51
 33 lpar fchart
 34 fchart m51
 35 fchart m51
 36 fchart m51
 37 fchart ra=13:29:55.7 dec=47:13:54
 38 fchart "" ra=13:29:55.7 dec=47:13:54
 39 lpar fchart
 40 unlearn fchart
 41 lpar fchart
 42 fchart "" ra=13:29:55.7 dec=47:13:54
 43 fchart "" ra=13:29:55.7 dec=47:13:54
 44 fchart "" ra=13:29:55.7 dec=47:13:54
 45 fchart "" ra=13:29:55.7 dec=47:13:54
 46 fchart "" ra=13:29:55.7 dec=47:13:54 grid-
 47 chart abell2255
 48 end
    chart abell2255
 49 end
    chart abell2255
 50 chart abell2255
 51 ls
 52 ls /tmp/
 53 chart abell2255
 54 ls /tmp/
 55 ls
 56 del tm* char*fits
 57 del tm*,char*fits
 58 ls
 59 chart abell2255
 60 flpr 0
 61 flpr 0
 62 chart abell2255
 63 task chart = chart_soln.cl
 64 chart m65
 65 skybot 0.0 0.0 now
 66 lpar skyb
 67 skybot 0.0 0.0 900 epoch=now
 68 logo
  1 nvo
  2 int res
  3 res = regSearch("ngc 188")
  4 =regValue (res, "ShortName,Title",3)
  5 =regResCount(res)
  6 =regValue (res, "Title", 3)
  7 res = dalConeSvc (regResolver("USNO-A2","cone"), 0.0, 0.0, 0.1)
  8 =dalRecordCount (res)
  9 conecaller regResolver("USNO-A2","cone") 0.0 0.0 0.1
 10 conecaller regResolver("USNO-A2","cone") 0.0 0.0 0.1 otype=votable
 11 conecaller regResolver("USNO-A2","cone") 0.0 90.0 0.1 insys=galactic
 12 ls
 13 ?
 14 lpar skybot
 15 ls ..
 16 lpar imskyb
 17 lpar data
 18 datascope dev$wpix service=cone bandpass=infrared
 19 ?
 20 lpar vizier
 21 phelp siap
 22 pwd
 23 ls
 24 mkdir test1
 25 cd test1
 26 vi z
 27 tables
 28 ttools
 29 lpar thist
 30 logo
  1 nvo
  2 registry "abell cluster"
  3 registry "abell cluster" svc=cone
  4 registry "abell cluster" svc=cone inter+
  5 c
  6 end
    clear
  7 print (regResolver("chandra","","ShortName,Title",-1))
  8 =nresolved()
  9 int res
 10 res = regSearch("ngc 188")
 11 =regResCount(res)
 12 =regValue(res,"Title",3)
 13 res = dalConeSvc(regResolver("USNO-A2","cone"), 0., 0., 0.1)
 14 =dalRecordCount(res)
 15 cone regResolver("usno-a2","cone") 0. 90. 0.1 insys=galactic otype=votable
 16 cone regResolver("usno-a2","cone") 0. 90. 0.1 insys=galactic
 17 ?
 18 sesame M31
 19 lpar m31
 20 lpar sesame
 21 sesame M31 verb+
 22 sesame M31 verb+ | scan (x,y)
 23 =x
 24 c
 25 end
    clear
 26 skybot 0. 0.0 epoch=now
 27 !ds9 &
 28 imskybot ../ecliptic.fits
 29 clear
 30 datascope dev$wpix service=cone bandpass=infrared verb+ filter+
 31 logo
  1 cd test
  2 ls
  3 mkdir test
  4 cd test
  5 nvo
  6 copy getData("http://iraf.net/nvoss/chart.cl") chart.cl
  7 vocinit
  8 copy getData("http://iraf.net/nvoss/chart.cl") chart.cl
  9 dir
 10 task chart = chart.cl
 11 lpar chart
 12 unlearn chart
 13 lpar chart
 14 ty chart.cl
 15 edit chart.cl
 16 ls
 17 copy getData("http://iraf.net/nvoss/chart_soln.cl") chart.cl
 18 reset clobber = yes
 19 copy getData("http://iraf.net/nvoss/chart_soln.cl") chart.cl
 20 chart abell2255
 21 logo
  1 nvo
  2 cd test
  3 ls
  4 task chart = chart.cl
  5 unlearn sesame
  6 sesame abell2255
  7 lpar sesame
  8 sesame abell2235
  9 llpar sesame
 10 lpar sesame
 11 chart abell1689
 12 chart m23
 13 chart abell2235
 14 ls
 15 cp chart.cl /tmp
 16 ls
 17 del *
 18 ls
 19 ls
 20 logo
  1 ls
  2 !rm -rf test test1
  3 clear
  4 ls
  5 logo
  1 pwd
  2 mkdir test
  3 cd test
  4 dir
  5 copy getData("http://iraf.net/nvoss/chart.cl") chart.cl
  6 vocinit
  7 logo
  1 vocinit
  2 nvo
  3 cd test
  4 dir
  5 copu getData("http://iraf.net/nvoss/chart.cl") chart.cl
  6 copy getData("http://iraf.net/nvoss/chart.cl") chart.cl
  7 dir
  8 registry "" svc=siap bandpass=optical
  9 registry "" svc=cone bandpass=optical
 10 cone regResolver("NED","cone") 0. 0. 0.05
 11 cone regResolver("NED","cone") 0. 0. 0.1
 12 cone regResolver("NED","cone") 0. 0. 0.3
 13 sesame m31
 14 cone regResolver("NED","cone") sesame.ra sesame.dec 0.2
 15 ls
 16 cp /tmp/chart.cl
 17 cp /tmp/chart.cl .
 18 ls
 19 vi chart.cl
 20 vi chart.cl
 21 !ds9 &
 22 task chart = chart.cl
 23 chart abell2235
 24 registry dss svc=siap inter+
 25 chart M51 size=0.25
 26 loogo
 27 logo
  1 cd test
  2 !ds9 &
  3 lgoo
  4 logo
  1 nvo
  2 cd test
  3 task chart = chart.cl
  4 ls
  5 copy getData("http://iraf.net/nvoss/fchart.cl") fchart.cl
  6 vocinit
  7 copy getData("http://iraf.net/nvoss/fchart.cl") fchart.cl
  8 task fchart = fchart.cl
  9 ls
 10 epar fchart
 11 =320*60.*
 12 =320 * 60.
 13 c
 14 vocstop
 15 logo
  1 =regResolver("USNO-B1")
  2 vocstop
  3 vocstop 1
  4 logo
  1 vocinit
  2 vocstop 1
  3 vocinit
  4 logo
  1 =regResolver("GSSC2.2")
  2 vocstop 1
  3 vocinit
  4 vocstop 1
  5 vocstop 1 1
  6 voc init
  7 vocinit
  8 vocstop
  9 logo
  1 vocinit
  2 vocstop 1
  3 logo
  1 vocinit
  2 vocstop 1
  3 vocinit console
  4 vocinit console
  5 vocstop
  1 vocinit
  2 =regResolver("GC2.2")
  3 nvo
  4 ?_
  5 vocdctl debug=0
  6 lpar vocdctl
  7 vocdebug 0
  8 vocreset
  9 logo
  1 vocinit
  2 =regResolver("GSC2.2")
  3 =regResolver("GSC2.2")
  4 logo
  1 vocinit
  2 =regResolver ("GSC2.2")
  3 =regResolver ("GSC2.2")
  4 =regResolver ("GSC2.2")
  5 =regResolver ("GSC2.2")
  6 =regResolver ("GSC2.2")
  7 vocstop
  8 vocstop 1
  9 vocstop 2
 10 flpr 0
 11 prc
 12 vocstop
 13 logo
  1 =regResolver("GSC2.2")
  2 =regResolver("GSC2.2")
  3 logo
  1 vocinit
  2 =regResolver("GSC2.2")
  3 =regResolver("GSC2.2")
  4 =regResolver("GSC2.2")
  5 =regResolver("GSC2.2")
  6 =regResolver("GSC2.2")
  7 =regResolver("GSC2.2")
  8 vocrset
  9 vocreset
 10 vocinit
 11 vocstop
 12 vocreset
 13 logo
  1 nvo
  2 ls ..
  3 sesame m56 verb+
  4 sesame 23
  5 lpar
  6 lpar sesame
  7 sesame m23
  8 lpar sesame
  9 =regResolver("USNO-B1")
 10 cone regResolver("usno-b1","cone") sesame.ra sesame.dec 0.05
 11 registry
 12 flpr 0
 13 registry "radio galaxies" svc=cone inter+
 14 registry "ngc 188" inter+
 15 logo
  1 nvo
  2 imskybot ../ecliptic
  3 ls
  4 cd ..
  5 ls
  6 cd vocl
  7 ls
  8 cd test
  9 ls
 10 task chart = chart.cl
 11 chart ngc176
 12 chart abell2535
 13 type getData("http://iraf.net")
 14 imstat getData("http://iraf.net/nvoss/ecliptic.fits","foo.fits")
 15 !ds9 &
 16 imskybot ../ecliptic
 17 flpr 0
 18 ls ../ecliptic
 19 cd ..
 20 dir
 21 imskybot ../ecliptic
 22 !ds9 &
 23 dir
 24 imskybot ../ecliptic
 25 imskybot ../ecliptic
 26 edit nvo$src/imskybot.cl
 27 imskybot ../ecliptic2
 28 imskybot ../ecliptic2
 29 imskybot ../ecliptic3
 30 imskybot ../ecliptic3
 31 imskybot ../ecliptic3
 32 imskybot ../ecliptic3
 33 imskybot ../ecliptic3
 34 imskybot ../ecliptic3
 35 imskybot ../ecliptic3
 36 imskybot ../ecliptic3
 37 imskybot ../ecliptic3
 38 imskybot ../ecliptic3
 39 imskybot ../ecliptic4
 40 imskybot ../ecliptic4
 41 imskybot ../ecliptic4
 42 imskybot ../ecliptic4
 43 imskybot ../ecliptic4
 44 imskybot ../ecliptic4
 45 imskybot ../ecliptic4
 46 cd test
 47 ls
 48 task fchart = fchart.cl
 49 fchart ../../ecliptic4.fits
 50 lpar wcsinfo
 51 wcsinfo ../../ecliptic4.fits
 52 lpar wcsinfo
 53 fchart ../../ecliptic3.fits
 54 fitsutil
 55 fxhead ../../ecliptic3.fits
 56 fxhead ../../ecliptic.fits
 57 fxhead ../../ecliptic1.fits
 58 fchart ../../ecliptic1.fits
 59 wcsinfo ../../ecliptic1
 60 lpar wcsinfo
 61 clear
 62 logo
  1 cd nvo
  2 vi nvo.cl
  3 nvo
  4 lpar spectab
  5 spectab "" ra=180 dec=1 sr=10
  6 vi src/spectab.cl
  7 spectab "" ra=180 dec=1 sr=10
  8 logo
  1 nvo
  2 d_trace
  3 spectab ra=180 dec=1 sr=10
  4 d_trace
  5 lpar rawcaller
  6 vocreset
  7 vocinit
  8 edit nvosrc$spectab.cl
  9 flpr 0
 10 spectab ra=180 dec=1 sr=10
 11 edit nvosrc$spectab.cl
 12 vocstop
 13 vocinit
 14 spectab ra=180 dec=1 sr=10
 15 cat tmp$stab*.csv
 16 type tmp$stab*.csv
 17 flpr 0
 18 del tmp$stab*
 19 logo
  1 cd nvosrc
  2 nvo
  3 cd nvosrc
  4 ls
  5 vi t_dalclient.x
  6 pwd
  7 cd ..
  8 !mkpkg -p nvo
  9 logo
 82 spectab ra=180 dec=1
 83 spectab "" ra=180 dec=1
 84 edit nvo$lib/specCone.xsl
 85 spectab "" ra=180 dec=1
 86 edit nvosrc$spectab.cl
 87 spectab ../../ecliptic1
 88 edit nvosrc$spectab.cl
 89 spectab "" ra=180 dec=1 sr=60
 90 edit nvosrc$spectab.cl
 91 spectab "" ra=180 dec=1 sr=60 | count
 92 spectab "" ra=180 dec=1 sr=60 | match Qso | count
 93 edit nvosrc$spectab.cl
 94 ls
 95 cd ../src
 96 ls
 97 ty tabout.cl
 98 edit nvosrc$spectab.cl
 99 unlearn spectab
100 edit nvosrc$spectab.cl
101 spectab "" ra=180 dec=1 sr=10 output=STDOUT
102 edit nvosrc$spectab.cl
103 ls
104 spectab "" ra=180 dec=1 sr=10 output=STDOUT
105 edit nvosrc$spectab.cl
106 spectab "" ra=180 dec=1 sr=10 output=STDOUT
107 edit nvosrc$spectab.cl
108 edit nvo$lib/specCone.xsl
109 edit nvosrc$spectab.cl
110 spectab "" ra=180 dec=1 sr=10 output=STDOUT
111 ty nvosrc$tabout.cl
112 spectab "" ra=180 dec=1 sr=10 output=STDOUT
113 flpr 0
114 flpr
115 spectab "" ra=180 dec=1 sr=10 output=STDOUT
116 flpr 0
117 flpr 0
118 spectab "" ra=180 dec=1 sr=10 output=STDOUT
119 flpr 0
120 flpr 0
121 spectab "" ra=180 dec=1 sr=10 output=STDOUT
122 flpr 0
123 spectab "" ra=180 dec=1 sr=10 output=STDOUT
124 flpr 0
125 flpr 0
126 spectab "" ra=180 dec=1 sr=10 output=STDOUT
127 flpr 0
128 flpr 0
129 spectab "" ra=180 dec=1 sr=10 output=STDOUT
130 dir
131 vi spectab.cl
132 ls ../..
133 spectab ../../ecliptic1
134 flpr 0
135 del /tmp/ofile*,/tmp/stab*
136 flpr 0
137 spectab ../../ecliptic1.fits
138 ls /tmp
139 ls /tmp/
140 edit nvosrc$spectab.cl
141 spectab ../../ecliptic1.fits
142 edit nvosrc$spectab.cl
143 lpar count
144 edit nvosrc$spectab.cl
145 spectab ../../ecliptic1.fits
146 edit nvosrc$spectab.cl
147 spectab ../../ecliptic1.fits
148 flpr
149 flpr 0
150 flpr 0
151 spectab ../../ecliptic1.fits
152 flpr 0
153 flpr 0
154 spectab "" ra=180 dec=1 sr=10 output=STDOUT
155 flpr 0
156 flpr 0
157 vi spectab.cl
158 flpr 0
159 spectab "" ra=180 dec=1 sr=10 output=STDOUT
160 edit nvosrc$spectab.cl
161 flpr 0
162 spectab "" ra=180 dec=1 sr=10 output=STDOUT
163 ls /tmp/of*
164 edit nvosrc$spectab.cl
165 flpr 0
166 flpr 0
167 spectab "" ra=180 dec=1 sr=10 output=STDOUT
168 flpr 0
169 spectab "" ra=180 dec=1 sr=10 output=STDOUT
170 flpr 0
171 edit nvosrc$spectab.cl
172 flpr 0
173 spectab ../../ecliptic1
174 flpr 0
175 flpr 0
176 spectab ../../ecliptic1
177 prc
178 flpr 0
179 flpr0
180 flpr 0
181 logo
  1 nvo
  2 spectab ../../ecliptic1
  3 spectab ../../ecliptic1.fits
  4 pwd
  5 spectab ../ecliptic1.fits
  6 flpr
  7 cone regResolver("USNO-B1") 180. 1. 0.05
  8 flpr 0
  9 spectab ../ecliptic1.fits
 10 flpr 0
 11 spectab ../ecliptic1.fits
 12 flpr 0
 13 flpr 0
 14 spectab ../ecliptic1.fits
 15 flpr 0
 16 nvologo
 17 logo
  1 nvo
  2 lpar gsspec
  3 cd ../nvo/
  4 ls
  5 spectab "" ra=180. dec=1 sr=10
  6 gsspec ivo://jhu/sdss/dr4\#80443408212033536 foo.fits
  7 gsspec "ivo://jhu/sdss/dr4#80443408212033536" foo.fits
  8 gsspec 8044340821203353" foo.fits
  9 gsspec 8044340821203353 foo.fits
 10 d_trace
 11 gsspec 8044340821203353 foo.fits
 12 gsspec "8044340821203353" foo.fits
 13 d_trace
 14 gsspec "8044340821203353" foo.fits
 15 gsspec "8044340821203353" foo.fits
 16 gsspec "8044340821203353" foo.fits
 17 flpr 0
 18 d_trace
 19 ls
 20 gsspec "8044340821203353" foo.fits
 21 flpr 0
 22 logo
  1 cd ../nvo
  2 nvo
  3 spectab "" ra=180. dec=1 sr=10
  4 gsspec "80443408207839232" foo2.fits
  5 vocstop
  6 vocreset
  7 vocinit
  8 gsspec "80443408233005056" foo2.fits
  9 oneds
 10 ls
 11 gsspec "80443408233005056" foo2.fits
 12 lpar rspect
 13 vi ssrc/gsspec.cl
 14 vi src/gsspec.cl
 15 ls
 16 gsspec "80443408233005056" foo2.fits
 17 splot foo2
 18 ls
 19 imhead foo2 l+
 20 ls
 21 imdel foo2
 22 flpr 0
 23 ls src
 24 vi src/spectab.cl
 25 spectab "" ra=180. dec=1 sr=10
 26 gsspec "80443408212033536" foo.fits
 27 splotfoo.fits
 28 splot foo.fits
 29 vocstop
 30 vocreset
 31 lpar hedit
 32 vocinit
 33 logo
  1 nvo
  2 oned
  3 pwd
  4 spectab "" ra=180 dec=1 sr=10
  5 dir
  6 cd ../nvo
  7 gsspec ivo://jhu/sdss/dr4\#80443408207839232 foo.fits
  8 flpr 0
  9 gsspec ivo://jhu/sdss/dr4\#80443408212033536 foo.fits
 10 logo
  1 s1 = "ivo://jhu/sdss/dr4#12345"
  2 lpar trans
  3 print (s1) | trans
  4 unlearn trans
  5 print (s1) | trans STDIN "/" "%2f"
  6 print (s1) | trans STDIN "/" "\%2f"
  7 print (s1) | sed -e "s/:\/\//%3a%2f%2f/g
  8 print (s1) | sed -e "s/:\/\//%3a%2f%2f/g"
  9 task $sed = $foreign
 10 print (s1) | sed("-e","s/:\/\//%3a%2f%2f/g")
 11 print (s1) | sed("-e","s/:\\/\\//%3a%2f%2f/g")
 12 print (s1) | sed("-e","s/:\/\//\%3a\%2f\%2f/g")
 13 print (s1) | sed("-e","s;:\/\/;\%3a\%2f\%2f;g")
 14 print (s1) | sed("-e","s;://;%3a%2f%2f;g")
 15 print (s1) | sed("-e","s:\://:%3a%2f%2f:g")
 16 print (s1) | sed("-e","s:\\\://:%3a%2f%2f:g")
 17 print (s1) | sed("-e","s:\\\://:%3a%2f%2f:g","-e","s:/:%2f","-e","s:#:%23:")
 18 print (s1) | sed("-e","s:\\\://:%3a%2f%2f:g","-e","s:/:%2f","-e","s:\\\#:%23:")
 19 print (s1) | sed("-e","s:\\\://:%3a%2f%2f:g","-e","s:/:%2f:g","-e","s:\\\#:%23:g")
 20 print (s1) | sed("-e","s:\\\://:%3a%2f%2f:g","-e","s:/:%2f:g","-e","s:#:%23:g")
 21 print (s1) | sed("-e","s:\\\://:%3a%2f%2f:g","-e","s:/:%2f:g","-e","s:\#:%23:g")
 22 flpr 0
 23 nvo
 24 spectab "" ra=180 dec=1 sr=10
 25 dir
 26 gsspec  ivo://jhu/sdss/dr4\#80443408212033536 foo.fits
 27 cd ../nvo
 28 ls
 29 del foo.fits
 30 vi src/gsspec.cl
 31 gsspec  ivo://jhu/sdss/dr4\#80443408212033536 foo.fits
 32 oned
 33 gsspec  ivo://jhu/sdss/dr4\#80443408212033536 foo.fits
 34 imhead foo l+
 35 vi src/gsspec.cl
 36 lpar hedit
 37 vi src/gsspec.cl
 38 imdel foo
 39 gsspec  ivo://jhu/sdss/dr4\#80443408212033536 foo.fits ; splot foo.fits
 40 flpr 0
 41 splot foo
 42 reidentify foo
 43 identify foo
 44 specplot foo,foo,foo,foo,foo,foo,foo
 45 vocreset
 46 vocinit
 47 =regResolver("2mass-psc")
 48 vocinit
 49 vocinit
 50 vocinit
 51 vocstop
 52 vocabort
 53 vocinit
 54 logo
  1 nvo
  2 nvo
  3 logo
  1 nvo
  2 findspec zlo=0.05 zhigh=0.055
  3 flpr 0
  4 logo
  1 nvo
  2 findspec zlow=0.9 zhi=0.99
  3 findspec zlow=0.9 zhi=1.99
  4 findspec zlow=0.01 zhi=1.99
  5 logo
  1 nvo
  2 pwd
  3 spectab "" ra=180.0 dec=10.0 sr=15.
  4 spectab "" ra=180.0 dec=10.0 sr=15. > z
  5 m z
  6 head z
  7 list = z
  8 list = "z"
  9 while (fscan (list,s1,x,y,z,s2,s3,line) != EOF) {
       print (s1)  ; print (line)
    }
 10 fields z 1,2
 11 vi z
 12 dir
 13 cd ../nvo/src
 14 vi spectab.cl
 15 del z
 16 ls
 17 spectab "" ra=180.0 dec=10.0 sr=15. > z
 18 vi z
 19 spectab "" ra=180.0 dec=10.0 sr=5. > z
 20 fields z 1,2
 21 fields z 1-
 22 lpar fields
 23 ty z
 24 fields z 1-7
 25 fields z 1-7
 26 fields z 1-7
 27 fields z 1-7 | fields STDIN 2
 28 fields z 1-7 | fields STDIN 2-7
 29 vi spectab.cl
 30 spectab "" ra=180.0 dec=10.0 sr=5. outout=zzout
 31 spectab "" ra=180.0 dec=10.0 sr=5. output=zzout
 32 ty zzout
 33 vi spectab.cl
 34 spectab "" ra=180.0 dec=10.0 sr=5.
 35 vi spectab.cl
 36 spectab "" ra=180.0 dec=10.0 sr=5.
 37 vi spectab.cl
 38 spectab "" ra=180.0 dec=10.0 sr=5.
 39 spectab "" ra=180.0 dec=10.0 sr=5.
 40 vi spectab.cl
 41 clear
 42 spectab "" ra=180.0 dec=10.0 sr=5.
 43 vi spectab.cl
 44 spectab "" ra=180.0 dec=10.0 sr=5.
 45 vi spectab.cl
 46 spectab "" ra=180.0 dec=10.0 sr=5.
 47 vi spectab.cl
 48 clear
 49 spectab "" ra=180.0 dec=10.0 sr=5.
 50 vi spectab.cl
 51 print ("foo", >& "STDOUT")
 52 vi spectab.cl
 53 spectab "" ra=180.0 dec=10.0 sr=5.
 54 spectab "" ra=180.0 dec=10.0 sr=5. output=zxc
 55 ty zxc
 56 ls
 57 del z,zxc,zzout
 58 spectab "" ra=180.0 dec=10.0 sr=5.
 59 logo
  1 nvo
  2 findspec zlo=2.25 zhigh=2.27
  3 findspec ra=180 dec=1 sr=10
  4 flpr 0
  5 vocinit
  6 findspec ra=180 dec=1 sr=10.
  7 flpr 0
  8 findspec ra=180 dec=1 sr=10.
  9 flpr 0
 10 findspec zlo=4.5 zhi=4.7
 11 findspec zlo=4.5 zhi=4.7
 12 findspec zlo=4.5 zhi=5.0
 13 findspec zlo=4.5 zhi=4.8
 14 findspec zlo=4.5 zhi=4.8 | count
 15 findspec zlo=4.5 zhi=4.8 |& count STDIN
 16 findspec zlo=1.5 zhi=1.8 ra=180 dec=1. sr=15
 17 findspec zlo=1.5 zhi=1.8 ra=180 dec=1. sr=15
 18 unlearn findspec
 19 findspec zlo=1.5 zhi=1.8 ra=180 dec=1. sr=15
 20 findspec zlo=1.5 zhi=1.8 ra=180 dec=1. sr=15
 21 vocinit
 22 vocstop
 23 vocinit
 24 logo
  1 nvo
  2 findspec zlo=1.5 zhi=1.8 ra=180 dec=1. sr=15
  3 flpr 0
  4 vocinit
  5 findspec zlo=1.5 zhi=1.8 ra=180 dec=1. sr=15
  6 flpr 0
  7 findspec zlo=1.5 zhi=1.8 ra=12.0 dec=1. sr=15
  8 flpr 0
  9 vocinit
 10 flpr 0
 11 vocinit
 12 logo
  1 nvo
  2 findspec zlo=1.5 zhi=1.8 ra=12.0 dec=1. sr=15
  3 flpr 0
  4 vocinit
  5 findspec zlo=1.5 zhi=1.8 ra=12.0 dec=1. sr=15
  6 flpr 0
  7 findspec zlo=1.5 zhi=1.8 ra=12.0 dec=1. sr=15
  8 findspec zlo=0.05 zhi=0.8 ra=12.0 dec=1. sr=15
  9 findspec zlo=0.05 zhi=0.2 ra=12.0 dec=1. sr=15
 10 findspec zlo=0.05 zhi=0.2 ra=12.0 dec=1. sr=15
 11 findspec zlo=4.05 zhi=4.2 ra=12.0 dec=1. sr=15
 12 findspec zlo=4.05 zhi=4.2 ra=12.0 dec=1. sr=15
 13 findspec zlo=4.05 zhi=4.2 ra=12.0 dec=1. sr=15
 14 findspec zlo=4.05 zhi=4.2 ra=12.0 dec=1. sr=15
 15 findspec zlo=4.05 zhi=4.2 ra=12.0 dec=1. sr=15
 16 findspec zlo=4.05 zhi=4.2 ra=12.0 dec=1. sr=15
 17 findspec zlo=4.05 zhi=4.2 ra=12.0 dec=1. sr=15
 18 findspec zlo=1.05 zhi=4.2 ra=12.0 dec=1. sr=15
 19 findspec zlo=0.05 zhi=4.2 ra=12.0 dec=1. sr=15
 20 findspec zlo=0.05 zhi=4.2 ra=12.0 dec=1. sr=15
 21 findspec zlo=0.05 zhi=1.2 ra=12.0 dec=1. sr=15
 22 findspec zlo=0.05 zhi=1.0 ra=12.0 dec=1. sr=15
 23 findspec zlo=0.05 zhi=0.1 ra=12.0 dec=1. sr=15
 24 vocinit
 25 logo
  1 nvo
  2 findspec zlo=0.1 zhi=0.11 ra=12 dec=1 sr=15
  3 cd nvosrc
  4 edit findspec.cl
  5 edit findspec.cl
  6 findspec zlo=0.1 zhi=0.11 ra=12 dec=1 sr=15
  7 findspec zlo=0.1  ra=12 dec=1 sr=15
  8 edit findspec.cl
  9 findspec zlo=0.1  ra=12 dec=1 sr=15
 10 edit findspec.cl
 11 findspec zlo=0.1  ra=12 dec=1 sr=15
 12 edit findspec.cl
 13 findspec zlo=0.1  ra=12 dec=1 sr=15
 14 edit findspec.cl
 15 findspec zlo=0.1  ra=12 dec=1 sr=15
 16 edit findspec.cl
 17 findspec zlo=0.1  ra=12 dec=1 sr=15
 18 edit findspec.cl
 19 findspec zlo=0.1  ra=12 dec=1 sr=15
 20 edit findspec.cl
 21 clear
 22 findspec zlo=0.1  ra=12 dec=1 sr=15
 23 findspec zhi=0.1  ra=12 dec=1 sr=15
 24 getspec ivo://jhu/sdss/dr4\#145464500152172544 foog.fits
 25 task $sed = $foreign
 26 getspec ivo://jhu/sdss/dr4\#145464500152172544 foog.fits
 27 splot foog.fits
 28 vi ../nvo.cl ../getspec.cl
 29 vi getspec.cl
 30 del foog.fits
 31 getspec ivo://jhu/sdss/dr4\#145464500152172544 foog.fits verb+
 32 edit getspec.cl
 33 del foog.fits
 34 getspec ivo://jhu/sdss/dr4\#145464500152172544 foog.fits verb+
 35 splot foog
 36 findspec zlo=-1 zhi=0.002
 37 edit getspec.cl
 38 edit findspec.cl
 39 edit findspec.cl
 40 findspec  zhi=0.002
 41 flpr 0
 42 edit findspec.cl
 43 findspec  zhi=0.002
 44 edit findspec.cl
 45 findspec  zhi=0.002
 46 flp r0
 47 flpr 0
 48 vocinit
 49 findspec  zhi=0.002
 50 edit findspec.cl
 51 findspec  zhi=0.002
 52 edit findspec.cl
 53 findspec  zhi=0.0002
 54 findspec  zhi=0.002
 55 edit findspec.cl
 56 flpr 0
 57 findspec  zhi=0.002
 58 findspec  zhi=0.02
 59 findspec  zlo=2.5
 60 findspec  zlo=2.5 zhi=3.0
 61 logo
  1 nvo
  2 cd test
  3 ls
  4 del pos*,char*.fits,foo.fits
  5 cd nvo$data
  6 dir
  7 ls
  8 lpar imsspec
  9 imsspec abell2235 inter+
 10 bye
 11 nvo
 12 imsspec abell2235 inter+
 13 lpar makewcs
 14 lpar imsspec
 15 imsspec abell2235 inter+ sr=0.15
 16 cd /tmp
 17 ls
 18 fields zz.tab 1,2
 19 fields zz.tab 1,2 > z
 20 lpar wcsctran
 21 wcsctran z STDOUT zx.fits world logical
 22 wcsctran z STDOUT zx.fits[0] world logical
 23 imhead zx.fits[0] l_
 24 imhead zx.fits[0] l+
 25 wcsctran z STDOUT zx.fits[0] world logical
 26 wcsctran z STDOUT zx.fits[0] world logical units="n n"
 27 wcsctran z STDOUT zx.fits[0] world logical units="h n"
 28 wcsctran z STDOUT zx.fits[0] world logical units="h d"
 29 wcsctran z STDOUT zx.fits[0] world logical units="d d"
 30 wcsctran z STDOUT zx.fits[0] world logical units="h n"
 31 lpar wcsctran
 32 phelp wcsctran
 33 wcsctran z STDOUT zx.fits[0] world tv units="h n"
 34 imhead zx[0] l+
 35 imhead zx[0] l+ | page
 36 hfix zx[0]
 37 wcsctran z STDOUT zx.fits[0] world tv units="h n"
 38 =imcur
 39 lapr makewcs
 40 lpar makewcs
 41 ls
 42 ty z
 43 wcsinfo zx.fits
 44 wcsinfo zx.fits[0]
 45 lpar wcsinfo
 46 printf ("%H %h\n", wcsinfo.llx, wcsinfo.lly)
 47 sesame abell2235 verb+
 48 imsspec abell2235 inter+ sr=0.15
 49 lapr tvmark
 50 lpar tvmark
 51 phelp tvmark
 52 lpar wcsctran
 53 phelp wcsctran
 54 lapr tvmark
 55 lpar tvmark
 56 imsspec abell2235 inter+ sr=0.15
 57 q
 58 lpar tvmark
 59 imsspec abell2235 inter+ sr=0.15
 60 q
 61 flpr 0
 62 imsspec abell2215 inter+ sr=0.2
 63 end
    imsspec abell2215 inter+ sr=0.2
 64 q
 65 end
    imsspec abell2315 inter+ sr=0.2
 66 end
    imsspec abell2315 inter+ sr=0.2
 67 imsspec abell2415 inter+ sr=0.2
 68 imsspec abell1415 inter+ sr=0.2
 69 =sqrt (288.)
 70 =sqrt(288.0) / 60.0
 71 imsspec abell1234 inter+ sr=0.2
 72 imsspec abell2415 inter+ sr=0.2
 73 imsspec abell2315 inter+ sr=0.2
 74 imsspec abell2255 inter+ sr=0.15
 75 imsspec abell2355 inter+ sr=0.15
 76 flpr 0
 77 imsspec abell2215 inter+ sr=0.15
 78 logo
  1 nvo
  2 dir
  3 imsspec abell2415 inter+ sr=0.15
  4 sesame abell2415
  5 lpar sesame
  6 sesame abell2215
  7 lpar sesame
  8 imsspec abell2215 inter+ sr=0.15
  9 imsspec abell2235 inter+ sr=0.15
 10 vocreset
 11 vocinit
 12 logo
  1 nvo
  2 imsspec abell2235 inter+ sr=0.15
  3 lpar sesame
  4 sesame abell2235
  5 lpar sesame
  6 ls
  7 cd test
  8 ls
  9 cd nvo$data
 10 imsspec ndwfs.fits inter+
 11 q
 12 tvmark 1 /tmp/zz.tab lab+
 13 vi /tmp/zz.tab
 14 tvmark 1 /tmp/zz.tab lab+
 15 tvmark 1 /tmp/zz.tab lab+ color=107
 16 vi /tmp/zz.tab
 17 tvmark 1 /tmp/zz.tab lab+ color=207
 18 imsspec ndwfs.fits inter+
 19 end
    unlearn  imsspec
 20 end
    unlearn  imsspec
 21 end
    unlearn  imsspec
 22 unlearn imsspec
 23 lpar imsspec
 24 imsspec image=ndwfs.fits inter+
 25 imsspec image=ndwfs.fits inter+
 26 imsspec image=ndwfs.fits inter+
 27 imsspec image=ndwfs.fits inter+
 28 imsspec obj=abell2235 inter+
 29 imsspec obj=abell2215 inter+
 30 imsspec obj=abell2215 inter+
 31 imsspec image=ndwfs.fits inter+
 32 imsspec image=ndwfs.fits inter+
 33 findspec image=nswfs.fits
 34 findspec image=ndwfs.fits
 35 lpar findspec
 36 imsspec image=ndwfs.fits inter+
 37 imsspec image=ndwfs.fits inter+
 38 unlearn imsspec
 39 imsspec image=ndwfs.fits inter+
 40 imsspec image=ndwfs.fits inter+
 41 imsspec image=ndwfs.fits inter+
 42 imsspec object=abell2215 inter+
 43 imsspec object=abell2235 inter+
 44 imsspec object=abell2235 inter+
 45 findspec obj=abell2235 sr=0.2
 46 logo
  1 nvo
  2 imsspec obj=abell2235 inter+
  3 imsspec obj=abell2215 inter+
  4 imsspec obj=abell2215 inter+
  5 imsspec obj=abell2235 inter+
  6 imsspec obj=abell2235 inter+
  7 vocstop
  1 nvo
  2 imsspec obj=abell2235 inter+
  3 logo
  1 nvo
  2 imsspec obj=abell2235 inter+
  3 imsspec obj=abell2235 inter+
  4 imsspec obj=abell2235 inter+
  5 imsspec obj=abell2235 inter+
  6 imsspec obj=abell2235 inter+
  7 imsspec obj=abell2235 inter+
  8 imsspec obj=abell2235 inter+
  9 ?
 10 imsspec obj=abell2235 inter+
 11 imsspec obj=abell2215 inter+
 12 imsspec ra=12.0 dec=0.0 inter+ sr=0.2
 13 imsspec ra=12.0 dec=0.0 inter+ sr=0.2
 14 q
 15 flpr 0
 16 imsspec ra=12.0 dec=0.0 inter+ sr=0.1
 17 imsspec ra=12.0 dec=0.0 inter+ sr=0.1
 18 imsspec ra=12.0 dec=0.0 inter+ sr=0.2
 19 imsspec ra=12.0 dec=0.0 inter+ sr=0.2
 20 logo
  1 cd test
  2 dir
  3 !scp tucana:nvoss/vocl/test/ndwfs.fits .
  4 nvo
  5 pwd
  6 edit nvo$nvo.cl
  7 bye
  8 nvo
  9 dir
 10 reset stdimage = imt1024
 11 display ndwfs 1 bpm=""
 12 nedoverlay ndwfs
 13 ls /tmp
 14 ls /tmp/
 15 edit nvosrc$conecaller.cl
 16 wcsinfo ndwfs
 17 conecaller regResolver("NED","cone") wcsinfo.midx wcsinfo.midy 0.23
 18 flpr 0
 19 conecaller regResolver("NED","cone") wcsinfo.midx wcsinfo.midy 0.23
 20 flpr 0
 21 conecaller regResolver("NED","cone") wcsinfo.midx wcsinfo.midy 0.23
 22 conecaller regResolver("NED","cone") wcsinfo.midx wcsinfo.midy 0.23 |& page
 23 flpr 0
 24 flpr 0
 25 conecaller regResolver("NED","cone") wcsinfo.midx wcsinfo.midy 0.23
 26 conecaller regResolver("NED","cone") wcsinfo.midx wcsinfo.midy 0.23 |& page
 27 conecaller regResolver("NED","cone") wcsinfo.midx wcsinfo.midy 0.23 |& page
 28 conecaller regResolver("NED","cone") wcsinfo.midx wcsinfo.midy 0.23
 29 pwd
 30 pwd
 31 pwd
 32 conecaller regResolver("NED","cone") wcsinfo.midx wcsinfo.midy 0.23
 33 flpr 0
 34 conecaller regResolver("NED","cone") wcsinfo.midx wcsinfo.midy 0.23
 35 logo
 35 gflush
 36 edit nvosrc$imsspec.cl
 37 nedoverlay ndwfs
 38 reset stdimage = imt1024
 39 reset stdimage = imt1024
 40 nedoverlay ndwfs
 41 edit nvosrc$imsspec.cl
 42 registry "" svc=siap bandpass=infrared
 43 registry "" svc=siap bandpass=infrared inter+
 44 nedoverlay ndwfs
 45 cd nvo$data
 46 ls
 47 imskybot essence.fits disp+ ned+
 48 imskybot essence.fits disp+
 49 flpr 0
 50 flpr 0
 51 lpar tabclip
 52 imskybot essence.fits disp+
 53 imskybot essence.fits disp+
 54 dir
 55 display essence2 3 fill+
 56 imskybot essence2.fits disp+
 57 flpr
 58 flpr 0
 59 flpr 0
 60 imskybot essence2.fits disp+
 61 imskybot essence.fits disp+
 62 lpar skybot
 63 imskybot essence.fits disp+
 64 help skybot
 65 cd nvo$doc
 66 ls
 67 cd ../data
 68 imskybot essence.fits disp+
 69 lpar wcsinfo
 70 imskybot essence.fits disp+
 71 flpr 0
 72 imskybot essence.fits disp+
 73 imskybot essence.fits disp+
 74 gflush
 75 flpr 0
 76 imskybot essence.fits disp+
 77 imskybot essence.fits disp+
 78 imskybot essence.fits disp+
 79 =1:12:30
 80 imskybot essence.fits disp+
 81 imskybot essence.fits disp+
 82 imskybot essence.fits disp+
 83 flpr 0
 84 imskybot essence.fits disp+ ned-
 85 imskybot essence.fits disp+ ned-
 86 imskybot essence.fits disp+ ned-
 87 imskybot essence.fits disp+ ned-
 88 imskybot essence.fits disp+ ned-
 89 lpar imsky
 90 unlearn imskybot
 91 imskybot essence.fits disp+
 92 unlearn imskybot
 93 imskybot essence.fits disp+ nh=3.
 94 imskybot essence.fits disp+ nh=.5
 95 imskybot essence.fits disp+ nh=1
 96 imskybot essence.fits disp+ nh=3
 97 imskybot essence.fits disp+ nh=3
 98 dir
 99 imskybot ecliptic.fits disp+ nh=3
100 imskybot ecliptic1.fits disp+ nh=3
101 imskybot ecliptic2.fits disp+ nh=2
102 imskybot ecliptic3.fits disp+ nh=2
103 imskybot ecliptic3.fits disp+ nh=2
104 imskybot essence.fits disp+ nh=2
105 imskybot essence.fits disp+ nh=2
106 imskybot essence.fits disp+ nh=4
107 imskybot ecliptic2.fits disp+ nh=2
108 imskybot ecliptic2.fits disp+ nh=2
109 imskybot ecliptic2.fits disp+ nh=2
110 clear
111 imskybot ecliptic2.fits disp+ nh=2
112 imskybot ecliptic2.fits disp+ nh=2
113 wcsinfo ecliptic2
114 lpar wcsinfo
115 imhead ecliptic2 l+
116 =sqrt (162)
117 =17. / 3600
118 =17. / 3600. * 4
119 printf ("%h %h\n", 1.0, 1.01888888)
120 = 4 * 17. / 3600.
121 imskybot ecliptic2.fits disp+ nh=2
122 imskybot ecliptic2.fits disp+ nh=2
123 imskybot ecliptic2.fits disp+ nh=2
124 imskybot ecliptic2.fits disp+ nh=2
125 imskybot ecliptic2.fits disp+ nh=2
126 imskybot ecliptic2.fits disp+ nh=4
127 imskybot ecliptic2.fits disp+ nh=4
128 imskybot ecliptic2.fits disp+ nh=4
129 imskybot ecliptic2.fits disp+ nh=4
130 imskybot ecliptic2.fits disp+ nh=4
131 imskybot ecliptic4.fits disp+ nh=4
132 imskybot ecliptic3.fits disp+ nh=4
133 imskybot essence2.fits disp+ nh=4
134 logo
  1 nvo
  2 cd nvo$data
  3 dir
  4 imskybot ecliptic1.fits disp+
  5 imskybot ecliptic2.fits disp+
  6 logo
  1 nvo
  2 cd ../nvo/data
  3 skybot ecliptic2 disp+ ned+
  4 reset stdimage = imt2048
  5 ned ecliptic2.fits
  6 ls
  7 del c1
  8 flpr 0
  9 sloanspec ndwfs disp+ inter+
 10 sloanspec image=ndwfs disp+ inter+
 11 ned ndwfs
 12 ned ndwfs
 13 ned ndwfs
 14 ned ndwfs
 15 ned ndwfs
 16 lpar match
 17 ned ndwfs
 18 reset stdimage = imt800
 19 flpr 0
 20 sloanspec image=ndwfs disp_
 21 sloanspec image=ndwfs disp+ inter+
 22 lpar sloan
 23 unlearn sloan
 24 sloanspec image=ndwfs disp+ ned- inter+
 25 sloanspec image=ndwfs disp+ ned- inter+
 26 sloanspec image=ndwfs disp+ ned- inter+
 27 =imcur
 28 =imcur
 29 sloanspec image=ndwfs disp+ ned- inter+
 30 =imcur
 31 =imcur
 32 flpr 0
 33 sloanspec image=ndwfs disp+ ned- inter+
 34 =imcur
 35 =imcur
 36 =imcur
 37 gflush
 38 sloanspec image=ndwfs disp+ ned- inter+
 39 =imcur
 40 logo
  1 nvo
  2 cd ../nvo/data
  3 sloanspec image=ndwfs disp+ inter+
  4 flpr 0
  5 gflush
  6 flpr 0
  7 gflush
  8 sloanspec image=ndwfs disp+ inter+
  9 cd /tmp/
 10 dir
 11 lpar tstat
 12 tstat ttab4607kg
 13 tstat ttab4607kg c1 >& dev$null
 14 lpar tstat
 15 phelp tvmark
 16 sloanspec image=ndwfs disp+ inter+
 17 dir
 18 cd nvo$data
 19 flpr 0
 20 sloanspec image=ndwfs disp+ inter+
 21 flpr 0
 22 gflush
 23 sloanspec image=ndwfs disp+ inter+
 24 flpr 0
 25 gflush
 26 sloanspec image=ndwfs disp+ inter+
 27 =regResolver("FOS","cone")
 28 wcsinfo ndwfs
 29 lpar wcsinfo
 30 cone regResolver("FOS","cone") wcsinfo.midx wcsinfo.midy 0.1
 31 cone regResolver("FOS","cone") wcsinfo.midx wcsinfo.midy 2
 32 cone regResolver("HST","cone") wcsinfo.midx wcsinfo.midy 2
 33 lpar wcsinfo
 34 cone regResolver("HST","cone") wcsinfo.midx wcsinfo.midy 0.25
 35 cone regResolver("HST","cone") wcsinfo.midx wcsinfo.midy 0.25 | fields ("STDIN","3,4,8")
 36 cone regResolver("SPITZERCONE","cone") wcsinfo.midx wcsinfo.midy 0.25 | fields ("STDIN","3,4,8")
 37 cone regResolver("SPITZERCONE","cone") wcsinfo.midx wcsinfo.midy 0.25 | fields ("STDIN","3,4,8")
 38 flpr 0
 39 flpr 0
 40 cone regResolver("SPITZERCONE","cone") wcsinfo.midx wcsinfo.midy 0.25 | fields ("STDIN","3,4,8")
 41 logo
  1 nvo
  2 cd ../nvo/data
  3 lpar wcsinfo
  4 cone regResolver("SPITZERCONE") wcsinfo.midx wcsinfo.midy 10
  5 =regResolver("XMM")
  6 =regResolver("XMM","cone")
  7 cone regResolver("XMM","cone") wcsinfo.midx wcsinfo.midy 0.25
  8 cone regResolver("Chandra","cone") wcsinfo.midx wcsinfo.midy 0.25
  9 cone regResolver("Chandra","cone") wcsinfo.midx wcsinfo.midy 0.5
 10 =regResolver("Chandra","cone")
 11 lpar wcsinfo
 12 logo
  1 nvo
  2 show stdimage
  3 reset stdimage = imt1024
  4 pwd
  5 cd nvo$data
  6 dir
  7 lpar sloan
  8 sloanspec ndwfs
  9 =regResovler("GSC2.2")
 10 =regResolver("GSC2.2")
 11 clear
 12 lpar contour
 13 dir
 14 task radiooverlay = nvo$src/radiooverlay.cl
 15 lpar radiooverlay
 16 radio ndwfs disp+
 17 radio ndwfs disp+
 18 radio ndwfs disp+
 19 radio ndwfs disp+
 20 radio ndwfs disp+
 21 radio ndwfs disp+
 22 =regResolver("NVSS","siap")
 23 siap regResolver("NVSS","siap") 217.29675 34.3895 0.2
 24 siap regResolver("NVSS","siap") 217.29675 34.3895 0.2 0.2
 25 print (regResolver("NVSS","siap","ShortName,ServiceURL",-1))
 26 radio ndwfs disp+
 27 siap regResolver("NVSS","siap","",1) 217.29675 34.3895 0.2 0.2
 28 siap regResolver("NVSS","siap","",2) 217.29675 34.3895 0.2 0.2
 29 siap regResolver("NVSS","siap","ServiceURL",2) 217.29675 34.3895 0.2 0.2
 30 siap regResolver("NVSS","siap","ServiceURL",1) 217.29675 34.3895 0.2 0.2
 31 =regResolver("NVSS","siap","",1)
 32 =regResolver("NVSS","siap","",2)
 33 radio ndwfs disp+
 34 del /tmp/rad*
 35 flpr 0
 36 radio ndwfs disp+
 37 ls /tmp/rad*
 38 radio ndwfs disp+
 39 radio ndwfs disp+
 40 flpr 0
 41 radio ndwfs disp+
 42 dir
 43 lpar contour
 44 dir
 45 radio ecliptic1 disp+
 46 flpr 0
 47 vocreset
 48 dir
 49 radio M33small disp+
 50 logo
  1 nvo
  2 ?
  3 logo
  1 nvo
  2 registry "" svc=siap bandpass=infrared
  3 registry "" svc=siap bandpass=infrared inter+
  4 ?
  5 cd nvo$data
  6 sloan abell2235 disp+
  7 dir
  8 sloan M33small disp
  9 sloan M33small disp+
 10 radio M33small
 11 radio M33small  disp+
 12 nedover M33small
 13 obsllog M33small HST
 14 obslog M33small HST
 15 loog
 16 logo
  1 nvo
  2 cd nvo$data
  3 dir
  4 lpar datascope
  5 datascope ndwfs svc=siap bandpass=x-ray
  6 datascope ndwfs service=siap bandpass=x-ray
  7 logo
  1 nvo
  2 registry "galaxy cluster" svc=cone bandpass=x-ray
  3 !ds9 &
  4 pwd
  5 cd nvo$data
  6 dir
  7 sloan ecliptic3
  8 sloan abell2235
  9 logo
  1 nvo
  2 cd nvo$data
  3 dir
  4 datascope ndwfs service=cone bandpass=x-ray
  5 lpar cone
  6 cone regResolver("GSC2.2") 180.0 0.0 0.05
  7 logo
  1 nvo
  2 sloan abell2235
  3 sloan ndwfs
  4 flpr 0
  5 gflush
  6 lpar sloan
  7 unlearn sloan
  8 sloan ndwfs disp+
  9 sloan ndwfs disp+
 10 dir
 11 cd nvo$data
 12 sloan ndwfs disp+
 13 registry "galaxy cluster" svc=cone bandpass=optical
 14 registry "" svc=siap bandpass=infrared
 15 lpar skybot
 16 skybot ecliptic4 ned+
 17 skybot ecliptic3 ned+
 18 skybot ecliptic3 ned+
 19 reset stdimage = imt1024
 20 skybot ecliptic2 ned+
 21 logo
  1 nvo
  2 registry "" svc=siap bandpass=infrared inter+
  3 registry "" svc=siap bandpass=infrared inter+
  4 logo
  1 nvo
  2 registry "" svc=siap bandpass=infrared inter+
  3 cone regResolver("GSC2.2") 180. 0.0 0.05
  4 cd nvo$data
  5 dir
  6 sloan ndwfs
  7 cd nvo$data
  8 dir
  9 cd
 10 cd test
 11 dir
 12 /Users/fitz/nvoss/vocl/test
 13 cd /Users/fitz/nvoss/vocl/test
 14 dir
 15 sloan ndwfs
 16 logo
  1 nvo
  2 lpar registry
  3 registry "" svc=siap bandpass=infrared inter+
  4 cone regResolver("GSC2.2") 180.0 0.0 0.05
  5 cd test
  6 dir
  7 sloan ndwfs disp
  8 sloan ndwfs disp+
  9 ?
 10 logo
 47 plastic ping "" ""
 48 plastic ping hub ""
 49 plastic ping hub ""
 50 plastic ping hub ""
 51 plastic_req ("-sync -targetName hub ivo://votech.org/test/echo ping")
 52 plastic ping hub ""
 53 plastic ping hub ""
 54 plastic ping hub ""
 55 plastic ping hub ""
 56 plastic ping hub ""
 57 plastic ping hub ""
 58 unlearn plastic
 59 plastic ping hub ""
 60 lpar matc
 61 plastic ping hub ""
 62 ls
 63 plastic loadVOtable topcat demo.vot
 64 plastic loadVOtable topcat demo.vot
 65 cd /tmp
 66 ls
 67 reset imdir = HDR$
 68 flpr 0
 69 imdel *.imh
 70 flpr 0
 71 dir bin$x_dataio.e l_
 72 dir bin$x_dataio.e l+
 73 rfits marek.fits "" foo.imh
 74 ls
 75 flpr 0
 76 imdel foo.fits
 77 flpr 0
 78 rfits marek.fits "" foo.imh
 79 prc
 80 c
 81 flpr
 82 pwd
 83 cd nvodata
 84 plastic loadVOtable topcat demo.vot
 85 plashub
 86 plastic_req ("-sync -targetName topcat ivo://votech.org/info/echo true")
 87 plastic loadVOtable topcat demo.vot
 88 plastic loadVOtable topcat demo.vot
 89 plastic loadVOtable topcat demo.vot
 90 plastic loadVOtable topcat demo.vot
 91 plastic loadVOtable topcat demo.vot
 92 plastic loadVOtable topcat demo.vot
 93 task plasping = nvotools$plasping.cl
 94 plastping topcat
 95 plasping topcat
 96 plasping topcat
 97 plasping topcat
 98 plasping topcat
 99 plastic loadVOtable topcat demo.vot
100 lpar plastic
101 unlearn plastic
102 ls
103 plastic aladin loadImage M33small.fits
104 plastic aladin loadImage M33small.fits
105 aladin
106 plastic topcat loadImage M33small.fits
107 topcat
108 pwd
109 conecaller regResolver("GSC2.2") 180.0 0.0 0.1 outfile=topcat
110 lpar cone
111 conecaller regResolver("GSC2.2") 180.0 0.0 0.1 output=topcat
112 ls
113 del topcat
114 conecaller regResolver("GSC2.2") 180.0 0.0 0.1 output=topcat
115 dir
116 del topcat
117 conecaller regResolver("GSC2.2") 180.0 0.0 0.1 output=topcat
118 conecaller regResolver("GSC2.2") 180.0 0.0 0.1 output=topcat
119 siapcaller regResolver("DSS") 180.0 0.0 0.1 output=topcat
120 siapcaller regResolver("DSS2") 180.0 0.0 0.25 0.25 output=topcat
121 siapcaller regResolver("DSS2") 180.0 0.0 0.25 0.25
122 siapcaller regResolver("DSS2","siap") 180.0 0.0 0.25 0.25
123 siapcaller regResolver("DSS2r","siap") 10.0 0.0 0.25 0.25
124 siapcaller regResolver("DSS2r","siap") 10.0 0.0 0.25 0.25 output=topcat
125 siapcaller regResolver("DSS2r","siap") 10.0 0.0 0.25 0.25 output=topcat
126 plasping topcat start+
127 d_trace
128 plasping topcat start+
129 d_trace
130 plasping topcat start+
131 conecaller regResolver("GSC2.2") 180.0 0.0 0.1 output=topcat
132 plastic topcat highlightObject 3
133 conecaller regResolver("GSC2.2") 180.0 0.0 0.1 output=topcat
134 plastic topcat highlightObject mytable 3
135 plastic topcat highlightObject "mytable 3"
136 plastic topcat highlightObjects "mytable 3"
137 plastic topcat highlightObjects "3 mytable"
138 plastic topcat highlightObject "3 mytable"
139 ls
140 history
141 plastic aladin loadImage M33small.fits
142 conecaller regResolver("GSC2.2") (15.0*1:33:11) 30:22:44 0.4 output=aladin
143 registry ned
144 conecaller regResolver("NED","cone") (15.0*1:33:11) 30:22:44 0.4 output=aladin
145 flpr 0
146 logo
  1 nvo
  2 registry "cool stars"
  3 logo
  1 nvo
  2 registry "radio galaxies"
  3 logo
  1 nvo
  2 registry iraf
  3 logo
  1 real a[20,20]
  2 logo
  1 nvo
  2 conecaller regResolver("GSC2.2") 180.0 10.0 0.1 output=topcat
  3 logo
  1 nvo
  2 conecaller regResolver("NED","cone") 180.0 20.0 0.3 0.3 output=topcat
  3 conecaller regResolver("NED","cone") 180.0 20.0 0.3 output=topcat
  4 logo
  1 nvo
  2 conecaller regResolver("GSC2.2") 0.0 0.0 0.3 output=topcat
  3 =regResolver("GSC2.2")
  4 logo
  1 nvo
  2 conecaler
  3 conecaller
  4 unlearn cone
  5 conecaller
  6 d_trace
  7 conecaller
  8 unlearn cone
  9 lpar coneca
 10 conecaller regResolver("GSC2.2","cone") 0. 0. 0.3 output=topcat
 11 d_trace
 12 registry "cool stars" svc=siap
 13 dir
 14 cd nvodata$
 15 conecaller regResolver("GSC2.2") 180.0 10.0 0.2 output=topcat
 16 logo
  1 nvo
  2 conecaller regResolver("GSC2.2") 180.0 10.0 0.2 output=topcat
  3 logo
  1 nvo
  2 conecaller regResolver("GSC2.2") 180.0 0.0 0.1 output=plastic
  3 conecaller regResolver("GSC2.2") 180.0 0.0 0.1 output=topcat
  4 flpr
  5 logo
  1 nvo
  2 cd nvodata
  3 dir
  4 plastic loadImage M33small.fits
  5 plastic aladin loadImage M33small.fits
  6 which convert
  7 plastic topcat loadTable demo.vot
  8 ls
  9 ls
 10 pwd
 11 ls
 12 cp ../java/demo.tab .
 13 del ../java/demo.tab
 14 ls
 15 plastic topcat loadTable demo.tab
 16 ls
 17 conecaller regResolver("GSC2.2") 180. 0. 0.05 output=tpcat
 18 ls
 19 plastic topcat loadTable tpcat
 20 type tpcat
 21 del tpcat
 22 ls
 23 conecaller regResolver("GSC2.2") 180. 0. 0.05 output=topcat
 24 ls
 25 flpr 0
 26 conecaller regResolver("GSC2.2") 180. 0. 0.05 output=topcat
 27 logo
  1 nvo
  2 conecaller GSC2.2 180.0 0.0 0.1 output=topcat
  3 conecaller GSC2.2 180.0 0.0 0.1 output=topcat
  4 phelp getcat
  5 astcat
  6 bye
  7 ls
  8 bye
  9 nvo
 10 nvotools
 11 ?_
 12 logo
  1 nvo
  2 getcat GSC2.2 180.0 0.0 0.1 output=topcat
  3 cd nvodata
  4 plastic aladin loadimage M33small.fits
  5 cd /tmp
  6 reset imtype = fits
  7 imcopy dev$wpix foo.fits
  8 hfix foo.fits
  9 hedit foo "naxis-" del+ verify+
 10 imhead foo l+
 11 hfix foo
 12 !less foo.fits
 13 del foo.fits
 14 flpr 0
 15 imcopy dev$wpix foo.fits
 16 hfix foo
 17 del foo.fits
 18 from
 19 clear
 20 logo
  1 nvo
  2 cd nvodata
  3 dir
  4 sloan
  5 end
    flpr 0
  6 cd vocl/test
  7 cd /Users/home/fitz/nvoss
  8 cd /Users/fitz/nvoss/vocl/test
  9 dir
 10 task fchart = fchart.cl
 11 !scp 140.252.1.86:nvoss/vocl/test/ndwfs.fits.gz .
 12 !gunzip ndwfs.fits.gz
 13 dir
 14 sloan
 15 lpar findspec
 16 bye
 17 nvo
 18 lpar findspec
 19 sloan ndwfs
 20 flpr 0
 21 logo
  1 nvo
  2 cd test
  3 dir
  4 sloan ndwfs ned+
  5 !ds9 &
  6 logo
  1 =regResolver("GSC2.2")
  2 logo
  1 =regResolver("HST")
  2 logo
  1 =regResolver("NOAO")
  2 logo
  1 nvo
  2 registry "cool stars" inter+
  3 logo
  1 i = 1
  2 dir
  3 = 1 + 2
  4 logo
  1 vocinit
  2 imstat getData("http://iraf.net/test.fits")
  3 logo
  1 vocinit
  2 imstat getData("http://iraf.net/test.fits")
  3 logo
  1 vocinit
  2 imstat getData("http://iraf.net/test.fits")
  3 logo
  1 imstat getData("http://iraf.net/test.fits")
  2 vocinit
  3 imstat getData("http://iraf.net/test.fits")
  4 logo
  1 vocinit
  2 imstat getData("http://iraf.net/test.fits")
  3 logo
  1 vocinit
  2 imstat getData("http://iraf.net/test.fits","foo.fits")
  3 vocinit
  4 logo
  1 vocinit
  2 !ps
  3 type getDat("http://iraf.net")
  4 logo
  1 vocinit
  2 type getData("http://iraf.net")
  3 type getData("http://iraf.net")
  4 type getData("http://iraf.net")
  5 type getData("http://iraf.net")
  6 logo
  1 vocinit
  2 imstat getData("http://iraf.net/test.fits","foo.fits")
  3 dir foo.fits
  4 imstat getData("http://iraf.net/test.fits","foo.fits")
  5 logo
  1 vocinit
  2 type getData("http://cnn.com")
  3 type getData("http://cnn.com")
  4 vocstop
  5 vocinit
  6 logo
  1 vocinit
  2 vocinit
  3 type getData ("http://cnn.com")
  4 type getData("http://cnn.com")
  5 type getData("http://cnn.com")
  6 vocreset
  7 type getData("http://cnn.com")
  8 vocreset
  9 type getData("http://cnn.com")
 10 type getData("http://cnn.com")
 11 vocinit
 12 vocinit
 13 vocstop
 14 vocinit
 15 type getData("http://cnn.com")
 16 logo
  1 vocinit
  2 type getData("http://cnn.com")
  3 vocinit
  4 type getData("http://cnn.com")
  5 logo
  1 vocinit
  2 type getData("http://cnn.com")
  3 type getData("http://cnn.com")
  4 logo
  1 imstat getData("http://iraf.net/test.fits")
  2 count getData("http://iraf.net/index.php")
  3 vocinit
  4 imstat getData("http://iraf.net/test.fits")
  5 type getData("http://cnn.com")
  6 imstat getData("http://iraf.net/test.fits")
  7 logo
  1 type getData("http://cnn.com")
  2 vocinit
  3 type getData("http://cnn.com")
  4 registry "cool stars" interact+
  5 nvo
  6 registry "cool stars" interact+
  7 lpar registry
  8 registry "cool stars" svc=cone interact+
  9 =regResolve("GSC2.2")
 10 =regResolve("GSC2.2","cone")
 11 =regResolve("GSC2.2","cone","ShortName,Title")
 12 registry "cool stars" interacat+
 13 registry "cool stars" interact+
 14 logo
  1 nvo
  2 type getData("http://cnn.com")
  3 vocinit
  4 type getData("http://cnn.com")
  5 imstat getData("http://iraf.net/test.fits","foo.fits")
  6 registry "cool stars" interact+
  7 registry noao interact+
  8 logo
  1 nvo
  2 implot dev$pix
  3 vocinit
  4 registry "cool stars" svc=cone bandpass=optical interact+
  5 logo
  1 nvo
  2 nvotools
  3 type getData("http://iraf.net")
  4 logo
  1 type getData("http://iraf.net")
  2 vocinit
  3 type getData("http://iraf.net")
  4 vocreset
  5 vocinit
  6 type getData("http://iraf.net")
  7 logo
  1 type getData("http://cnn.com")
  2 logo
  1 type getData("http://cnn.com")
  2 logo
  1 type getData("http://cnn.com")
  2 logo
  1 type getData("http://cnn.com")
  2 logo
  1 type getData("http://cnn.com")
  2 logo
  1 type getData("http://cnn.com")
  2 logo
  1 nvo
  2 registry "cool stars" bandpass=infrared interact+
  3 registry M82 interact+
  4 registry M82 interact+
  5 registry M82 interact+
  6 registry M82 interact+ bandpass=optical
  7 registry "ngc 188" interact+ bandpass=optical
  8 logo
  1 =1024./1440.
  2 logo
  1 nvo
  2 =regResolved("HST")
  3 =regResolver("HST")
  4 logo
  1 nvo
  2 =regResolver("HST")
  3 logo
  1 =regResolver("HST")
  2 logo
  1 =regResolver("HST","cone","ShortName,Identifier,Title")
  2 logo
  1 =regResolver("HST","catalog","ShortName,Identifier,Title")
  2 nvo
  3 vocataloc hst 12:00:00 12:00:00 0.5
  4 vocatalog hst 12:00:00 12:00:00 0.5
  5 vocatalog hst 12:00:00 12:00:00 0.5
  6 unlearn vocatalog
  7 lpar vocataloc
  8 lpar vocatalog
  9 vocatalog hst 12:00:00 12:00:00 0.5
 10 vocatalog hst 12:00:00 12:00:00 0.5
 11 vocatalog hst 12:00:00 12:00:00 0.5
 12 unlearn vocatalog
 13 vocatalog hst 12:00:00 12:00:00 0.5
 14 lpar cl
 15 vocatalog hst 12:00:00 12:00:00 0.5logo
 16 logo
  1 nvo
  2 vocatalog hst 12:00:00 12:00:00 0.5
  3 vocatalog hst 12:00:00 12:00:00 0.5
  4 flpr 0
  5 vocatalog hst 12:00:00 12:00:00 0.5
  6 d_trace
  7 vocatalog hst 12:00:00 12:00:00 0.5
  8 print ("foo bar") | scan (s1,line)
  9 d_trace
 10 =line
 11 print ("foo bar rab oof") | scan (s1,line)
 12 =line
 13 unlearn vocat
 14 vocatalog hst 12:00:00 12:00:00 0.5
 15 end
    vocatalog hst 12:00:00 12:00:00 0.5
 16 end
    vocatalog hst 12:00:00 12:00:00 0.5
 17 vocatalog hst 12:00:00 12:00:00 0.5
 18 vocatalog hst 12:00:00 12:00:00 0.5
 19 end
    vocatalog hst 12:00:00 12:00:00 0.5
 20 end
    print ("foo bar rab oof") | scan (s1,s2,line)
 21 =line
 22 end
    vocatalog hst 12:00:00 12:00:00 0.5
 23 vocatalog hst 12:00:00 12:00:00 0.5
 24 end
    vocatalog hst 12:00:00 12:00:00 0.5
 25 logo
  1 =regResolver("HIA")
  2 logo
