{--
--- Day 8: Seven Segment Search ---
You barely reach the safety of the cave when the whale smashes into the cave mouth, collapsing it. Sensors indicate another exit to this cave at a much greater depth, so you have no choice but to press on.

As your submarine slowly makes its way through the cave system, you notice that the four-digit seven-segment displays in your submarine are malfunctioning; they must have been damaged during the escape. You'll be in a lot of trouble without them, so you'd better figure out what's wrong.

Each digit of a seven-segment display is rendered by turning on or off any of seven segments named a through g:
  
    0:      1:      2:      3:      4:
   aaaa    ....    aaaa    aaaa    ....
  b    c  .    c  .    c  .    c  b    c
  b    c  .    c  .    c  .    c  b    c
   ....    ....    dddd    dddd    dddd
  e    f  .    f  e    .  .    f  .    f
  e    f  .    f  e    .  .    f  .    f
   gggg    ....    gggg    gggg    ....
    
    5:      6:      7:      8:      9:
   aaaa    aaaa    aaaa    aaaa    aaaa
  b    .  b    .  .    c  b    c  b    c
  b    .  b    .  .    c  b    c  b    c
   dddd    dddd    ....    dddd    dddd
  .    f  e    f  .    f  e    f  .    f
  .    f  e    f  .    f  e    f  .    f
   gggg    gggg    ....    gggg    gggg
  
So, to render a 1, only segments c and f would be turned on; the rest would be off. To render a 7, only segments a, c, and f would be turned on.

The problem is that the signals which control the segments have been mixed up on each display. The submarine is still trying to display numbers by producing output on signal wires a through g, but those wires are connected to segments randomly. Worse, the wire/segment connections are mixed up separately for each four-digit display! (All of the digits within a display use the same connections, though.)

So, you might know that only signal wires b and g are turned on, but that doesn't mean segments b and g are turned on: the only digit that uses two segments is 1, so it must mean segments c and f are meant to be on. With just that information, you still can't tell which wire (b/g) goes to which segment (c/f). For that, you'll need to collect more information.

For each display, you watch the changing signals for a while, make a note of all ten unique signal patterns you see, and then write down a single four digit output value (your puzzle input). Using the signal patterns, you should be able to work out which pattern corresponds to which digit.

For example, here is what you might see in a single entry in your notes:

  acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
  cdfeb fcadb cdfeb cdbaf

(The entry is wrapped here to two lines so it fits; in your notes, it will all be on a single line.)

Each entry consists of ten unique signal patterns, a | delimiter, and finally the four digit output value. Within an entry, the same wire/segment connections are used (but you don't know what the connections actually are). The unique signal patterns correspond to the ten different ways the submarine tries to render a digit using the current wire/segment connections. Because 7 is the only digit that uses three segments, dab in the above example means that to render a 7, signal lines d, a, and b are on. Because 4 is the only digit that uses four segments, eafb means that to render a 4, signal lines e, a, f, and b are on.

Using this information, you should be able to work out which combination of signal wires corresponds to each of the ten digits. Then, you can decode the four digit output value. Unfortunately, in the above example, all of the digits in the output value (cdfeb fcadb cdfeb cdbaf) use five segments and are more difficult to deduce.

For now, focus on the easy digits. Consider this larger example:

  be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
  fdgacbe cefdb cefbgd gcbe
  edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |
  fcgedb cgb dgebacf gc
  fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |
  cg cg fdcagb cbg
  fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |
  efabcd cedba gadfec cb
  aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |
  gecf egdcabf bgf bfgea
  fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |
  gebdcfa ecba ca fadegcb
  dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |
  cefg dcbef fcge gbcadfe
  bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |
  ed bcgafe cdgba cbgef
  egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |
  gbdfcae bgc cg cgb
  gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |
  fgae cfgab fg bagce

Because the digits 1, 4, 7, and 8 each use a unique number of segments, you should be able to tell which combinations of signals correspond to those digits. Counting only digits in the output values (the part after | on each line), in the above example, there are 26 instances of digits that use a unique number of segments (highlighted above).

In the output values, how many times do digits 1, 4, 7, or 8 appear?

Your puzzle answer was 493.

--- Part Two ---
Through a little deduction, you should now be able to determine the remaining digits. Consider again the first example above:
  
  acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
  cdfeb fcadb cdfeb cdbaf
  
After some careful analysis, the mapping between signal wires and segments only make sense in the following configuration:
  
   dddd
  e    a
  e    a
   ffff
  g    b
  g    b
   cccc
  
So, the unique signal patterns would correspond to the following digits:
  
  - acedgfb: 8
  - cdfbe: 5
  - gcdfa: 2
  - fbcad: 3
  - dab: 7
  - cefabd: 9
  - cdfgeb: 6
  - eafb: 4
  - cagedb: 0
  - ab: 1
  
Then, the four digits of the output value can be decoded:
  
  - cdfeb: 5
  - fcadb: 3
  - cdfeb: 5
  - cdbaf: 3
  
Therefore, the output value for this entry is 5353.

Following this same process for each entry in the second, larger example above, the output value of each entry can be determined:

  - fdgacbe cefdb cefbgd gcbe: 8394
  - fcgedb cgb dgebacf gc: 9781
  - cg cg fdcagb cbg: 1197
  - efabcd cedba gadfec cb: 9361
  - gecf egdcabf bgf bfgea: 4873
  - gebdcfa ecba ca fadegcb: 8418
  - cefg dcbef fcge gbcadfe: 4548
  - ed bcgafe cdgba cbgef: 1625
  - gbdfcae bgc cg cgb: 8717
  - fgae cfgab fg bagce: 4315

Adding all of the output values in this larger example produces 61229.

For each entry, determine all of the wire/segment connections and decode the four-digit output values. What do you get if you add up all of the output values?

Your puzzle answer was 1010460.
--}

import qualified Data.List as L
import           CommonStuff (separateBy)

input = "daegb gadbcf cgefda edcfagb dfg acefbd fdgab fg bdcfa fcgb | cdfgba fgbc dbfac gfadbc\
\\nbdfc dcbegf bf egfbcda gebad cfgaed bfe edfgc aegfcb gebdf | fb fb bcdfaeg fcgdeb\
\\ncebdgaf bfcd gceab bf bfcea gceafd ecdfa fegdab bfcade fba | dfcb dagfbe fbaged bfa\
\\nefabcg aegcdb fgaed fac dgafbc becf eadcgbf aegfc fc cagbe | ecgfa agdef eagfc gdceab\
\\nfcdae cdeabf fga gf gabfde cgadb gadebfc cgfe aegcdf afgcd | fbgadce gadefb fag bafegd\
\\ngecadbf bgc dacgf gaecbf cbeda dbfg bgdca bg bafcgd gdacef | cdgfa fceabg dgfb dgabc\
\\nfbecdga gcdbea cegab fc cafe cfg ebgdf cbgfe afbgec bagcdf | feac acegb bfagce gcafbe\
\\ndefgca fbdcga dbcfeg ag dabg cfagb dbcfg afcbe cga abgedcf | ag gefcbda afecdg ga\
\\ngfecd cabgde bdfgcea ca cdfbag egadb bgfaed cga egcda acbe | ac ac egfbacd cdage\
\\nadefg aecgd bfgae gfd dcgfea fd fcde dcfbag cdbega ecadbfg | gdfea df fd egafb\
\\neafbg bcefad abfcge bdga ad fgade bdafeg agefdcb fecgd dfa | cgbdeaf ad fdgec da\
\\nfea fbadgc bagfc bgeafdc aegbd gbafe gcfe fagecb fe fbdcae | gefc dbfaceg feabg ef\
\\nfebda bacfd bagedc eadgfb dcb deacbgf abedcf fcabg fdce dc | bfcda cd dcfgbea efbda\
\\nab fbcea abc fedgac debfgca cfdea afbcdg ebad bfaced gefcb | abc abde adbe acefdgb\
\\nafbedcg fcbegd aecbfg dfgab cdae cd dcf adbcf fabdec feabc | begfcda dc bfgda debgfac\
\\nabdfge bef cgedfb eagfc bf geacdb aecdbfg gdbec dcfb gbfec | fbcged bf fgabed dfbc\
\\ngfacbe fegcdba fa acdf cgebfd fag gbaed acdfge fdega gdecf | cfda adcbgfe cfegd af\
\\nedc bgecad gfaced deacb de gdfcbae aecfb gabdc acdbfg egbd | gbdac edc cefdag afegdc\
\\nbagdf cagfb acgbef dbfcag egbad bgecafd bdecaf daf df gcfd | bdfag bfgad fd cfadebg\
\\negd dcbgfe gecfd agfbdc egfb ge gcbeadf deafc bgcdf cbegad | edgcf ge efbg ge\
\\ngeadcf bfcda ab abd dagfc dbcfe eabdgc agfb bfegacd gbcadf | gcbfead bad dab abgf\
\\necbgda gfc gfbe bafceg gf ecbfagd cbfda gbaec daefcg cfbag | afdbc fgc fg dcaefgb\
\\ndegbf aecfdb aced adbef gabcdf ea aef beafcg fbedcag dfabc | fae fae fdegb eaf\
\\nfbeacd bfe bgfadc ef eafc fdcegb eacfgbd fadeb edagb cfbda | fdegbac ef dbfcge fdacegb\
\\neadgfb ae egcfb cdfab geca cdefbg bcafe eba bcefag abdefgc | gcae bea abe afbce\
\\nfgace bef dcbfa eb dfcbeg efcab deab efdabgc bdfcae dbcgfa | be feb becfda aebfdc\
\\nabfd da gad egbfd dbgeac adgef gecfa daebgf cfdbge abecgfd | ebcgfad gad gefca bfda\
\\ngbaefc geabd gbfeadc ebgadf feb abcfd fe dacbge bfdea dgfe | ebfdgca fbe feb gcdbea\
\\nac edacf debacg acbf gcfed fbade cea afbdce edcbfga gfdbae | aedbfg abecfgd dfabecg dcabeg\
\\nfabeg dcfgba deacbg bcagf adbgc caf gefcabd fc dfgc edcfab | afbge cdfg bcedfa cgfd\
\\nfgdbae dbfeca gcbef egfdac edgfa cd cde dgaebfc cgad fcdeg | cde ecd cd eadgfc\
\\nga cfdea egfdbc gefad acbegfd geba abfgdc adg gebfd dafgbe | dga dfega egbdcfa gdbfe\
\\nefcda dfgbea egf fdgab fbedcg ge bdcfga aebg edafg acgfdeb | fbcadg eg eg ge\
\\ndefcg gdcbae gfcbde ace afcg ca abfed cfebgad gacfde faedc | ca ac ace bcafegd\
\\negfdab bedac dbecaf cefd fbdac fagbc dbcgea fd adf bdgcaef | agbcf efdc cfadb dfce\
\\nbecdag eagd cgbed gdefbac agbcd bcdefg cfbeda acd ad gabcf | gdebfc ecadbg cgdfeb cbgad\
\\ncdabg bedgac daec afbcgde cabgf dcb dc bagde ecgdfb febdga | debga dace bdgfeac cdagb\
\\nbcgeafd gfb ebdcfa cbeagf bfcea fg afebg dagbe gfec bgdafc | cfge fegc gabef dgeacbf\
\\nfdagecb fed dfabgc fe gabed bdecfg cgfe ebdfca cfbdg egfbd | gcef fgec dfe fde\
\\nbfedc ac cfdbae gfbae acb bgfcde agdcbe facbe gfaecdb dcfa | bcfea bgdfeca efcbgda dcaf\
\\naecbdf eabgcd cb geacb ecb dcbg fabeg adegc fdcgae gbadfec | cafdeb efgba bc bc\
\\ncfebag gcfbd bgadc fcegda dag beacg adbe ecgdab afbcedg ad | deab bacedgf gcefad fgcbd\
\\nbefdac gbdfca adebf dafcb edcafbg def edagb bcfe fe dgfcae | efd fcadb dgfbac ef\
\\nbfcage dcbg edcbag gcadbef bc ebfda agcde deabc bca gdacef | fagbedc cb cbefdga dbgc\
\\neagfbd ebgafc abdcfe ed efgdb gabef fde geda degfabc gdfcb | gead feabg aedg dfe\
\\nbcfdaeg cbedaf fgc gacfed eacfd gc bgfde aceg cedgf cdbgaf | cgf adefc cgfde dgfbe\
\\ncadgbef gbaedf acebf efdgcb bdcgf fcbeg bcadgf eg gef cdge | fge efbdacg faedbg gef\
\\nagdec gadbce ac cae acgb dfegab ebcdfa agebd fegcd gcebadf | aec bgaed ac ac\
\\nbeadgfc ea bceag fgbaec gbcaf cedbg ecaf dbeagf gea bacfdg | ceaf ae caebgf cbfdeag\
\\ndfa fgadc cgefd cdfebga cebdfa gdabc af agfe gcbfed acefgd | fa egaf fbecdg fa\
\\nfecbd fcgbd fdegbc deb abcfdg aegbdf cged ed aebfc bdefcag | gdcbf cedbf bed dcfgeb\
\\ngfdec afbgedc bedga bafe gbfed bdf gfbaed bf gdcfab cbagde | bfae gdfeab bagcdf fgeabd\
\\ncabe be efb decfgb agfbe edfgbac badfg deafcg cgfabe gaecf | be efabg dbafecg eb\
\\nfbce agcdebf dcaefg dcgbaf dfb beadf cadfe bf fcbdae bgdea | faced dfceba adebg bf\
\\ncagdbe agfdec dfega dgcf fabed gafec debfcag dg adg fgaceb | acbdge fdgc bfacge ecabgfd\
\\nafce gebdcfa cbdage gfadec ecdgf fdbagc cf fgc bgefd adegc | bdfgac cf gabcfd cbaegfd\
\\nedfacb cdaeg adgf gaebc agbdcef ead dfcage da degcbf egdcf | dgaf da fegcdba acedg\
\\nab bfacgd agdeb ebadfg adb defgcb gaedc dagebfc gfdbe eabf | beaf baedg ab dba\
\\nfcag gdcfeab gebdfa af bgdfc abf acbfd cdfegb baced abgdfc | fa gafc fab gfca\
\\ngf cdeabfg dcbfe edgba bcgfde ecfg gebdf fgb fdcabg cebadf | edbacf dfgeb gbead fbg\
\\ngfbade da afdbg dag gbdef fade beadgc fdbgec cfbga cfeabdg | ad adg fbagd fegbd\
\\nfdcbea dafbecg ebcfg eca aefcb fcadb ae cdefag dgacbf daeb | ae aedb ace eca\
\\nba dbga gbacf cab aebcdfg fgcad caedfg febcda adfcbg egcbf | agbd ba afebdc badg\
\\nbgcef adfg dfegbca dacge abdfec gadebc af ecgaf gfaedc eaf | efa gdaf eacfdg cfgadbe\
\\nbfa gcebaf gbfdaec ebga efgcb acbdef gcafd ab dfbgec gfbca | ba fgebdc bcfegd dcafebg\
\\nbefagd fbgadc cebdgf acegd bdc cb bfgda gdabc bagecfd cbfa | dfgebc cb fcab cgaed\
\\ngadef dfgbac cfeabd abdge gab dcgaeb adceb cbge edbgacf gb | aebdfc gab efdag agb\
\\ngfcde fgead ag agf gacd debfa gafcbe facedg gdbfec cegafdb | fga ga fga cbefag\
\\ngfaed abedgf edbac cfabge egc bfdaegc cfgd fegadc dcgae cg | gacedf cebfga cafgbe ecgdbfa\
\\nadefgc dg deg cegbad eadbfc abcefgd gdbc daecb eagfb adgbe | ecbda acbfed dg ebdga\
\\nebfda bdgcaf cbfeag gbdefca ecgfa fdc dgce egcadf cd cafde | bcfeagd aedfc gacebf fcd\
\\nbdcgf bgecfad deaf feabgc ebgfad adfgb gadeb fa afb bgdeac | fa bfa dgafebc fa\
\\ndagfcb fcbgdae adb cbea cbdgfe dfega dabgce agdbe ab gdecb | caeb dbcagef cgbde gcdfba\
\\nfcaebg fdbgeac bcg bg efcgb ecbfa fdbace abgced fbga edgcf | bagf afbg gb befadc\
\\nbdfc cdegb fgecb eabcfg db egdfcb aegbfd deb dgace gfcabed | fdaebcg ebd bd bd\
\\nba ecbfd fcebgad cdaeg adbce fbda fecbag bca cfedab cdbegf | dbfa febgdc cab bac\
\\ndgbcfea cea ebgad cgef gdeafc ec agdcf dcgea dbagcf bcaefd | ec aec eac dbage\
\\ndbcfeg ba abedfc fba dcgfa bafdc efbgac afcdgeb ebcdf daeb | ebadfc bdea ab baf\
\\ngcdefab gd fbdag aefgb gade dgb cfbgde cfbda febgca bgefad | gd dg cfbedg dage\
\\nebdg cfedg gebcfa eb ceb bdgaecf edfcag dfbec gfedbc fbdca | be bec gbde cdfega\
\\ndgecfa dab adbce ecgbd gcbfdea fcbgda ab aefb decaf ceabdf | ecdbafg ba adfcbge dgafcb\
\\nacdbgf baefcg dcea dfegb edbcg cd dcg abcge dbcegfa gcbdae | abdgefc ceda cdg deca\
\\ngabef fda afgcbe gabd ebafd gacedf ad eadgfb decbf bgcefda | edcfag adf cagdfe bdefc\
\\nbe cafdbe gcbaefd ebf dgabcf baedf debc fdbac eafgd agecbf | cadfeb dbce dfcbae efb\
\\ngedf gadbc ed fbaegc efacdb feagb degfacb gaebfd ade dbaeg | fgabed cbgda gfeabdc ed\
\\nagde dgcebfa fdbce badcgf dgfaec feg ge fgedc gefbac dafgc | ge geda aegd fedcb\
\\ndcagb gfdbc egcdfba fedbca cfge fedcb dfbceg gfabed bgf fg | febacd edcfb gf abgecdf\
\\ndabfecg dgefb bafgdc aedg efabg gd fbegda aefbgc dgf efdbc | fdecb cebdf gfd geda\
\\nbfacg cgbaef cf gcf fdegbc adefbg feac gbcda abegf gdebafc | dgafceb efdabg ecgfbd gcf\
\\ngdbcaf dcabgfe becg cb gebfa bca gfdbea efadc bfecag fbeca | gbce dcfbage ebcg bcge\
\\ndgbaf faedbg abdgfce gabcd dbf gefd eacdfb faebg bcegaf df | dbf febadgc dfb fdb\
\\nfcaegd db dcbe gcdbfe bfdeg efbga cadefbg dgb fedcg acdbgf | db efgbdca fcbdgae gdafbc\
\\ngbacdf gafcbe dagcb dace ea dbgea cbdgae aeb debgf cfedbga | gbedac dacbge gaedbc afgdcbe\
\\nfgceb abg badcfg dbaec feag ga cebagdf bdcfge agbce cefabg | feag degfcb acbgfd cagbe\
\\ndbfgae fgade cgefba aecdbgf bfg bf befgd dbfa bcgde cedgfa | bdgec fbgcea dafb gfcabe\
\\necgbf afgce gcbdfea ag fcadge dacfe bfacde eag dcga dgafeb | feadc acgd cdaefb cefbg\
\\ncdabf gbdcaf fgceabd efa ae aecd fagebd ecfba cbgef eadcbf | eadc ea bgcef ea\
\\nega efadg eg bgef dbagec cbafde ebgafd dfebgca adefb afgdc | adegbfc ge eg fcdga\
\\necadgf egdcf fdcbga fda afec fgaed eagbd dcgefb fa fbeagcd | daf fda gfabcd adbcgf\
\\ngabef cbefa egabfd gbe badecfg agedf ecdafg gb gbda ebgdfc | gdba aebgf egb fecdga\
\\nafcbe dcbae db efbd egcad bafcge gcbadfe dgcfab bcd dcabef | acbfe bd dbc cadbgfe\
\\ndfcgabe bcafgd cafbed dgabe gbacd dae de egcd bgdeac gfbea | aed efbdgca de cdabge\
\\nabd adcgfe bcdg bcdaeg adbec cegafdb agcde fceab adfebg db | deabc gedca db bad\
\\nbdf efbagcd cgfde bfeg dfgeca bdfceg cgdfb agbcd bf dbfcae | fb gefb cefgd bf\
\\ncga bgdcf ac bcgfa agbdfce gdabfe gcdabe bfcaeg gafbe eafc | ceaf ca bcfag faebg\
\\ngdfb bd dbc adfcbg facbeg eadfbcg abgcf dbcga fecdab gceda | bd bagcf caged gbcfeda\
\\ngabe fcdeab gbdfc fbgadec ebadf edg ge bfegd dfaebg dafgce | fbdage feadb bfagdec bgcdeaf\
\\naegdfb bcf cfbed ebcdgf cb cbdg defgb gcadefb fcgeab afced | fcedb cb bc cabefg\
\\ncef fecag fdgbec agefb ce geafdb dbgceaf egcfba beca gdcfa | agfce aebgf beac fec\
\\nbafdc dgbc bgcfad egafc dg fdg cafgd fbgdea cfgbeda cbdeaf | gfd fcage gd dfg\
\\naegf gbeac fg gdfcab agfecb bgefc bdgace bgf bfcde dcabfeg | dcgabfe caedbg gefcba gefa\
\\ndcgab acfbed acfdgb gefbdca cbgae cfbgd ad gdfa bda bfedcg | da bafdec da acebg\
\\ncaf dafg cagfde fa efgac abcfged bcfdae fcegd gdefbc ecbag | af acbeg cabge fadg\
\\ngfade gaefdc bagecfd dbfeg eacdfb ecfda fgdbac aceg ga gad | bcaegdf cgae bdfaec gda\
\\ncbe edcbag ecfad dbeacfg abedc gcdbef eb agbfcd bega bgacd | bcdag gbdefc ecadbg geab\
\\nfdcbega gaed fdbag fdbgea ag bag gbdfc fcegba edfba dcbefa | gab bafecgd bdagfe fbeadc\
\\ndaf ebcgaf edfb gdeca afdgbc dfega bgafe gcdaebf dbfage df | fd fdbe gbafce edfga\
\\ngebfd eadbg bdcagef befcag df fde fbcd cgefbd gcdaef cfgeb | bgcef abedg afbcegd dbega\
\\ngdcbe cbadgfe egafd agbc abe ecabfd cdbfge egbda ab cdebag | bdgace egfdcb aebgd fgade\
\\ndbc efbad dgeca becda ecgb ecadfg cb gbdacef cdbfag egcdab | bc dcbafg dacgbe faebd\
\\ngbafe dgbefca gdbfe fa ebgcdf bcgae bdaegf fga fead bdfcga | befacdg fa af efad\
\\nfeb gedbaf beafcgd fged bacgfd gbdfa dcbae caefgb efdab fe | fbdea gfabd bceda bef\
\\nefg ef fagdceb fbagde dabfg gadef cadgfb defb gdeca afecgb | bceadfg fe cfaedgb abfdegc\
\\nbafdc gabfd facgbd cbgd bfeadgc eadfbc acfegd gdf fbeag dg | bgcdfae faegb cedbaf dcgaef\
\\ncadeg cdgafe cabge cbaefdg cgdb bfeca bag cbaegd gb dagbef | afebc cefdbag gba bga\
\\nbce gdbcef fcebd dfegba cagbef bc fcdgabe dbgef dbcg ecdaf | bdgc cdafbge cbe bgdcfae\
\\nafdcbge fcgdb gabf dabgfc dfcab bgd bg fedcba bedagc gdfce | bgfedac gb gb bg\
\\nfgacbd bedgfa dfgbec ebgda adbce adfbg gfae eg bcfdgea ebg | ebgdcaf beg acbgfde edcba\
\\nfdgb bdaec befda bf cbefag dcebafg dagef bafedg bef fgdcae | fb dgfb bdfg efgbadc\
\\ncfebga fce beac gcafb egcbf gcebafd bedfg ce adbfgc fedacg | ec beacfg egcadbf bgefd\
\\nbegfda bfcg ecabdgf fb gdfbca cbeadg dbf acfde bgcad abcdf | eagdcb fb bdgca dcafb\
\\ncadgb abd bgdfc gadf ad dfcgbea bagdcf gceab febadc cfgbed | acbge bcfgad cgbea bgcdaf\
\\ncbdfga bdecf fgca gbc gcbdea cgdbaef abfdge dfcgb bafdg cg | fadebcg fcga cfga cbeagdf\
\\ndbefagc fadbe gdbca cgfa fg bgafd bfdcga bgf gadecb dgcfbe | cgfa gbfedc gf dgfebac\
\\necfgba fbeadc bagfe bgac ac gfabced bdeagf ecgaf dfgce caf | gcab ca cebafgd adfgebc\
\\ngba gdbace gcea gabcd cbegadf abdefg gbcdf bfcdea acbed ag | bga ag gba gfbdc\
\\ngfdce gcf aecgfb fegda fcdagbe dfbceg cg acefdb edfcb dbgc | gdbc adcbfge gc deabfc\
\\ndcba bed cedfga fdeab bcfdge dbfagec cdebaf fabge db eadcf | edb bdac bd edb\
\\ndcgb cfgae cdeba cedfab ged aecdg gd acbfedg acdgeb afgdbe | dcabef gd debac dg\
\\nfecag bfedcg egf ef edfa geadbc dfbagec dcfeag acgfb caedg | dfae cfgab fe aecgfd\
\\ndegbca ebadg gafd af gfcbe bfagde fea cgfbade bdeacf gfbea | gdaf fcbge gfabedc fbegc\
\\ndgcfa abcedf bcegad bedg cge fagbce caegd ge dfgacbe bdeca | gedb afbgdce aebdc ge\
\\nbefad afdcb fcbdga bacfged cfb gafc cf dbagc cdbage ebfdgc | efbda cadbefg dgaebcf bfc\
\\nafgecd fgaec bf begfca bgacfd efab gcbed fbc bgecfda bgfec | fcb bfae ebaf edfbagc\
\\nbe afbced agcde cfabd bed dceba cefb gafbcd agedcfb degfab | ecbf fcbe deb cfgdaeb\
\\necgadb cebfdg face egdfac gfe gdeaf fe egfcdab aedgc fagdb | acfgbde afec fe acegdb\
\\nfead adbegc cdgefb adcgef gbcaf bgfeacd gedcf da dgafc adc | bgfdec facgb adef cgefd\
\\nbaedfcg gd gfcae efgbda bagd bgdefc fdg ecafdb gfaed edfba | gefbdac gbda fdg ebdfga\
\\ncbdafg bgfec efcdbag ed gdcef cafdg fde dfaecg aced efagdb | def ed aecd adfcg\
\\nfae cfeba gabe dcbfa ae fcaedg gebafdc dfcbge gcebf acfebg | fbcda egacdbf eaf afe\
\\ncgaf dbgfa cdg gc bacgd bdfacg aedcfgb bdfgce aedbgf badec | dcgbef bdcfag agebdf fdagb\
\\nbdf acgedf dgcfa gafb adecb fadcbg fcdba cfaegbd gdbecf fb | fb abedc fbadc gafb\
\\ncdbegaf dcf afgdbc agbdef cefbd dbefa afce baedcf debgc fc | cf fc ecfa acef\
\\ndb fgcbde dfcb bfged dbgcea dgbecaf bdg afdceg gafeb fgedc | db ecfgd cgadbe gdfacbe\
\\ncaedgbf cgd gcfbe cfdeg fdcbag dc bedgfc dfega ebcfag bced | cadebgf ebcd cgbefa fegabdc\
\\ned adfbcge bfgdac edac dcefgb abcgd gadeb dceagb aegfb dge | feagb baefdgc egabf aegbd\
\\nefdcab gfbead cf begdc fcbde acfe afbed dgbecaf dagcfb fbc | cgaefdb cadgfb bfecd cgbed\
\\nagbfcd bdfcaeg acdbg cdebg bed bgcef de geabcd bacfed gdae | geda gcfeb gade cbdefa\
\\nbcaegd bcd cbfge afbd gfcbd gabfcde db gecdaf dgafcb cdgaf | gfadc afcbgde dfacge gdbceaf\
\\nacfdeb fgabd dfb gdcf gcbaf bgfcad aegbfc fd gbecafd bgade | fbd cfgd bdaefc gafebc\
\\nbedgc dgceba gdacb aebg ebd bdcafg cadfbe ecdfg eb dgecafb | dbe geba be decfg\
\\ncebg geafcbd ecfag gfcda dabefg cfgaeb ce efc bedcfa abefg | ec ce bgec agcef\
\\nfbgde bgfec cb cfaeg adbecg ecfdgb bfdega bcfd bgdacfe ebc | ceb gabedfc dcfb eagfc\
\\ndegfa fceag aegbc gefcad dcgfabe cfad fc fgaebd dgbcfe fce | agecb cf cf dafc\
\\nfebdga gabec abgdc aegdfc adgcf cgbdaf bcfd bd beacgdf gbd | cfdga dfabecg dbcf bcfd\
\\ngabce dacbge ebg eacgd fegdba baecdgf cdbg fbaec gb edfcga | ebg degcfa fdeacbg geb\
\\nagecb begda bfdae deafbg adgcef fcdageb dg dbfg edg fecbad | dbeafc dbfg gd fbgdace\
\\ncgfdab efabgdc eagbdf ed bgdace dcegb dgcab befgc deca dge | ecad ed dcea agbdc\
\\nbdfce dbfeg fc egfbad fbgc dcf fdgbec ecgafdb cfegad edcab | fbcg gefbd fc cdf\
\\ngdb gbead cgbdaef cabd aegcdf geadc bdaceg fagbe dfbgce bd | aedgb cbfgade geadc cegdfb\
\\nbgefdca aefc gdafb bgdcae ac dfegc gfdbce dgaecf fagcd dca | ac cda adc gebcafd\
\\ngdbcea fadcebg cebfad af cedfg abfcgd daf fbae debca efcad | faeb gbcaefd facde adf\
\\ndaebcg fgbeacd dfegcb cbfagd acbde ecbfa gade ed bdcag cde | bcdae bagcdfe cegafdb gcdfab\
\\nbgcedf dfbec efagcd adfbg afdceb ebagcfd dbgfe bgce eg gde | gceb gaecdbf dbfge bcdeaf\
\\nedbca cbdfega fceg dcf fc dfbge fdabeg fdbgec bdgcaf fdebc | dfecbg fc cbaed fc\
\\nfgdc fdaeg cdegfa dga cgaef bgcaef ecfbadg abdef cgeadb dg | adg gda aecbdg gda\
\\ngbce edgbfac dcgfa bg adecfb bcefgd debgfa gdb dcfbe cfbgd | gceb gecb gb gb\
\\nfcgebda deafb dgb gdefb bcgf cdefg bdefcg gafced bg ebcdga | bg cgfdeb gedfb bg\
\\ndeabc cfgbe bfa gaedbc afdc dcegbaf eafdbg fa afcbe daecfb | dacbef fa fa abf\
\\nfeg degcba gbade gdfcea gacebdf fbgdc bafged beaf ef dfgbe | ebfa fe fegbad ef\
\\nebagf abegdc befgca dg bdefc gdbefa dgebf agebfdc dbg dfga | dgaf gd afcbdge adfg\
\\ndefbgc gbd degfab cfbge fbcdg fegcadb fcdag cdbe bd fegabc | db gdb cfebg gdb\
\\ncbedfga gefac bfa ba abdg efgbd afecdb efgab gfbdea bcegdf | debacfg ab ab ba\
\\ngfbdeca cebfg abg gdea fcdgba ga eabfg ebadfg fedab ebcafd | cgdefba ga dfcagb gebfda\
\\ncaegdfb bcdgfa ecadg bagefc cebga edcb dac gcebad fdgae cd | cedb bcfgda cad adegc\
\\negdcaf ged gd fdeac gdfc efadg dbafec becdfag cgdaeb gabfe | edagf dgfc gfcd gd\
\\ncbeafg baedcg bedcgfa gdebf abgfd dbcegf edg ed cdfe febgc | de cbgafe gde acgfebd\
\\nfcd bdacg fd ebdgfc ecagbf gafcde cagdf fecga eadf gebcafd | fd ecgdabf df bfcdeag\
\\ncb agefbc aegfbd dgbc eacbd ecdgba degab cab fdgceba eafcd | fdabgec cgeabf cb cdeaf\
\\nebacdg cbd badf bdcfe db efbadgc abfec aebfdc faecgb efgcd | db dfab dcefg ecafbd\
\\ndaefgc bdcafe fcbega bfcda bgfcdea dcabg fbed bafce fdc df | dbef df acfgdbe cgdab\
\\ncaefd fcaebg bdcfe afdge geafcdb caf ac dagc edgafb agdfce | edcgfa cfa dcga fac\
\\ngcfedb daegbc bcfad gedcf fgecda ae eda agef gadcebf afdce | abgced fgecd adecfg ae\
\\ngf aebfgc cabef cbdge aegfbd acfg gfb ecdbfa fdcegab bfecg | gcfbe bfgeca gf fgdaeb\
\\nbaegf cebaf bfcade bga bfacedg bg egbacd bcfg fcbeag gafed | cefba befcga fedbgca geafd\
\\nfgcd fg adebfcg daebg gdfba fdacbe cbadf gfa gcafeb gdabfc | ebdag fcgd afcbd gf\
\\nbdfegca ae aed ebfgdc gbdae cdaefg gedbf gbfdea ebfa bcdga | dea dcbegf dfebag cdegbf\
\\neagfdc bdfe geafcdb bf afegdb fdega agebf bgcae baf gcdbaf | baf edcgfa bf cabfgd\
\\nfagbcd bfdac bedacg fgcb bc cba acfde gfabd afbdge fdacbge | cbgf bc bcfgeda bcgf\
\\nbgafcde dgc adfgcb bceadg fecda bcage ebgcaf gdeca gd gbed | bdeg cdg agecb acbeg"

intersectAll :: Eq a => [[a]] -> [a]
intersectAll x = foldl L.intersect unioned x
  where unioned = foldl L.union [] x

segmentsToDigit :: String -> Char
segmentsToDigit s =
  case L.sort s of
    "abcefg"  -> '0'
    "cf"      -> '1'
    "acdeg"   -> '2'
    "acdfg"   -> '3'
    "bcdf"    -> '4'
    "abdfg"   -> '5'
    "abdefg"  -> '6'
    "acf"     -> '7'
    "abcdefg" -> '8'
    "abcdfg"  -> '9'
    _         -> undefined

decode :: [String] -> [String] -> Int
decode x y = read . map (segmentsToDigit . rewrite) $ y
  where two   = head . filter ((==2) . length) $ x
        three = head . filter ((==3) . length) $ x
        four  = head . filter ((==4) . length) $ x
        seven = head . filter ((==7) . length) $ x
        five  = filter ((==5) . length) x
        six   = filter ((==6) . length) x
        segA  = three L.\\ two
        segG  = intersectAll six L.\\ four
        segE  = ((seven L.\\ four) L.\\ segA) L.\\ segG
        segD  = (intersectAll five L.\\ segA) L.\\ segG
        segB  = (four L.\\ two) L.\\ segD
        segF  = intersectAll six L.\\ (segA ++ segB ++ segG)
        segC  = seven L.\\ (segA ++ segB ++ segD ++ segE ++ segF ++ segG)
        decode' :: Char -> Char
        decode' c
          | elem c segA = 'a'
          | elem c segB = 'b'
          | elem c segC = 'c'
          | elem c segD = 'd'
          | elem c segE = 'e'
          | elem c segF = 'f'
          | elem c segG = 'g'
        rewrite :: String -> String
        rewrite = map decode'

main :: IO ()
main = do
  print count1478
  print patterns
  print . sum . map (\(a, b) -> decode a b) . zip patterns $ outputSegments
  where outputSegments = map (words . (!!1) . separateBy '|') . lines $ input
        patterns       = map (words . head . separateBy '|') . lines $ input
        nOutSegments   = map (map length) outputSegments
        count1478      = sum . map (length . filter (flip elem [2, 3, 4, 7])) $ nOutSegments