import AoCLib.exts.*
import AoCLib.exts.SeqExts.*

import AoCLib.{Puzzle, Grid as _}
import AoCLib.graph.mylib.*
import AoCLib.graph.BFS

import scala.collection.mutable
import scala.util.Random
import scalax.collection.immutable.*
import scalax.collection.GraphOps
import scalax.collection.edges.{UnDiEdge, UnDiEdgeImplicits}
import scalax.collection.OuterImplicits.toOuterEdge

import scala.util.Random

val s = "ggk: tkd\nvcd: nhn\nxhj: mft rtg hvp sgl\nhgx: dxq stc cfn\nqbl: zgp\ntxm: xsv xrx mcx ggk\npks: hfp hhb lgm zvx\nblp: smh npm\nrkz: nlv\nxkc: dkk bsl rcs\nxtb: ssl kxx bsh cqg\nnhx: chb\nsbb: bcn mcm\ngnq: fxd slj kzp\ncmg: jvc ncx rtx gcs\nqdb: ggh ldk vdv pkn\nznn: jpn\ncrg: zbn\nldp: gtl fbl fkg srk\nmzc: ptq bxs vrv\nsns: mbh xpj\nfhx: mcd pmg hxz brj\nqjv: dpc\npzh: xqs qth\nmjf: gnd dlv\nhmk: gss srb rrp csz\npqj: sgb qlr jnc\nkcs: frq jht tbf cfn\ngnv: cfk tqb nfv\nzmk: tgl vnq hcq\ngsg: ckb qvz dpr pdx rmb\nqzp: pcc mgv zcx nrm nrl\nnlm: pzq\npvf: nkh fzd gqg zdf\nqqr: qsx pkn\nbfr: mrg\njnk: hnx rcs\nvvt: nlq\nbkc: hfp\ngzm: hlh\nmgj: lsl\nsnt: vcv kfm kzq qmr\njtr: xzt qrg fnk\nqvx: dlg pcd\nqbk: rrk jzv vjl gqn\nsmj: sjd lhx\ncsf: dkl gfv mlq xvp\ntkm: mbx\ngtp: rhf fvv hfz ljg\nqxh: xkx frs\npqd: nmz rcg qrj nlm\nzgn: rnl rlh gps\ntkn: zjq ftc ddh\nbgn: nrl jht kqf qqj bkn jgl\nzdf: mlv nrl lqj vsj\nvbx: dtd xcq\nqmr: pls snn cmp\nxkd: rpc\nkhl: dhj lrv\ngff: kcd pcp hcc\nsjv: tkm chf\nqsg: btm ssz pks fpq\ndxq: pzd\nkhf: rcx mtt qcs dsl\nlmp: ztp kss thd bsd\nkbx: fhb tzs jjc\nbft: flm ndl dcp lhz\nccn: zrr hxq\nnts: rpf fkx\nfrx: nvz srl xfk\ngzf: snk tjf\nsbn: xth\ndsk: dnz vrv nsh\nznt: fgx\nfmp: dtd dmr kxj mfh\njgp: zsk jtm ztq qnn\nqxv: vmb cpc pfh gjb jnk\ntjf: vcv\nlqq: nfv znn nrm fvt\ntsf: kpm hnv gxk vtv\nnpk: kcd rgp dll kvb dzc\ntdz: slj rcg\nrcr: nck dnt\nmbx: sjh\npmp: flm mnb\ncxb: tjf gjv txm\nmjp: tlb jdf\nthg: bfn\nrxp: qxz\nltz: xjp plc\ndgc: lzd ldk ckp bdb\nbjf: blb sfj nlv\nmjc: shs qzz\nrtd: kqf bmg zql\nzsk: xvn\nbmg: gfg qgj gpb\nxfm: chf mrk fcg\nsks: pbh jmp mrx\nxvp: bsh\njgm: mnl zqh\nlxn: tdz zbn pcm pkl\nszq: qbl bcn chj\nlhz: ncn czc vrg\ngvj: cgc rfp vls tsq\nbnt: bnz xtg hkt\nmdk: rbh dld lgz snl tmh\nbxs: dfl fnr bzm\nfpk: nzn\nrvb: cfv cck xqs\nztb: jct fpm dfl\nrdv: prs bfn fqp\nzvs: jpz ssl slq khd vgm\nqch: pbf czd gzm\nrgh: gjb pds czx cgp tkn\njsr: pck\nkhd: dth rxh\nsnl: pvq brn\nhhg: fsp mpm tbz qhr kzq\nmcm: vvx\npzp: xcq\nrcg: zbn\nxlq: gzt dpr\nbjr: bfk lpf skv mvs gtj\nrjd: rhg bkz vfm qmr\npmg: fbl fbg hbc\njct: xrc\nhfr: tpv lzd\nlns: qgj\nzkj: pzq bsl\nmdx: ntz nbv kgs\nvgb: xmm pql lsl\ndjs: nhx tpt pcm\nrfq: mfl\nsnn: xrx\nmqg: tbj\nvnq: xqs\nhrn: pzq mtt nbq\nfhk: mtt\nvxv: vgf\ngmg: bdf krv tjf ltj nmq klx\nxkx: jhr mqg\nhzx: qtj dfv pps\nmvz: xnn tgb pdl czg\nxjx: nhd\npss: mvh nxd bfr gpm\nqxz: qgk\nrpc: pcd lhx\nrjc: kqn hnf rxp xmm\nmxn: vgj jct jrd\nlhl: hfg\nspn: xck njp trd tvn\nvgq: rgp\nqrg: zlq\ngqm: frf vcv\nfxs: kxz fcg dlz\nvrb: gfz tpm bmk cvv dnk\nlzd: rfz kds\nsds: jrd hfz\nkbj: nrd prr jct bhn\nnsh: ncg cqm\nbtj: nts hlr fkl mxn qfh\njhm: pcm jmd\nmnx: jhj kkf hdn bfd\nnjm: dqh\nfhb: gth lcq\nbff: tvc prs kbs\nmrx: jqx\nnqp: cvz\npvc: xth\nnbq: ccq\nfnx: cdb\nmnk: lsl frp pcv\nznv: btd cgl ddj cvc\nbng: mdg dfv\nfjr: fcm zpp klv\njnc: bpc\nbgr: nkh qzm cdv cgj\nntf: ftl gnm bdq zdl\ndzt: tgl lpf\ndds: nbq mlm kxd\nssb: shs jdc\nnkq: zpr bpc jsv jrj\nhml: rcg\npkl: xjv hnx lhx\ntvt: dnk ltb\nbkn: hbh xpk vsz\ndqs: vsz\nlbt: nqt jqt\nbmt: vqf fzk fkn\nnmq: npl\nzrc: bqz zgg kjh sck\nmkp: dtb\nlnr: kzp ghs lpc rss hrn\nvfm: qmr mkv\nfff: ddj\nzxt: dhv vcd ltp jmd\nkvl: khj chf\nncv: dtp zkq txl\nckt: kcz vhn znt\nrkg: rbh jtt nlm fqh\nptb: tkm qrt bkb bgb\nvvg: cqg mdx srl rxf\ndfr: nmf glx pbh rzm mrg hfq\ntpt: nxb qrj\nxjn: lhx zdf kpm tsq\nzfk: kbq hlr\nrrj: zvd mfv ndb\nldg: gxk jsv qhq\njms: ttz gfg jpg kkk gzm\nrlz: bcn\ndml: lnj mkp kzr gzt\nshb: dqs rbh mmq\nzln: tlb rrc dsc\nkpl: bzq xjp qgc gzr kjr\njzv: fbg kjv\nkgs: tbz\nrhh: zkv\nnkn: zrt rxs\nhpg: gjg vbx qtf msm\nzjv: crf jzr\nfdd: brt qqs ssb\nsbf: vtv lpm skp\ntpm: kds jjb hkb\nhkt: psb nlf vhs\nkbs: lld ptf\nkzm: fpv msm gcd khj\nxxz: zgd\npsd: pzd\njlx: khg mjf qhr zkx pdj\nncl: qxh\njdc: dzc mdg krv\nmrp: kzp kbq nzx\nssl: kds\nsgj: rcq rvr prr\nzkx: znl\nnhl: fhr\npdx: ptq nzn\nmnn: jpz pdc\nssx: qjv dtj fxs\ntpv: ssl nkl\nbmj: bsl\nncx: mnk\nnck: tgq tbz\ngnz: tbt dqs frq nsh\nkzg: qtg pcc zbf jhm\nntk: skk vlk nsd tfn\nbbm: lft qzm\nqkf: lhj xsv bdn tjt\nnkl: fff hrt mfh\nfgz: ptf qch\nncb: tmh nzx bqz zfk\ncbf: ltm hml ncn\nsck: bnt fhj vdd\nffb: xqz kks rhh\ndpz: tvj lnj mtn kxh cqm\nmlm: tvc ggm sfz qtg\nqzm: hcr lpm\nkcd: nxm\nmvj: pxn srr fps\nkkx: zgv jjb zgh\ntns: sln nsq\npdm: lvv ssx svk vzs\nvts: qvn phh tms drt\nbtm: mbm rgp\nxxp: chb hrg\ndfv: ltb thk rdl xjx\ntth: czb qcq ttx qbl\nvtg: vkl tbt rxv nxp\nnzs: brn tss zpr hlh\ndvd: nmq dth qfm\ngnm: vcv npl\nqsx: ssz krf jtp\npfh: ptq\nqgj: pvq\nrpk: hlg pmp lbc\nffz: kks qlr hdb rkq dzk\nvhr: chd dds npz txn\nqgc: rtx\nsnc: hpg vpr grr\njkl: ktp mfz czg bch vcd\ndpd: pbf lbl nxp gld xkc\nlch: bkv kvl bmk rzz mpt\nmpk: jmx cbr dbh\nrzn: xbq dzp mhb rtd\nvls: tvc blb vfh\ngpb: mhx tzt\nhfg: skg\njmj: msl ncl xps zjv\nlvh: qjp knx qrj qvx\nksb: mnb njm ckt\nhqt: jng ppd\nnbt: fhr lmr cck\nlrv: mjc\nvjg: xtx svk php ddj\nxhg: gjh bpc pdx\nqsl: ljg gzm\nsqr: zgv xbt xcm jmp\nzbf: znt mgb\ngck: srk hfq ntz nhd\nggr: znl hdl lmr mgj\nfzs: xmk jgp xvn mgj\ntph: hnp ksb jzl nkh tvj nsg smj\njtm: vcv fzk\nxfq: mnl vfm jxd lrv\nlxc: bjf xzn\ntsq: gss znt fsf dpr rmr\nfjd: gth\ntml: ndx nrd thg prs\nngh: vsj dgg vnb bgr\nsmx: lcf msm vgq pnv\nkbd: xpf qtj rhg vkt\nfkx: mlv mzv gzt\ntzp: rcg\nxsv: hxz\ntxg: xrc tqb\nbqc: klj ddh tvc\ndkl: rzz txl qnf\nqtz: zrr knp pzh\nsxg: fgz lbg hmm xhf\nnph: dlj smh\ntgf: hml nxp qgj\ndnl: szf jnr sth jjb\nxks: npp\ncpm: mbf sff dfl gvc\nmgd: xpf hlt kvp phq\nmtc: xjp khj zgh\nbkb: sns mdn xzb dtd\nzcx: nzn\nrqr: nnd zxt zhz fhk\nplz: rxh srn dlz ppd xsj zlq rmv\ntjm: ncp sbz kpm rmb\nsjd: dmj\nvsj: tvc\nrxs: msc\nvvk: mrr pck\nrrf: ttx ddb txn bhn vgj zgp\nrbh: kxh\nktp: fnx ttl qrj\npcl: kxx phx tgl\nnpz: tgf nmd\nlhd: sct gnv tkk npz xbn\ndcm: psn nsd hpp mjp rtg\ncbq: bfr gcv cxb cmh\nmmd: qvj klv\njdb: rcr jxx\ndll: qnf xjx\nrrz: vbx\nmcx: srk tgl\nvzj: tnd\nvsz: vvx\nnmp: xll\nxbd: hfr jdq rsx xpf\nbsd: pnz\nfcq: mnn\nrcq: qhq vvm psb\nxsp: hvp rxg brt srl\nmgz: nmd\nrfd: xll pdz\nvfh: fvz rkf fgx\nzps: zxx tkz xzb ssp\nzfh: qzr tgt ltp cgl\nnpt: csz vhj\ndtj: mfh tvt\nbpn: mnb zhz vrg lnj\nvzs: jgq\nnsq: flm zcp dhd fpl\nkks: cpc vgj\nppd: szp pzp mrr\ncfz: zpn fcq vvh\ncpq: jlc vbl cfk\nkvx: scn\nssd: xjx bxg ngq xth\nnzr: fjd jjc\npps: rdl rrz\ngqn: mks xtv fzt\nhlg: pjq\nfcm: pfh crf kcz\nvbg: kvl vgf vqf fzk nhl\nsss: shb jtt tsg zkh\nsvx: mmq rmb\nmgq: sjq hrt hzx bdf\njkv: zxx\nbnm: kcz lpm dld\nltj: xjm\nhcv: skp tbt\nczc: vsj\ngmr: nhx fht\nkpt: hlg rbk\ndmr: pdc\nrlc: frs ppc vhn btd\nfgn: jhj\ndxs: gpb vtv jhr ftx\nddz: lqd cfv\ndqh: vkn\nldk: jjl mgj\nmjv: xpq hxv hpt\nvnd: nfv npz crt gvp\nfbk: vvn hdr nvn gjk nkh\nlng: fhj mlm qcr hsr\njrp: zvx ncv gsv\nnnk: rpc bpx rhf\nqcq: jhp tsg\nxbt: ftl qrg fbn\njdq: rrz jgp\nzkt: gtp dtb fvv kss\ngdz: tgt hnp sds ppc\nnsv: tgp szp lhl mbm pdz\nhcr: xtg\nzbb: vjt vgq szf\nngq: hvp dkp\nrxg: rrc bfr\nldq: bhn fkl kpt bjf\nbxg: kmc snn hbj\njnz: dzc nck qsx mvj\nxpz: sdd mks bsh jqx cgf qtf\ndmj: xrc cvc\nbdb: fjd\nmft: mlq tkd hrt\nxdm: fbn\npfv: bch bzm\nthk: cqt grc\ntfn: qth kvp\nmdn: ssx kqn ltb pzh\nmfz: rdt ccq\nftx: nzn mlm\nndl: pjq slj\nmvs: dvd lhl bqb jvc\nnrl: mgv\nsrh: kxz tgl qgc qnc jvq\nlpj: nhl grr btm pmc bzq\njlk: nbt mgr ssx lpt\ndrg: jrp gdc vnq dth\nghg: qvn zgg brn nkf\nbnz: hcr fnr\nkjq: zqh\ntgb: sgj sjd jqt jqs\nfbz: btt dsl xlm\ncgc: hls grt fqk\nrhg: krv mrx\nsgr: fpm cvz\nfkl: hls zcp fct\ngfz: srk cck ssz\nhnp: kxd fvz\nmbm: lpt\ngjk: dzp sfj xps fvm\ncqg: svk\nnhf: qxx lpb xxz jsr\ntdb: pbf rdt\nfnv: pmp hkt hfz\ngfb: fgl fkj kvp spx\nnzn: ndb\nxnn: grt qbl npz\ncsb: ttf fkx fkz\nrgp: ggh\nsgb: mtq phc\njhv: bkc bbs\nbmc: ndb jmd lnj zgg\nrmd: pdl vbl\nbsz: mnx rqn zxx\njtt: blb\nxfk: vqf hlt gsj\nmrj: gsv xzb xdt xdm xsv\nqcs: mmd zfk\nrvr: sbb fpm mzv\nkmc: hfg lpf jgq\nczg: czx btt\nhtp: xbk lsl mbh sjv\nztk: ccq lns qfh xpk\nkdf: hqk prd npd mtc\nfck: rjh mbq mtr\nsdg: qjp qqj pgl\nlgm: rrg mgh\nvpg: rdr snc gzf zgn\nhdk: vkn stv msc tdz\nhbc: ttj\nqvd: bzm\nzkq: nbv xcf\nfjf: zps grp grc qxx\nvqk: mgk xfq xcm rnl\ndld: gvc npm\nptq: vsz\nnxd: hvg\nbfk: kvx\nnhh: qvl jmx bdf qjt gcv\nlrr: dlz dlv\nkfh: kct pzp vzs\nzvx: lhl\npsn: pdj lkv zld lfm\nsdv: klj cdb sln rkz\nzrr: ntz xpf\nmkm: pvq bff stv fnv ckt\nbdn: bkc qvc\nptd: lbk slz xlb cpc\nrkk: ngq jcz fbn xtf\ndlj: zpp nsg rmb kzr kxh\ndnm: xxp ndl kkk dxv nrd vbl\nsxt: bng prd jtc fps bfd\nzpr: jhp\nhdl: zgh prd\nzdl: lfd\nmcd: vgm ncv vvk\nxzg: phq vgh mbm\nnkf: fnv ncz vhj tmh\nvvn: rxs mgv mbl lbc\ntgp: zld cgr\nndd: dzc mrx krk\nqvl: tlc dnh qrg\nljg: nzx zrs\nfct: dhd sbb mgz\nsjz: sjq\ndzp: bmj stt\nvhn: mtt\ncvv: hbj fgp\nfnk: kgs\ncxp: qxp mfl jgl smj\nspx: ggk\nsvk: rsx\nzlg: tqb nfg nph\nrtx: qth mzj\nqrf: hbc hpp cmg rhg\nrbn: sbb jqt nkf sbz\ngrt: bhn\nmfv: bbm rmr qqj\nsct: mgb bnj\nvvm: fhj nsg znn\nfqk: hlh\nhmt: gjb svx\npqv: rnl\nmzf: fhr kvx cmp krf\ntkp: dsc jsh dzt tzs\nlqd: lpt\nllj: dfl mff rxv pqj\ntbt: dxv gss\nlpc: dxv mcm\nfgl: qfm\nnhr: qgj gld hrg dxq\nscl: dzk chh mzc\nchh: sgr tdb bbr\nklx: mjf xcm pzb\njrd: bhn xlm\nqzr: sfj jhr cqm npm chh\nbgb: fjz vkt\njsh: rxh vkt\nmcs: qzz pls zlq\nxdt: grc rmv\ncfk: nxp nhn\nlfm: lcq\nlvf: jjl prd lrr\nvgn: mbx mjp slc\nrrg: kxj\nmnv: bmc pjq ncg mgz mqz\ntgz: mnk zmt kgs\nqmh: ttf cbs npt mrp\nknx: xjv tqm\nrjx: tpm pqv dnh mkv\nmpm: tkz fcq dpc lfl qvc\nkpm: nlf\ngcd: ktm rrk vfv\nfbg: djr\ntkt: nnd svx bnr qcs\nzhk: mtr xbq nnk\ndhj: fgl bbs\ndcp: hxv mkp xqz\nkps: kvx ltz dbh qjt\nkjt: vgf bdf\ndbh: vpr mnk\nfhr: mgh hbj\nnmf: kbx nsv\nmpq: fpk cbf nxb\nqhs: mvj rqn vzs mkv pqv\ndlp: htq qxz fgn jvq hkb\nmhx: vvx\nbbr: rqd\ntrj: ldp lfl tkm qvl dvd\nxtn: qnf vzj jdb\nkrv: ltb\nmkk: ncp jqs hfz\ncgk: bfn\nmsc: qvj\ngqg: qlr bch tzp\npcv: fjz\nhbv: lpt mrk\nstc: mtt\njzl: rmr\nxmm: qzz\nhks: qgj\nzqs: nbt kcd xzg zxp\nnvt: jdc rjt nqd ssp\ntld: lpk cmp rfd tlc\nmtn: ftc qvj\nbdq: jgq ddj\njmq: xdm jkv bdb zkq fsn\nlfk: bfk frf dmr\nhrg: fhk dlg\nzjk: xkd rss\nvgj: snl kcz\nzhz: fqp btt\ngng: xcm fgk qvc\nbmb: cfn flm dxv lxc\npcp: xtq hcc\nkdv: bct njm jkl bkn\nkjv: zpn fps rdr\ndhd: zcx mmd\nbpt: cfv hdl jgm gjv snc\nfcz: jhv ftm lmr xzt\nsnk: fkj\nkzp: fpk zbf hcv kxh\nsfz: zpp\ndqz: dgk ffz lpm tdz\nnjv: xks cbb\nzhp: bzm bpn hrg zgj\nbzq: grc pdc\nssp: fzt pps hbc\nbnr: stc tkh\ntcv: psb dlg hxv\nlmm: dfv lrr dzt\nglx: lkv\njdd: ltj hnf bzp\npkn: rhg sjh\nxfv: cqt vnq qtj fpv\njhp: rdv kjh\ncjq: tcv mkk rkf\nlgr: vjt dth vvt dtp\nlbk: nrd hnv\ngtj: lrr zxx\ntss: vrv jpg rdt\nlcg: pdj dlv vbg zmk\ntsp: mgv sbz rbf vnb\nghv: mrx pdn slq\ndjr: sjh\ndpc: lqd\npxn: zpn\nlbf: qfm nxd dlz dth\nchd: rcs\nxjv: mpq sjd kbj\nrvv: hbj hvp bgb drz\nfsf: bpc rjf ttl\nqnv: pcl cfz gbz hdl qgc\nxtm: nhn cgk ztb\nnhd: zgd\nsbk: vdv kkx\nqrt: tkd rrk vlk\nndq: tms kcz psd tsp rtl qhb\nztq: sbn vpr xtq\nlzx: tqb mtn npt\njgl: nkn rjh\nrkq: qnp stt csb\nrtc: plv khl xmk\nzkh: dkk\nkls: slz czd vrm mcm\nrzk: mgh hqt kjq ntf\ntsz: shs rjt\nzzr: ltm prs njm xtg pzq\nfrf: nlq cgr\nrmp: jlx ssb mpt nqz\npdj: rtg\nvmp: mks mbh zlq spx\npnv: gfv jgm hfr\nttx: njm jsv zfk npp\ngtq: fps gsv gth pbn\ngxk: qnl ckt\nnkg: bmt jtm pbh\nqxp: fxd rcq zkh\nxzb: jhj\njvq: frf\nmzj: jmx\nbpx: nqp xqz qsl rkf\nznl: rrg\nmkv: fsp cqt\nmgh: vgf\nsjq: bkj\ncfc: xzj ffm cqg jjc\nxck: xvp jdb\ntbf: lhx\nqbh: dlz pzp vbg xgn jdd\ncgf: mnk rxf qxq\nnqt: tjn qnp\ngtl: zsk cqt\nmmq: czd\nldj: nqt ccq\nqsk: phh dqh sfz dtb\nkvb: rnl gng pcl\nspk: lpt nvz jpz\njtc: vcv\nfqh: pdt\nhxq: zgv jtp\nmzt: ldj cvc bjf qvx\nnzz: rpc cdv sln zdd\nvhq: nsq ttf fpk\ntgt: hmt bpx\nvlj: mvh frp\nqcr: frs bbr knx\ngml: xlq pnz gzt pfv\nhlv: fdd cmg tfn nvz\nfkg: bkc fbg qtf\nplv: nvz sgl rfd nzc\nnmv: kjq pmc pck\ntxl: brt nxm\nhhb: fbl\nclq: vkl ncp nmd fvm qxh\ngcv: rcr zgd\ncnh: tsg tkk hms nhx\nkfm: fpq zsk\nbdf: vcv\nhpt: czc jht vvx\ngqc: zld xqs szp\nvrv: rjh\nfhj: vxj ddh\ngjs: bhn gfh tbf ffb\npzb: pmc tgq\nqtb: xhf nsh prr jmd\nbsv: ztp flz lqj\nkct: bfk kkf nmv php\njzr: nkh crg jqs vrg\nxdb: nhn bmj bsd\ngjb: pzd\nsjg: nzc hfq tzs phx\npxh: bsz jdb jgq\nfvv: fpl\nzrt: xpk zrs nxp\nnfv: kzr\nlvp: mff nxb smh rpx\nrpf: lzl rqd\ncbs: fgx pnz hdk\nnfp: lqj rdt znn hcv mjk\ntxb: tld xdt rxp zxx\nqfh: pds\nmlq: kvp kjt\nkrf: tgq\nftm: xtv gsv\nsrr: jsr fkj hhb jtp\nmhb: vtv czd fnx\nqnn: fbg ndd\ndbr: smh\nhmr: krk fgn hch jrm\nhls: mhx\nzvd: blb\ngxh: tlb xtq\nzdd: ppc qcq ttx\nhqk: jgb bqb\nttz: mkp dtb mll mbq ppc\nvkl: bct\nhdr: lns vhn ggm qvd\npzq: rrz\ncql: chh vhj\ndgk: tcv mlm\ntvn: kkf mrk jgb\nxxt: hls\nxcf: gjv nlq\nghs: crg ndb\ngnd: jpd jng bbs pnp qgk\nvml: ndx jgl jjh fck\njhj: mbh\nphp: pdn\ndzb: fpm pbf lld nmd\nmbq: qhq\nhvg: kdr\ncqp: thk mpt\nrfp: jzr gmr bsv\nvpr: lsm\nxps: czd pcd cbf\nbtl: hxq sjz cmh lhj\ngsv: mvh\nnzf: fxs vlj slq djr\nbzp: jpd nxm\nnjp: ltj jkn brj ghv\njrm: vcv fmp pls\nrbf: mmd znt\nvxj: rbk flz ggm\ntmm: hbh ngs nbq hcr\nskp: bct\nvmb: tbj mhx\nndx: vkl vbl\ngjv: kdr\nqzg: bdf nxm ntk hvp\nzcp: nqt ccq\ngpf: lcq fnk sbk\nbxc: qqr fjd ggk smt\ngvm: czx ftx lqj lbt gxk\nbgs: gxh rmv smt jtc drz\nrrp: lpm tzt mgz\npdt: sct xxt chj\nhlr: bzm\nzld: jmx\nfzk: bzp\nrrc: lrv bkz\nxcn: tkd ccn tlc jjb\nrxn: fcg gps vvt\nfmd: znd ghs dsk plx cvp\npdn: rtg qgk\nptl: qsx kxj pnp\ncvb: szf kjr zxx frx\nltp: jpn slz\ntqz: hkt xlq pjq\nsln: fqk\nqnf: ggh\ndrz: rhg pmb\ntkd: sjv\nsrn: sjz\nnzc: dpc\nlpk: mcs hvg\njjf: gth rjt\njvc: fbn jng\nlnd: dgf zxp gqm qhr xcq\ntkk: hnx jlc rkf\nlfl: ckp fjz\nzxp: nzr ftm mnk\nnxb: qvd\nffm: hhg hvg rxg hqt\ngld: drt xqz\nrqn: vgh bsh\npdl: vvx bcf\nxmk: xdm\ngcs: jcz rfz\nsdd: bkz gfv slq\ngvp: cpm blp ncl\nvfv: jzv mcs\nkhx: cbr sbn hvg mpk rmv\nfzz: txp sns kvp qfm\ndtd: gbz\nvxn: rbn ncz pdt lxn\nsts: frq bmg bct dgk\njmd: xkx\nqkd: gfg pcc jpn zpr xxp\nnsd: cmh pls\nfmb: ttj szp lkv\nkdl: rcx fqp lgz\ndnk: vxv xpf\ntkh: qvx tqm vbl\nbkz: dnh\nvdv: jsr\nmtq: hdb jtr\nszf: krk snn pvc\ntdn: bsd cdb zkv fjr\njqx: pnp\nzzp: lpk lpb vlk qjv kjt\ndrd: zkh fhj vhq bnm\nkjz: spx gnm xgf xjp\nvvx: tbj\nvnb: dbr hsr\nhxv: ncg\npnz: bch stv\nfzt: gjg\nmsm: fmp\nhms: lgz bxs\nhnf: dnt brt gqc\nzrs: chb rfq\nclp: gvc nlm xpq skp\nthd: bbr pfh brn\nsbz: vhs\nfqp: tmh tqm\nzjg: msc fhk xks scl\ndnt: cqp\ntlz: qnl tbt rbn kbs\npbn: fgl jnr qjt\ngrp: plc tfx\nfvt: lft cgj xkd\nncp: ccg\nxpj: pvc\nqhb: hdb\nrrk: djr\nhch: rrg xzt zkx pxn vzj grr\ntjt: jnr lbv\ncpc: klj\nlvv: cmp nlq jgb\nstp: kfh lbv qjv hfq\nkgp: qnp cgp rfq nqt\nfzd: cbb ptf\nglg: rrj lbl dzk skp fnx pzd znn\nqjh: rkz nqp mgv\nngs: rlz lns cqm xhf\ndhv: rlz czg qtg\ncbr: msm\nxlb: rcg nqt rmr\npql: kbx zln ggh\nxhf: nlf\njqs: hks xlm ccg\nscn: sjq pcp xth\nkjh: zbn pvq\nkzq: zvx mnl\nsjr: lpc ggm tsq zjv vrm\nrtl: bnj ktp\nqxx: rfz cqp qqr\nqtr: jgb lvf tgz vgq hbv\nlcf: dtj pnv qxz\nmqr: mgz mhl cdv bcf\nhlt: hrt lfm bfd\nfhf: fzt kdf xxz qhr xsj\ngdc: lfd rjt kjv\nkhg: xmk mrg cgr\nccg: prs mbq\npbl: htq frf jdq ddz\nstt: ldj sgr\nsvc: czc jpg bct vmb\nrdr: jkv\njjh: rss nts fqh\npgl: npm zjq bcf xks\nrcd: xzj xfm gqm cbr\njfz: bsd jhr\nrpx: vhs\nhxz: xzt\nmfp: rjh flm xtg\nhgs: nmq jbq htq bkz\njjs: phq jmx lmm ncx\nrzc: hpp zbb nqd zqh\nsff: bnj rqd mtr qvz\ncdv: jrd drt\nkkk: xzn txg\nchj: gjb\njgd: bqc xxt fvv jsv\nfpl: mfl chb\ndpr: ncz\nmsl: jnc hml\nqnl: qnp ztk\ndkp: tgq dnt fff xfm fmb\nbtr: nmf xll sgl bkv rxn\ndzk: hdr\nkss: stv zkv\nmff: chb nsq\nfsp: lpb mzj\nmhl: bct xzn fqk\nvlk: jdf\njng: xtv\ngzr: hbv vjt bng jsb kkx\nslz: rbk\nhnv: tqb\nnct: vkn pbf lgz\npsb: kzr\ntcb: dtp lpk fgn zkx\nxbn: tnj ddb xps\nczb: dgk fnr mhb\nfkz: nrm ncz kpt ddh njv\nslr: xgn krk fcq qnv\ncrx: tzt smh drt dxq\nzgj: mmq cbb mmd zvd sfz\nnrm: pcc xpq\nfgk: rxp zdl\ndlv: jqx\njqt: rlz\ncnr: nzn rjf xtg znd\nvvs: jnc rtl blp stc\nvtv: rmb\ngrr: zgh tfk rxp\nmbs: nct hsr ddb xtm chj\nhcc: xrx\ndnq: frq csz tvj cgk\nkrn: jjh jpn rgh zlg dkk\nlbl: jtt\nnhm: rzz xxz npl\nqjk: dhj sbn fpq xpj znl\nrhf: dlg ltm\nmlv: njm hls\nkjr: pbn grp ttj\nphq: pdc tlc xcf\nrhn: bdq qtz ptl nkg\nbbs: hcc khd\nmrg: vgm vlj\nzql: zcx qqj vhj\nktm: pzb tfk jxx\nqcz: hnv skp nlm zjk\nrsm: zgp njv vnb rss xlm\njhd: ssz nxd lmm mrx\nzzm: fkn hdn hvg mgj\nvkt: fpv\nqxq: dnh jkv dzt\ndnz: lld zvd tsq\nznd: hdr sjs rpk gld bsl\nzfz: bcf mfp kpt pfp\nhgn: lbl cjq kxd pfv\njbq: frx vgb dmr\nhcq: nmp pvx jjl htq\ncvp: rpx bct grt\nmgk: kvb tvn tsz\nnqk: ccq nlv bnz\nvtx: zjk fqk mzv pfp\ntjn: tzt npp tbj\nbgd: rhh pzq kdl nqp\nkln: hgx psd dbr rjf\nhbh: crf\nflz: xpk dqh vkl\njpg: qhb\nzzq: crg njh qbl ldg\nsvn: tqz znv fbz ptf\nnrq: kxz ddz tfk bmk\nlfb: lhz tpt rfq hcv\nnfg: hlr dlj\nrsj: thg bqz qjp zjv\nckb: tjm bcn dbr\npcm: fst\ngjg: phx\npmb: jtp lhj gxh skk ktm\ntxp: vzj mvh qjt\ngps: qzz\ndsc: mcx bqb\ntnj: cvz bnr tns psd sgb\nzjq: chd hmt\nfpq: hrt\nxsj: ggk jpz spk\nnnd: zgp klj\nrml: btd gss fht pfp\nnqd: bkj hqk\nhmb: shs krf\ncbb: btd\nfrn: sbk xbk bzp jxx\njrj: zkj lzx hml tbf\ntsg: bct\ndgg: xxt kqf vrv cgk\nggq: cdb bcn rbk sqd\nmrr: mdg nzr\nmxj: jmp rtc jtm bqb\nmbf: cbf qhq qzm\nlbg: kxd fpl xzn\nkzd: djs bzr xxp cql fgx\nsrb: rkz klv cdv\nhqs: cdz qxq jjf gtj\nbzr: snl rpf tgt\nvrr: mfz pfv fgz qjp\nksn: hdn lqd rxn\nzgd: fnk mgr vxv\nxgf: hvp lmr tvt sks\nzst: hnx rjf cvc rmd\ntms: qvd qjh thg\nqbz: ltb ncx skg ksn cvv\nmnb: tvj\ntkz: xcq\nrsx: frp xmm\nbnj: ncn cgj bfn\nsmt: jcz pvc\nvdd: zkj psb mqg\npvx: vxv mnl zdl lgm\nfsn: qgk nhd fkn fzk\nlfc: lfm cgr jsr ccn plc\nnlf: vrg\nrqm: zhp cgl lhz szq nlv rrv\nnmz: nqt lld mzc jnk vkn\nhrf: mtq cgl chd pmp\nsth: pmc pxn gpm\nvqf: vvh hxz\ngvv: vcd txg bbm fsf hms kxd zkv\ncsn: jpz pqv jgb xtn\nsqg: pnz mfv rrv crg hlg pds\nmjk: rxs xkc mgb nfg zpp\nckp: fff ztq\nfrs: csz\nftl: mgr\ngxt: dgc mjf nmp gps lfl qnn\nxtf: ftl pks mpt hpp pck\nmtv: khj gtx vvt mgq qjt\nmll: mqg hks cgj gvc\nxbk: dnt rzz\nrxv: qtg hml\nttf: jlc\nqnc: dzt mgh\ntnd: gff nbv sbn\nrxf: gtl btl sjg\ngjh: btt\nslc: tjt mnn mbx\nskg: vgh\nlzl: bzm klv\nhfp: fpv\nmbl: rbk xjv dpz\njkn: phx fkj srl\nnsg: klj\nknp: tzs vdv mtc brj\nfvm: prr cgp\ndzc: lfd\nphh: ltm tzp\nhlh: tqm\nlbc: pjq\nrrv: qhq jzl\nslj: xzn\npcr: xhg lbk mzv rpx qvj\nphc: nqp fzd\njht: rcx\nnvn: sds cbb mjv\njsb: khl bdb krf msm\ngls: php lzd vgm jgb\nlhf: npt lhz nxb hbh cql\nrcx: dfl\nmqz: gmr rhh tcv\ngfg: tdb\ndgf: xmg zld nhl\nttj: ltb\nxgn: mks snk\nqtj: dtp mdg bkj\nsjs: vhs rbf lbt qhb\nmzv: pfp lpm\nttl: gzm\nxjm: vgm\nnpp: cdb nhn\nnbv: tbz\nbsl: prs\nbsh: gbz\ngpm: rxh fhb\nbgk: sdg hlg xdb mqz mzv\nvrm: cfn prs\ncgg: tbj bmj ttl gsg mtr bqz ztp nqk\nfbl: kdr\ngtx: tfk vgf\ntxn: cpq qsl\nkrk: kdr\nkqn: mrg zgv hfg\ngfh: xtm sbf fnr nzx\nspl: vzj xdm tkz vgh\nqff: bdn vgn rdr pcv xck\nhtq: jxx\nkxz: jpd\nxpq: ldj\nnqz: rlh zmt tgp qtf\nskv: hmb gzf pdz snk lkv\nckd: prd nqd vzs jjc kct tpv nmp\nmrk: xrx\nrlh: ggh lrr\ngfv: lfd zqh gcs\nftc: rqd zkv\nnpd: glx ntz ssz\npbh: vvh\nkxm: jmp nhm lsm brj pcv\nczx: jfz\nvjt: mjc bqb\nxtq: sbn\njjl: mzj tkm\nfst: dsl fht\nxsb: hks qlr znt tss\nkqf: phc\nhhp: dmr fjz hhb qnv\nlft: jsv\nkbq: jlc dsl\ncvz: crf\ntrd: pvc dll xvn\nfxd: ncn cgp jhm\nbdv: fgk chf nxd gjv\nlsm: npl lcq\ngnb: gjh lbc fst csz\ndgz: jhv tkd gjg jtc rvb\nbrj: kds kps jpd\njdf: jcz plc\njsv: fht\nbkv: sjz krv\ncdz: frp skk cmh\nqvn: thd ncl ncg rcs zhk\nmmp: sgl bgb xjm qth\nxqn: qnc vjt xvp lfk\nfvz: fpm xrc\nfgp: xrx xll\nlhj: kxj\nqvz: lft tzp\nqqs: rtg glx mvs\ntfx: jjf fmp mzj\nrdl: thk mgr\nhkb: kjq skk\nzns: mfh ltz pxh tlb\ndvv: mtt hsr rmd lxc\nlbv: fkn dzc\nkkp: lmr sjh hmb bmk\nqvc: bkj\ncrt: npt pds dqs\ngsj: vvh jtr xjm gbz rfz\nszp: pdz\nzjc: mgb znv ztp ndb\nvjl: cmp fgp lpf bfd\nzmt: zsk rrk\nhmm: rrv mzv gnq\nrzm: kkf jnr srn ssb\nnjh: xkd lzl pcd dkk\nzpn: dtd xzj\nsnb: lpk srn jcz gpf\ndmb: sfj hfz gjh ddb\ncck: pnp rjt\nxtx: xpj rrc kfm hfg\nxbq: zcx vtv\nvbp: rtc mgj lpb rrz\nflm: hdb jzl\nxmg: xvn nmq rgp\nnvm: jvq nbt fcg gtx\nplx: fct nph bnm rmr nkn kcs qfh fqh\nsqd: zgg dmj dzk tns\njxd: jsh kcd kvx phq\nlqx: hdl nzc tsz vfv\nkxx: xtv xzj\nqjp: jfz msl mfl\njlq: cfv skg vvk pcp\nxvh: vvk gtq hdn hfp dkp"
//val s = "jqt: rhn xhk nvd\nrsh: frs pzl lsr\nxhk: hfx\ncmg: qnr nvd lhk bvb\nrhn: xhk bvb hfx\nbvb: xhk hfx\npzl: lsr hfx nvd\nqnr: nvd\nntq: jqt hfx bvb xhk\nnvd: lhk\nlsr: lhk\nrzs: qnr cmg lsr rsh\nfrs: qnr lhk lsr"

val es = s.asStrs.flatMap { case s"$from: $to" =>
  to.asWords().map(t => from -> t) }

val g = UGraph[String]()

val fg = es.foldLeft(g) { case (g, (ef, et)) => g.addEdge(ef, et) }

val toDel = Set("mtq" -> "jtr", "pzq" -> "rrz", "znv" -> "ddj")
val newG  = UGraph[String](fg.edges.filterNot(e => toDel.contains(e) || toDel.contains(e.swap)))

val res = BFS.components(newG.vertices, newG.neighbours)
res.map(_.size).product
println(fg.edges.map((f, t) => s"$f,$t").mkString("\n"))

val wes = es.flatMap((f,t) => Seq((f -> t,1),(t -> f,1))).toMap
val wg = WeightedGraph(wes)

val rvs = Random.shuffle(wg.vertices).take(40)
val prs = rvs.zip(rvs.tail)

val resm = mutable.Map.empty[(String, String), Int].withDefaultValue(0)

val fts = prs.flatMap{ case (l, r) =>
  val sps = ShortestPath.shortestPath(l, r, wg)
  sps.zip(sps.tail)}

fts.foreach(e => resm(e) += 1)
resm//.toSeq.sortBy(_._2)(Ordering.Int.reverse).take(3)

//val spd = ShortestPath.shortestPath(wg.vertices.head, wg)
//val sps = spd.toSeq.sortBy(_._2._2)
//val near = sps.map(_._2._1).take(20)
//val far = sps.map(_._2._1).takeRight(20)
//val fts = cartesian(Seq(near, far)).map{ case Seq(f,t) =>
//  val path = ShortestPath.extractPath(f,t,spd)
//  path.zip(path.tail)
//}.take(100).flatten.toList
