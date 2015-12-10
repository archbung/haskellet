module Daubechies where

-- Reconstruction LPF

type Coeffs = [Double]

db1 :: Coeffs
db1 = [7.071067811865475244008443621048490392848359376884740365883398e-01,
       7.071067811865475244008443621048490392848359376884740365883398e-01]

db2 :: Coeffs
db2 = [4.829629131445341433748715998644486838169524195042022752011715e-01,
       8.365163037378079055752937809168732034593703883484392934953414e-01,
       2.241438680420133810259727622404003554678835181842717613871683e-01,
       -1.294095225512603811744494188120241641745344506599652569070016e-01]

db3 :: Coeffs
db3 = [3.326705529500826159985115891390056300129233992450683597084705e-01,
       8.068915093110925764944936040887134905192973949948236181650920e-01,
       4.598775021184915700951519421476167208081101774314923066433867e-01,
       -1.350110200102545886963899066993744805622198452237811919756862e-01,
       -8.544127388202666169281916918177331153619763898808662976351748e-02,
       3.522629188570953660274066471551002932775838791743161039893406e-02]

db4 :: Coeffs
db4 = [2.303778133088965008632911830440708500016152482483092977910968e-01,
       7.148465705529156470899219552739926037076084010993081758450110e-01,
       6.308807679298589078817163383006152202032229226771951174057473e-01,
       -2.798376941685985421141374718007538541198732022449175284003358e-02,
       -1.870348117190930840795706727890814195845441743745800912057770e-01,
       3.084138183556076362721936253495905017031482172003403341821219e-02,
       3.288301166688519973540751354924438866454194113754971259727278e-02,
       -1.059740178506903210488320852402722918109996490637641983484974e-02]

db5 :: Coeffs
db5 = [1.601023979741929144807237480204207336505441246250578327725699e-01,
       6.038292697971896705401193065250621075074221631016986987969283e-01,
       7.243085284377729277280712441022186407687562182320073725767335e-01,
       1.384281459013207315053971463390246973141057911739561022694652e-01,
       -2.422948870663820318625713794746163619914908080626185983913726e-01,
       -3.224486958463837464847975506213492831356498416379847225434268e-02,
       7.757149384004571352313048938860181980623099452012527983210146e-02,
       -6.241490212798274274190519112920192970763557165687607323417435e-03,
       -1.258075199908199946850973993177579294920459162609785020169232e-02,
       3.335725285473771277998183415817355747636524742305315099706428e-03]

db6 :: Coeffs
db6 = [1.115407433501094636213239172409234390425395919844216759082360e-01,
       4.946238903984530856772041768778555886377863828962743623531834e-01,
       7.511339080210953506789344984397316855802547833382612009730420e-01,
       3.152503517091976290859896548109263966495199235172945244404163e-01,
       -2.262646939654398200763145006609034656705401539728969940143487e-01,
       -1.297668675672619355622896058765854608452337492235814701599310e-01,
       9.750160558732304910234355253812534233983074749525514279893193e-02,
       2.752286553030572862554083950419321365738758783043454321494202e-02,
       -3.158203931748602956507908069984866905747953237314842337511464e-02,
       5.538422011614961392519183980465012206110262773864964295476524e-04,
       4.777257510945510639635975246820707050230501216581434297593254e-03,
       -1.077301085308479564852621609587200035235233609334419689818580e-03]

db7 :: Coeffs
db7 = [7.785205408500917901996352195789374837918305292795568438702937e-02,
       3.965393194819173065390003909368428563587151149333287401110499e-01,
       7.291320908462351199169430703392820517179660611901363782697715e-01,
       4.697822874051931224715911609744517386817913056787359532392529e-01,
       -1.439060039285649754050683622130460017952735705499084834401753e-01,
       -2.240361849938749826381404202332509644757830896773246552665095e-01,
       7.130921926683026475087657050112904822711327451412314659575113e-02,
       8.061260915108307191292248035938190585823820965629489058139218e-02,
       -3.802993693501441357959206160185803585446196938467869898283122e-02,
       -1.657454163066688065410767489170265479204504394820713705239272e-02,
       1.255099855609984061298988603418777957289474046048710038411818e-02,
       4.295779729213665211321291228197322228235350396942409742946366e-04,
       -1.801640704047490915268262912739550962585651469641090625323864e-03,
       3.537137999745202484462958363064254310959060059520040012524275e-04]

db8 :: Coeffs
db8 = [5.441584224310400995500940520299935503599554294733050397729280e-02,
       3.128715909142999706591623755057177219497319740370229185698712e-01,
       6.756307362972898068078007670471831499869115906336364227766759e-01,
       5.853546836542067127712655200450981944303266678053369055707175e-01,
       -1.582910525634930566738054787646630415774471154502826559735335e-02,
       -2.840155429615469265162031323741647324684350124871451793599204e-01,
       4.724845739132827703605900098258949861948011288770074644084096e-04,
       1.287474266204784588570292875097083843022601575556488795577000e-01,
       -1.736930100180754616961614886809598311413086529488394316977315e-02,
       -4.408825393079475150676372323896350189751839190110996472750391e-02,
       1.398102791739828164872293057263345144239559532934347169146368e-02,
       8.746094047405776716382743246475640180402147081140676742686747e-03,
       -4.870352993451574310422181557109824016634978512157003764736208e-03,
       -3.917403733769470462980803573237762675229350073890493724492694e-04,
       6.754494064505693663695475738792991218489630013558432103617077e-04,
       -1.174767841247695337306282316988909444086693950311503927620013e-04]

db9 :: Coeffs
db9 = [3.807794736387834658869765887955118448771714496278417476647192e-02,
       2.438346746125903537320415816492844155263611085609231361429088e-01,
       6.048231236901111119030768674342361708959562711896117565333713e-01,
       6.572880780513005380782126390451732140305858669245918854436034e-01,
       1.331973858250075761909549458997955536921780768433661136154346e-01,
       -2.932737832791749088064031952421987310438961628589906825725112e-01,
       -9.684078322297646051350813353769660224825458104599099679471267e-02,
       1.485407493381063801350727175060423024791258577280603060771649e-01,
       3.072568147933337921231740072037882714105805024670744781503060e-02,
       -6.763282906132997367564227482971901592578790871353739900748331e-02,
       2.509471148314519575871897499885543315176271993709633321834164e-04,
       2.236166212367909720537378270269095241855646688308853754721816e-02,
       -4.723204757751397277925707848242465405729514912627938018758526e-03,
       -4.281503682463429834496795002314531876481181811463288374860455e-03,
       1.847646883056226476619129491125677051121081359600318160732515e-03,
       2.303857635231959672052163928245421692940662052463711972260006e-04,
       -2.519631889427101369749886842878606607282181543478028214134265e-04,
        3.934732031627159948068988306589150707782477055517013507359938e-05]

db10 :: Coeffs
db10 = [2.667005790055555358661744877130858277192498290851289932779975e-02,
        1.881768000776914890208929736790939942702546758640393484348595e-01,
        5.272011889317255864817448279595081924981402680840223445318549e-01,
        6.884590394536035657418717825492358539771364042407339537279681e-01,
        2.811723436605774607487269984455892876243888859026150413831543e-01,
        -2.498464243273153794161018979207791000564669737132073715013121e-01,
        -1.959462743773770435042992543190981318766776476382778474396781e-01,
        1.273693403357932600826772332014009770786177480422245995563097e-01,
        9.305736460357235116035228983545273226942917998946925868063974e-02,
        -7.139414716639708714533609307605064767292611983702150917523756e-02,
        -2.945753682187581285828323760141839199388200516064948779769654e-02,
        3.321267405934100173976365318215912897978337413267096043323351e-02,
        3.606553566956169655423291417133403299517350518618994762730612e-03,
        -1.073317548333057504431811410651364448111548781143923213370333e-02,
        1.395351747052901165789318447957707567660542855688552426721117e-03,
        1.992405295185056117158742242640643211762555365514105280067936e-03,
        -6.858566949597116265613709819265714196625043336786920516211903e-04,
        -1.164668551292854509514809710258991891527461854347597362819235e-04,
        9.358867032006959133405013034222854399688456215297276443521873e-05,
        -1.326420289452124481243667531226683305749240960605829756400674e-05]

db11 :: Coeffs
db11 = [1.869429776147108402543572939561975728967774455921958543286692e-02,
        1.440670211506245127951915849361001143023718967556239604318852e-01,
        4.498997643560453347688940373853603677806895378648933474599655e-01,
        6.856867749162005111209386316963097935940204964567703495051589e-01,
        4.119643689479074629259396485710667307430400410187845315697242e-01,
        -1.622752450274903622405827269985511540744264324212130209649667e-01,
        -2.742308468179469612021009452835266628648089521775178221905778e-01,
        6.604358819668319190061457888126302656753142168940791541113457e-02,
        1.498120124663784964066562617044193298588272420267484653796909e-01,
        -4.647995511668418727161722589023744577223260966848260747450320e-02,
        -6.643878569502520527899215536971203191819566896079739622858574e-02,
        3.133509021904607603094798408303144536358105680880031964936445e-02,
        2.084090436018106302294811255656491015157761832734715691126692e-02,
        -1.536482090620159942619811609958822744014326495773000120205848e-02,
        -3.340858873014445606090808617982406101930658359499190845656731e-03,
        4.928417656059041123170739741708273690285547729915802418397458e-03,
        -3.085928588151431651754590726278953307180216605078488581921562e-04,
        -8.930232506662646133900824622648653989879519878620728793133358e-04,
        2.491525235528234988712216872666801088221199302855425381971392e-04,
        5.443907469936847167357856879576832191936678525600793978043688e-05,
        -3.463498418698499554128085159974043214506488048233458035943601e-05,
        4.494274277236510095415648282310130916410497987383753460571741e-06]

db12 :: Coeffs
db12 = [1.311225795722951750674609088893328065665510641931325007748280e-02,
        1.095662728211851546057045050248905426075680503066774046383657e-01,
        3.773551352142126570928212604879206149010941706057526334705839e-01,
        6.571987225793070893027611286641169834250203289988412141394281e-01,
        5.158864784278156087560326480543032700677693087036090056127647e-01,
        -4.476388565377462666762747311540166529284543631505924139071704e-02,
        -3.161784537527855368648029353478031098508839032547364389574203e-01,
        -2.377925725606972768399754609133225784553366558331741152482612e-02,
        1.824786059275796798540436116189241710294771448096302698329011e-01,
        5.359569674352150328276276729768332288862665184192705821636342e-03,
        -9.643212009650708202650320534322484127430880143045220514346402e-02,
        1.084913025582218438089010237748152188661630567603334659322512e-02,
        4.154627749508444073927094681906574864513532221388374861287078e-02,
        -1.221864906974828071998798266471567712982466093116558175344811e-02,
        -1.284082519830068329466034471894728496206109832314097633275225e-02,
        6.711499008795509177767027068215672450648112185856456740379455e-03,
        2.248607240995237599950865211267234018343199786146177099262010e-03,
        -2.179503618627760471598903379584171187840075291860571264980942e-03,
        6.545128212509595566500430399327110729111770568897356630714552e-06,
        3.886530628209314435897288837795981791917488573420177523436096e-04,
        -8.850410920820432420821645961553726598738322151471932808015443e-05,
        -2.424154575703078402978915320531719580423778362664282239377532e-05,
        1.277695221937976658714046362616620887375960941439428756055353e-05,
        -1.529071758068510902712239164522901223197615439660340672602696e-06]

db13 :: Coeffs
db13 = [9.202133538962367972970163475644184667534171916416562386009703e-03,
        8.286124387290277964432027131230466405208113332890135072514277e-02,
        3.119963221604380633960784112214049693946683528967180317160390e-01,
        6.110558511587876528211995136744180562073612676018239438526582e-01,
        5.888895704312189080710395347395333927665986382812836042235573e-01,
        8.698572617964723731023739838087494399231884076619701250882016e-02,
        -3.149729077113886329981698255932282582876888450678789025950306e-01,
        -1.245767307508152589413808336021260180792739295173634719572069e-01,
        1.794760794293398432348450072339369013581966256244133393042881e-01,
        7.294893365677716380902830610477661983325929026879873553627963e-02,
        -1.058076181879343264509667304196464849478860754801236658232360e-01,
        -2.648840647534369463963912248034785726419604844297697016264224e-02,
        5.613947710028342886214501998387331119988378792543100244737056e-02,
        2.379972254059078811465170958554208358094394612051934868475139e-03,
        -2.383142071032364903206403067757739134252922717636226274077298e-02,
        3.923941448797416243316370220815526558824746623451404043918407e-03,
        7.255589401617566194518393300502698898973529679646683695269828e-03,
        -2.761911234656862178014576266098445995350093330501818024966316e-03,
        -1.315673911892298936613835370593643376060412592653652307238124e-03,
        9.323261308672633862226517802548514100918088299801952307991569e-04,
        4.925152512628946192140957387866596210103778299388823500840094e-05,
        -1.651289885565054894616687709238000755898548214659776703347801e-04,
        3.067853757932549346649483228575476236600428217237900563128230e-05,
        1.044193057140813708170714991080596951670706436217328169641474e-05,
        -4.700416479360868325650195165061771321650383582970958556568059e-06,
        5.220035098454864691736424354843176976747052155243557001531901e-07]

db14 :: Coeffs
db14 = [6.461153460087947818166397448622814272327159419201199218101404e-03,
        6.236475884939889832798566758434877428305333693407667164602518e-02,
        2.548502677926213536659077886778286686187042416367137443780084e-01,
        5.543056179408938359926831449851154844078269830951634609683997e-01,
        6.311878491048567795576617135358172348623952456570017289788809e-01,
        2.186706877589065214917475918217517051765774321270432059030273e-01,
        -2.716885522787480414142192476181171094604882465683330814311896e-01,
        -2.180335299932760447555558812702311911975240669470604752747127e-01,
        1.383952138648065910739939690021573713989900463229686119059119e-01,
        1.399890165844607012492943162271163440328221555614326181333683e-01,
        -8.674841156816968904560822066727795382979149539517503657492964e-02,
        -7.154895550404613073584145115173807990958069673129538099990913e-02,
        5.523712625921604411618834060533403397913833632511672157671107e-02,
        2.698140830791291697399031403215193343375766595807274233284349e-02,
        -3.018535154039063518714822623489137573781575406658652624883756e-02,
        -5.615049530356959133218371367691498637457297203925810387698680e-03,
        1.278949326633340896157330705784079299374903861572058313481534e-02,
        -7.462189892683849371817160739181780971958187988813302900435487e-04,
        -3.849638868022187445786349316095551774096818508285700493058915e-03,
        1.061691085606761843032566749388411173033941582147830863893939e-03,
        7.080211542355278586442977697617128983471863464181595371670094e-04,
        -3.868319473129544821076663398057314427328902107842165379901468e-04,
        -4.177724577037259735267979539839258928389726590132730131054323e-05,
        6.875504252697509603873437021628031601890370687651875279882727e-05,
        -1.033720918457077394661407342594814586269272509490744850691443e-05,
        -4.389704901781394115254042561367169829323085360800825718151049e-06,
        1.724994675367812769885712692741798523587894709867356576910717e-06,
        -1.787139968311359076334192938470839343882990309976959446994022e-07]

db15 :: Coeffs
db15 = [4.538537361578898881459394910211696346663671243788786997916513e-03,
        4.674339489276627189170969334843575776579151700214943513113197e-02,
        2.060238639869957315398915009476307219306138505641930902702047e-01,
        4.926317717081396236067757074029946372617221565130932402160160e-01,
        6.458131403574243581764209120106917996432608287494046181071489e-01,
        3.390025354547315276912641143835773918756769491793554669336690e-01,
        -1.932041396091454287063990534321471746304090039142863827937754e-01,
        -2.888825965669656462484125009822332981311435630435342594971292e-01,
        6.528295284877281692283107919869574882039174285596144125965101e-02,
        1.901467140071229823484893116586020517959501258174336696878156e-01,
        -3.966617655579094448384366751896200668381742820683736805449745e-02,
        -1.111209360372316933656710324674058608858623762165914120505657e-01,
        3.387714392350768620854817844433523770864744687411265369463195e-02,
        5.478055058450761268913790312581879108609415997422768564244845e-02,
        -2.576700732843996258594525754269826392203641634825340138396836e-02,
        -2.081005016969308167788483424677000162054657951364899040996166e-02,
        1.508391802783590236329274460170322736244892823305627716233968e-02,
        5.101000360407543169708860185565314724801066527344222055526631e-03,
        -6.487734560315744995181683149218690816955845639388826407928967e-03,
        -2.417564907616242811667225326300179605229946995814535223329411e-04,
        1.943323980382211541764912332541087441011424865579531401452302e-03,
        -3.734823541376169920098094213645414611387630968030256625740226e-04,
        -3.595652443624688121649620075909808858194202454084090305627480e-04,
        1.558964899205997479471658241227108816255567059625495915228603e-04,
        2.579269915531893680925862417616855912944042368767340709160119e-05,
        -2.813329626604781364755324777078478665791443876293788904267255e-05,
        3.362987181737579803124845210420177472134846655864078187186304e-06,
        1.811270407940577083768510912285841160577085925337507850590290e-06,
        -6.316882325881664421201597299517657654166137915121195510416641e-07,
        6.133359913305752029056299460289788601989190450885396512173845e-08]

db16 :: Coeffs
db16 = [3.189220925347738029769547564645958687067086750131428767875878e-03,
        3.490771432367334641030147224023020009218241430503984146140054e-02,
        1.650642834888531178991252730561134811584835002342723240213592e-01,
        4.303127228460038137403925424357684620633970478036986773924646e-01,
        6.373563320837888986319852412996030536498595940814198125967751e-01,
        4.402902568863569000390869163571679288527803035135272578789884e-01,
        -8.975108940248964285718718077442597430659247445582660149624718e-02,
        -3.270633105279177046462905675689119641757228918228812428141723e-01,
        -2.791820813302827668264519595026873204339971219174736041535479e-02,
        2.111906939471042887209680163268837900928491426167679439251042e-01,
        2.734026375271604136485245757201617965429027819507130220231500e-02,
        -1.323883055638103904500474147756493375092287817706027978798549e-01,
        -6.239722752474871765674503394120025865444656311678760990761458e-03,
        7.592423604427631582148498743941422461530405946100943351940313e-02,
        -7.588974368857737638494890864636995796586975144990925400097160e-03,
        -3.688839769173014233352666320894554314718748429706730831064068e-02,
        1.029765964095596941165000580076616900528856265803662208854147e-02,
        1.399376885982873102950451873670329726409840291727868988490100e-02,
        -6.990014563413916670284249536517288338057856199646469078115759e-03,
        -3.644279621498389932169000540933629387055333973353108668841215e-03,
        3.128023381206268831661202559854678767821471906193608117450360e-03,
        4.078969808497128362417470323406095782431952972310546715071397e-04,
        -9.410217493595675889266453953635875407754747216734480509250273e-04,
         1.142415200387223926440228099555662945839684344936472652877091e-04,
         1.747872452253381803801758637660746874986024728615399897971953e-04,
        -6.103596621410935835162369150522212811957259981965919143961722e-05,
        -1.394566898820889345199078311998401982325273569198675335408707e-05,
         1.133660866127625858758848762886536997519471068203753661757843e-05,
        -1.043571342311606501525454737262615404887478930635676471546032e-06,
        -7.363656785451205512099695719725563646585445545841663327433569e-07,
         2.308784086857545866405412732942006121306306735866655525372544e-07,
        -2.109339630100743097000572623603489906836297584591605307745349e-08]

db17 :: Coeffs
db17 = [2.241807001037312853535962677074436914062191880560370733250531e-03,
        2.598539370360604338914864591720788315473944524878241294399948e-02,
        1.312149033078244065775506231859069960144293609259978530067004e-01,
        3.703507241526411504492548190721886449477078876896803823650425e-01,
        6.109966156846228181886678867679372082737093893358726291371783e-01,
        5.183157640569378393254538528085968046216817197718416402439904e-01,
        2.731497040329363500431250719147586480350469818964563003672942e-02,
        -3.283207483639617360909665340725061767581597698151558024679130e-01,
        -1.265997522158827028744679110933825505053966260104086162103728e-01,
        1.973105895650109927854047044781930142551422414135646917122284e-01,
        1.011354891774702721509699856433434802196622545499664876109437e-01,
        -1.268156917782863110948571128662331680384792185915017065732137e-01,
        -5.709141963167692728911239478651382324161160869845347053990144e-02,
        8.110598665416088507965885748555429201024364190954499194020678e-02,
        2.231233617810379595339136059534813756232242114093689244020869e-02,
        -4.692243838926973733300897059211400507138768125498030602878439e-02,
        -3.270955535819293781655360222177494452069525958061609392809275e-03,
        2.273367658394627031845616244788448969906713741338339498024864e-02,
        -3.042989981354637068592482637907206078633395457225096588287881e-03,
        -8.602921520322854831713706413243659917926736284271730611920986e-03,
        2.967996691526094872806485060008038269959463846548378995044195e-03,
        2.301205242153545624302059869038423604241976680189447476064764e-03,
        -1.436845304802976126222890402980384903503674530729935809561434e-03,
        -3.281325194098379713954444017520115075812402442728749700195651e-04,
        4.394654277686436778385677527317841632289249319738892179465910e-04,
        -2.561010956654845882729891210949920221664082061531909655178413e-05,
        -8.204803202453391839095482576282189866136273049636764338689593e-05,
        2.318681379874595084482068205706277572106695174091895338530734e-05,
        6.990600985076751273204549700855378627762758585902057964027481e-06,
        -4.505942477222988194102268206378312129713572600716499944918416e-06,
        3.016549609994557415605207594879939763476168705217646897702706e-07,
        2.957700933316856754979905258816151367870345628924317307354639e-07,
        -8.423948446002680178787071296922877068410310942222799622593133e-08,
        7.267492968561608110879767441409035034158581719789791088892046e-09]

db18 :: Coeffs
db18 = [1.576310218440760431540744929939777747670753710991660363684429e-03,
        1.928853172414637705921391715829052419954667025288497572236714e-02,
        1.035884658224235962241910491937253596470696555220241672976224e-01,
        3.146789413370316990571998255652579931786706190489374509491307e-01,
        5.718268077666072234818589370900623419393673743130930561295324e-01,
        5.718016548886513352891119994065965025668047882818525060759395e-01,
        1.472231119699281415750977271081072312557864107355701387801677e-01,
        -2.936540407365587442479030994981150723935710729035053239661752e-01,
        -2.164809340051429711237678625668271471437937235669492408388692e-01,
        1.495339755653777893509301738913667208804816691893765610261943e-01,
        1.670813127632574045149318139950134745324205646353988083152250e-01,
        -9.233188415084628060429372558659459731431848000144569612074508e-02,
        -1.067522466598284855932200581614984861385266404624112083917702e-01,
        6.488721621190544281947577955141911463129382116634147846137149e-02,
        5.705124773853688412090768846499622260596226120431038524600676e-02,
        -4.452614190298232471556143559744653492971477891439833592755034e-02,
        -2.373321039586000103275209582665216110197519330713490233071565e-02,
        2.667070592647059029987908631672020343207895999936072813363471e-02,
        6.262167954305707485236093144497882501990325204745013190268052e-03,
        -1.305148094661200177277636447600807169755191054507571666606133e-02,
        1.186300338581174657301741592161819084544899417452317405185615e-04,
        4.943343605466738130665529516802974834299638313366477765295203e-03,
        -1.118732666992497072800658855238650182318060482584970145512687e-03,
        -1.340596298336106629517567228251583609823044524685986640323942e-03,
        6.284656829651457125619449885420838217551022796301582874349652e-04,
        2.135815619103406884039052814341926025873200325996466522543440e-04,
        -1.986485523117479485798245416362489554927797880264017876139605e-04,
        -1.535917123534724675069770335876717193700472427021513236587288e-07,
        3.741237880740038181092208138035393952304292615793985030731363e-05,
        -8.520602537446695203919254911655523022437596956226376512305917e-06,
        -3.332634478885821888782452033341036827311505907796498439829337e-06,
        1.768712983627615455876328730755375176412501359114058815453100e-06,
        -7.691632689885176146000152878539598405817397588156525116769908e-08,
        -1.176098767028231698450982356561292561347579777695396953528141e-07,
        3.068835863045174800935478294933975372450179787894574492930570e-08,
        -2.507934454948598267195173183147126731806317144868275819941403e-09]

db19 :: Coeffs
db19 = [1.108669763181710571099154195209715164245299677773435932135455e-03,
        1.428109845076439737439889152950199234745663442163665957870715e-02,
        8.127811326545955065296306784901624839844979971028620366497726e-02,
        2.643884317408967846748100380289426873862377807211920718417385e-01,
        5.244363774646549153360575975484064626044633641048072116393160e-01,
        6.017045491275378948867077135921802620536565639585963293313931e-01,
        2.608949526510388292872456675310528324172673101301907739925213e-01,
        -2.280913942154826463746325776054637207093787237086425909534822e-01,
        -2.858386317558262418545975695028984237217356095588335149922119e-01,
        7.465226970810326636763433111878819005865866149731909656365399e-02,
        2.123497433062784888090608567059824197077074200878839448416908e-01,
        -3.351854190230287868169388418785731506977845075238966819814032e-02,
        -1.427856950387365749779602731626112812998497706152428508627562e-01,
        2.758435062562866875014743520162198655374474596963423080762818e-02,
        8.690675555581223248847645428808443034785208002468192759640352e-02,
        -2.650123625012304089901835843676387361075068017686747808171345e-02,
        -4.567422627723090805645444214295796017938935732115630050880109e-02,
        2.162376740958504713032984257172372354318097067858752542571020e-02,
        1.937554988917612764637094354457999814496885095875825546406963e-02,
        -1.398838867853514163250401235248662521916813867453095836808366e-02,
        -5.866922281012174726584493436054373773814608340808758177372765e-03,
        7.040747367105243153014511207400620109401689897665383078229398e-03,
        7.689543592575483559749139148673955163477947086039406129546422e-04,
        -2.687551800701582003957363855070398636534038920982478290170267e-03,
        3.418086534585957765651657290463808135214214848819517257794031e-04,
        7.358025205054352070260481905397281875183175792779904858189494e-04,
        -2.606761356786280057318315130897522790383939362073563408613547e-04,
        -1.246007917341587753449784408901653990317341413341980904757592e-04,
        8.711270467219922965416862388191128268412933893282083517729443e-05,
        5.105950487073886053049222809934231573687367992106282669389264e-06,
        -1.664017629715494454620677719899198630333675608812018108739144e-05,
        3.010964316296526339695334454725943632645798938162427168851382e-06,
        1.531931476691193063931832381086636031203123032723477463624141e-06,
        -6.862755657769142701883554613486732854452740752771392411758418e-07,
        1.447088298797844542078219863291615420551673574071367834316167e-08,
        4.636937775782604223430857728210948898871748291085962296649320e-08,
        -1.116402067035825816390504769142472586464975799284473682246076e-08,
        8.666848838997619350323013540782124627289742190273059319122840e-10]

db20 :: Coeffs
db20 = [7.799536136668463215861994818889370970510722039232863880031127e-04,
        1.054939462495039832454480973015641498231961468733236691299796e-02,
        6.342378045908151497587346582668785136406523315729666353643372e-02,
        2.199421135513970450080335972537209392121306761010882209298252e-01,
        4.726961853109016963710241465101446230757804141171727845834637e-01,
        6.104932389385938201631515660084201906858628924695448898824748e-01,
        3.615022987393310629195602665268631744967084723079677894136358e-01,
        -1.392120880114838725806970545155530518264944915437808314813582e-01,
        -3.267868004340349674031122837905370666716645587480021744425550e-01,
        -1.672708830907700757517174997304297054003744303620479394006890e-02,
        2.282910508199163229728429126648223086437547237250290835639880e-01,
        3.985024645777120219790581076522174181104027576954427684456660e-02,
        -1.554587507072679559315307870562464374359996091752285157077477e-01,
        -2.471682733861358401587992299169922262915151413349313513685587e-02,
        1.022917191744425578861013681016866083888381385233081516583444e-01,
        5.632246857307435506953246988215209861566800664402785938591145e-03,
        -6.172289962468045973318658334083283558209278762007041823250642e-02,
        5.874681811811826491300679742081997167209743446956901841959711e-03,
        3.229429953076958175885440860617219117564558605035979601073235e-02,
        -8.789324923901561348753650366700695916503030939283830968151332e-03,
        -1.381052613715192007819606423860356590496904285724730356602106e-02,
        6.721627302259456835336850521405425560520025237915708362002910e-03,
        4.420542387045790963058229526673514088808999478115581153468068e-03,
        -3.581494259609622777556169638358238375765194248623891034940330e-03,
        -8.315621728225569192482585199373230956924484221135739973390038e-04,
        1.392559619323136323905254999347967283760544147397530531142397e-03,
        -5.349759843997695051759716377213680036185796059087353172073952e-05,
        -3.851047486992176060650288501475716463266233035937022303649838e-04,
        1.015328897367029050797488785306056522529979267572003990901472e-04,
        6.774280828377729558011184406727978221295796652200819839464354e-05,
        -3.710586183394712864227221271216408416958225264980612822617745e-05,
        -4.376143862183996810373095822528607606900620592585762190542483e-06,
        7.241248287673620102843105877497181565468725757387007139555885e-06,
        -1.011994010018886150340475413756849103197395069431085005709201e-06,
        -6.847079597000556894163334787575159759109091330092963990364192e-07,
        2.633924226270001084129057791994367121555769686616747162262697e-07,
        2.014322023550512694324757845944026047904414136633776958392681e-10,
        -1.814843248299695973210605258227024081458531110762083371310917e-08,
        4.056127055551832766099146230616888024627380574113178257963252e-09,
        -2.998836489619319566407767078372705385732460052685621923178375e-10]
