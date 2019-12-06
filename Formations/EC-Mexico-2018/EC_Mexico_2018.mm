<map version="freeplane 1.6.0">
<!--To view this file, download free mind mapping software Freeplane from http://freeplane.sourceforge.net -->
<node TEXT="Ecole Chercheurs Mexico 2018" FOLDED="false" ID="ID_1914805481" CREATED="1522054748113" MODIFIED="1524513491278" STYLE="oval">
<font SIZE="18"/>
<hook NAME="MapStyle">
    <properties fit_to_viewport="false" edgeColorConfiguration="#808080ff,#ff0000ff,#0000ffff,#00ff00ff,#ff00ffff,#00ffffff,#7c0000ff,#00007cff,#007c00ff,#7c007cff,#007c7cff,#7c7c00ff"/>

<map_styles>
<stylenode LOCALIZED_TEXT="styles.root_node" STYLE="oval" UNIFORM_SHAPE="true" VGAP_QUANTITY="24.0 pt">
<font SIZE="24"/>
<stylenode LOCALIZED_TEXT="styles.predefined" POSITION="right" STYLE="bubble">
<stylenode LOCALIZED_TEXT="default" ICON_SIZE="12.0 pt" COLOR="#000000" STYLE="fork">
<font NAME="SansSerif" SIZE="10" BOLD="false" ITALIC="false"/>
</stylenode>
<stylenode LOCALIZED_TEXT="defaultstyle.details"/>
<stylenode LOCALIZED_TEXT="defaultstyle.attributes">
<font SIZE="9"/>
</stylenode>
<stylenode LOCALIZED_TEXT="defaultstyle.note" COLOR="#000000" BACKGROUND_COLOR="#ffffff" TEXT_ALIGN="LEFT"/>
<stylenode LOCALIZED_TEXT="defaultstyle.floating">
<edge STYLE="hide_edge"/>
<cloud COLOR="#f0f0f0" SHAPE="ROUND_RECT"/>
</stylenode>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.user-defined" POSITION="right" STYLE="bubble">
<stylenode LOCALIZED_TEXT="styles.topic" COLOR="#18898b" STYLE="fork">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.subtopic" COLOR="#cc3300" STYLE="fork">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.subsubtopic" COLOR="#669900">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.important">
<icon BUILTIN="yes"/>
</stylenode>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.AutomaticLayout" POSITION="right" STYLE="bubble">
<stylenode LOCALIZED_TEXT="AutomaticLayout.level.root" COLOR="#000000" STYLE="oval" SHAPE_HORIZONTAL_MARGIN="10.0 pt" SHAPE_VERTICAL_MARGIN="10.0 pt">
<font SIZE="18"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,1" COLOR="#0033ff">
<font SIZE="16"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,2" COLOR="#00b439">
<font SIZE="14"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,3" COLOR="#990000">
<font SIZE="12"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,4" COLOR="#111111">
<font SIZE="10"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,5"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,6"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,7"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,8"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,9"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,10"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,11"/>
</stylenode>
</stylenode>
</map_styles>
</hook>
<hook NAME="AutomaticEdgeColor" COUNTER="13" RULE="ON_BRANCH_CREATION"/>
<node TEXT="Travaux pratiques" POSITION="left" ID="ID_1164242356" CREATED="1522142336832" MODIFIED="1524513539921">
<edge COLOR="#7c007c"/>
<cloud COLOR="#ffffcc" SHAPE="ARC"/>
<node TEXT="TP0 : mise en place et planif d&apos;exp&#xe9;riences" ID="ID_862471735" CREATED="1522078614187" MODIFIED="1524072922752" LINK="TP/TP0_Mise-en-place/TP0_corrig&#xe9;.pdf"/>
<node TEXT="TP1 : analyse de sensibilit&#xe9; globale" FOLDED="true" ID="ID_1182813233" CREATED="1522132175521" MODIFIED="1524072955167" LINK="TP/TP1_ASGlobale/TP1_ASGlobale-consignes.odt">
<node TEXT="Morris" ID="ID_1191284010" CREATED="1522132494266" MODIFIED="1522133114903">
<node TEXT="Observations de l&apos;effet des param&#xe8;tres de Morris" ID="ID_332740587" CREATED="1522133115946" MODIFIED="1522136235108">
<node TEXT="K = 2 param&#xe8;tres/dimensions" ID="ID_1106833600" CREATED="1522134001148" MODIFIED="1522134019429">
<node TEXT="nb_r = nb trajectoires effectives" ID="ID_82221621" CREATED="1522133126410" MODIFIED="1522133156701"/>
<node TEXT="nb_l = nb de facteurs (d&#xe9;coupage de la grille)" ID="ID_434060059" CREATED="1522133157776" MODIFIED="1522134289841"/>
<node TEXT="delta = taille du saut" ID="ID_454604730" CREATED="1522133173974" MODIFIED="1522133238588"/>
</node>
<node TEXT="K &gt; 2 dimensions" FOLDED="true" ID="ID_1375379839" CREATED="1522134034557" MODIFIED="1522134044429">
<node TEXT="importance de la taille du saut : si trop grand, explore surtout les coins" ID="ID_1460465585" CREATED="1522134046359" MODIFIED="1522134072794"/>
</node>
</node>
<node TEXT="Mod&#xe8;le Volcan" ID="ID_1261589528" CREATED="1522134993608" MODIFIED="1522134998291">
<node TEXT="px = choix d&apos;une station de mesure" ID="ID_530339285" CREATED="1522135000723" MODIFIED="1522135804559"/>
<node TEXT="on voit que la sensibilit&#xe9; change selon la position de la station" ID="ID_1857165438" CREATED="1522135807552" MODIFIED="1522135835458"/>
<node TEXT="mu = effet individuel moyen des sauts, mu* = effet moyen des sauts en valeur absolue" ID="ID_1569981219" CREATED="1522136135823" MODIFIED="1522136197019"/>
</node>
<node TEXT="FAST" ID="ID_207640550" CREATED="1522137244117" MODIFIED="1522137254305">
<node TEXT="n = taille de l&apos;&#xe9;chantillon (discr&#xe9;tisation de l&apos;espace)" ID="ID_1890965428" CREATED="1522137577216" MODIFIED="1522137606841"/>
<node TEXT="M = nombre d&apos;harmoniques" ID="ID_367117249" CREATED="1522137607429" MODIFIED="1522137617580"/>
</node>
</node>
</node>
<node TEXT="TP2 : m&#xe9;tamod&#xe9;lisation/krigeage" ID="ID_772144192" CREATED="1522153898736" MODIFIED="1524073322425" LINK="TP/TP2_Metamodele-krigeage/mexico_tp2_corrige.pdf"/>
<node TEXT="TP3 : m&#xe9;thode bay&#xe9;sienne" ID="ID_1282815038" CREATED="1524073253435" MODIFIED="1524513432334" LINK="TP/TP3_Bayesien/TP_Bayesien.pdf"/>
<node TEXT="TP4 : optimisation bas&#xe9;e sur m&#xe9;tamod&#xe8;le" ID="ID_1919145461" CREATED="1524073295356" MODIFIED="1524083252770" LINK="TP/TP4_Optimisation_metamodele/TP4_corrige.pdf"/>
<node TEXT="TP5 : optimisation sans m&#xe9;tamod&#xe8;le" ID="ID_940393211" CREATED="1524073337669" MODIFIED="1524083279971" LINK="TP/TP5_Optimisation-sans-metamodele/TensionTestCase/correction_tension_test.R"/>
</node>
<node TEXT="Expos&#xe9;s" POSITION="right" ID="ID_527792151" CREATED="1522160330255" MODIFIED="1524072691466">
<edge COLOR="#007c7c"/>
<cloud COLOR="#ffcccc" SHAPE="ARC"/>
<node TEXT="Introduction 1. Ecole Chercheurs (R. Faivre)" ID="ID_1195754433" CREATED="1522142055056" MODIFIED="1524513336764" LINK="Pl&#xe9;ni&#xe8;re/01_mexicoEC2018-IntroductionEC.pdf">
<node TEXT="Mod&#xe8;les AgroBioSciences :" ID="ID_224088314" CREATED="1522054942812" MODIFIED="1522055136631">
<node TEXT="Mod&#xe8;les structure-fonction" ID="ID_1471946891" CREATED="1522055099957" MODIFIED="1522055119519"/>
<node TEXT="Mod&#xe8;les &#xe9;pid&#xe9;miologiques (dyn pop &#xe0; l&apos;&#xe9;chelle spatiale)" ID="ID_1008887306" CREATED="1522055083089" MODIFIED="1522055099452"/>
<node TEXT="Mod&#xe8;les de cultures" ID="ID_1604126361" CREATED="1522055142523" MODIFIED="1522055158138"/>
</node>
<node TEXT="Nombreux processus, donc nbx param&#xe8;tres, processus dynamiques" ID="ID_1624954472" CREATED="1522055186052" MODIFIED="1522055246493"/>
<node TEXT="Grandes questions : estimation, qualit&#xe9; pr&#xe9;dictive, simplification, conception de pratiques..." ID="ID_1026663159" CREATED="1522055247045" MODIFIED="1522055282836"/>
<node TEXT="2 gds r&#xe9;seaux : MEXICO et Mascot-Num" ID="ID_175986425" CREATED="1522055287169" MODIFIED="1522056096059"/>
<node TEXT="57 participants (50% INRA de 10 d&#xe9;partements, 15 autres instituts, &#xe9;coles et univ.)" ID="ID_669408162" CREATED="1522056097301" MODIFIED="1522056350549"/>
</node>
<node TEXT="Introduction 2. Analyse de sensibilit&#xe9; et optimisation pour la gestion des p&#xea;ches (S. Lehuta)" ID="ID_1238208607" CREATED="1522059670816" MODIFIED="1524513339730" LINK="Pl&#xe9;ni&#xe8;re/02_mexicoEC2018-expose_Introductif.pdf"/>
<node TEXT="Introduction 3. Exploration de mod&#xe8;les et fonctions co&#xfb;teuses (N. Durrande)" FOLDED="true" ID="ID_1638883098" CREATED="1522066708603" MODIFIED="1524513346170" LINK="Pl&#xe9;ni&#xe8;re/03_mexicoEC2018-expose_Exploration.pdf">
<node TEXT="intro" ID="ID_618617412" CREATED="1522066888449" MODIFIED="1522066897093">
<node TEXT="pourquoi co&#xfb;teux" FOLDED="true" ID="ID_1826692861" CREATED="1522066900384" MODIFIED="1522066912181">
<node TEXT="quand besoin de bcp de donn&#xe9;es" ID="ID_1397210660" CREATED="1522066927026" MODIFIED="1522066935550"/>
<node TEXT="exp&#xe9;s en milieu r&#xe9;el" ID="ID_219830710" CREATED="1522066935934" MODIFIED="1522066956720"/>
<node TEXT="test destructifs" ID="ID_500245894" CREATED="1522066966169" MODIFIED="1522066974978"/>
<node TEXT="exp&#xe9;s num&#xe9;riques" ID="ID_1620911007" CREATED="1522067002783" MODIFIED="1522067009749"/>
</node>
<node TEXT="y=f(x) fonction co&#xfb;teuse &#xe0; &#xe9;valuer" FOLDED="true" ID="ID_240093507" CREATED="1522067134411" MODIFIED="1522067162843">
<node TEXT="nbx param&#xe8;tres d&apos;entr&#xe9;e" ID="ID_654823607" CREATED="1522067163375" MODIFIED="1522067179271"/>
<node TEXT="sortie de simulation est un scalaire" ID="ID_1271365634" CREATED="1522067179714" MODIFIED="1522067186665"/>
</node>
<node TEXT="si co&#xfb;teux" FOLDED="true" ID="ID_1357639555" CREATED="1522067223490" MODIFIED="1522067227965">
<node TEXT="" ID="ID_139656125" CREATED="1522067390274" MODIFIED="1522067390281">
<hook NAME="FirstGroupNode"/>
</node>
<node TEXT="repr&#xe9;senter la fonction exacte est impossible, seulement des points de la fonction" ID="ID_1749692767" CREATED="1522067231081" MODIFIED="1522067258122"/>
<node TEXT="calculer les int&#xe9;grales n&apos;est pas possible" ID="ID_727070177" CREATED="1522067259093" MODIFIED="1522067283145"/>
<node TEXT="propagation d&apos;incertitude est impossible" ID="ID_862032477" CREATED="1522067283846" MODIFIED="1522067301738"/>
<node TEXT="optimisation est difficile" ID="ID_1819777638" CREATED="1522067345664" MODIFIED="1522067352015"/>
<node TEXT="" ID="ID_1372763806" CREATED="1522067390268" MODIFIED="1522067464186">
<hook NAME="SummaryNode"/>
<hook NAME="AlwaysUnfoldedNode"/>
<node TEXT="car doit faire appel &#xe0; la fonction de nombreuses fois" ID="ID_47584306" CREATED="1522067390286" MODIFIED="1522067464181"/>
</node>
</node>
<node TEXT="principe de mod&#xe9;lisation statistique : utiliser donn&#xe9;es pour aider &#xe0; construire une approximation du mod&#xe8;le" FOLDED="true" ID="ID_1853439715" CREATED="1522067477280" MODIFIED="1522067567556">
<node TEXT="bien s&#xfb;r il y aura une diff&#xe9;rence entre le m&#xe9;tamod&#xe8;le et le mod&#xe8;le vrai" ID="ID_1688581475" CREATED="1522067527572" MODIFIED="1522067557244"/>
<node TEXT="mod&#xe8;le statistique permet de quantifier l&apos;erreur" ID="ID_1207178784" CREATED="1522067568292" MODIFIED="1522067591244"/>
</node>
</node>
<node TEXT="Plans d&apos;exp&#xe9;riences" FOLDED="true" ID="ID_805985679" CREATED="1522067924315" MODIFIED="1522067929298">
<node TEXT="design OAT (one at a time)" FOLDED="true" ID="ID_464250457" CREATED="1522067938595" MODIFIED="1522067964055">
<node TEXT="ttes variables fix&#xe9;es, puis on fait varier 1 variable &#xe0; la fois pour voir si influence" ID="ID_12119395" CREATED="1522067968706" MODIFIED="1522068000336"/>
<node TEXT="pros :" ID="ID_586155894" CREATED="1522068014456" MODIFIED="1522068130233">
<node TEXT="faible budget (d+1 observation)" ID="ID_654234025" CREATED="1522068097537" MODIFIED="1522068115643"/>
<node TEXT="faciles &#xe0; interpr&#xe9;ter" ID="ID_857360882" CREATED="1522068116393" MODIFIED="1522068126329"/>
</node>
<node TEXT="cons :" ID="ID_248930027" CREATED="1522068060465" MODIFIED="1522068158303">
<node TEXT="on voit seulement les effets lin&#xe9;aires" ID="ID_1797252851" CREATED="1522068134852" MODIFIED="1522068147393"/>
<node TEXT="&#xe7;a ne couvre pas tout l&apos;espace" ID="ID_853370803" CREATED="1522068153244" MODIFIED="1522068155619"/>
</node>
</node>
<node TEXT="design factoriel" ID="ID_1334195100" CREATED="1522068247990" MODIFIED="1522068253932">
<node TEXT="on teste toutes les combinaisons min/max (on peut aussi ajouter des valeurs interm&#xe9;diaires)" ID="ID_959058342" CREATED="1522068255806" MODIFIED="1522068347333"/>
<node TEXT="pros : faciles &#xe0; utiliser et impl&#xe9;menter" ID="ID_74222630" CREATED="1522068299384" MODIFIED="1522068359613">
<node TEXT="adapt&#xe9;s au continu et discret" ID="ID_1870186313" CREATED="1522068364806" MODIFIED="1522068375908"/>
<node TEXT="peuvent se combiner avec OAT" ID="ID_1783738023" CREATED="1522068376405" MODIFIED="1522068433844"/>
<node TEXT="se pr&#xea;tent surtout &#xe0; la regression lin&#xe9;aire" ID="ID_64232491" CREATED="1522068434416" MODIFIED="1522068454705"/>
</node>
<node TEXT="" ID="ID_1799170328" CREATED="1522068360057" MODIFIED="1522068360057"/>
<node TEXT="cons :" ID="ID_557549994" CREATED="1522068308116" MODIFIED="1522068311183">
<node TEXT="nb d&apos;&#xe9;valuations non flexible" ID="ID_590197762" CREATED="1522068457717" MODIFIED="1522068476988"/>
<node TEXT="ne remplissent pas bien l&apos;espace" ID="ID_846887179" CREATED="1522068469859" MODIFIED="1522068487348"/>
<node TEXT="des points peuvent se confondre quand projection" ID="ID_97912837" CREATED="1522068495845" MODIFIED="1522068515500"/>
</node>
</node>
</node>
<node TEXT="Space filling" FOLDED="true" ID="ID_1281525746" CREATED="1522068636258" MODIFIED="1522068672233">
<node TEXT="&#xe9;valuer si le set de points couvre bien l&apos;espace" ID="ID_766158906" CREATED="1522068672237" MODIFIED="1522068707035">
<node TEXT="mesurer distance entre points, et chercher &#xe0; minimiser la distance maximale, ou maximiser la distance minimale" ID="ID_1078571476" CREATED="1522068707040" MODIFIED="1522068844923"/>
<node TEXT="comparer la distribution &#xe0; une distribution uniforme" ID="ID_571709770" CREATED="1522068845675" MODIFIED="1522068865737">
<node TEXT="&quot;discr&#xe9;pence&quot; : mesure de non uniformit&#xe9;" ID="ID_1130745984" CREATED="1522068887180" MODIFIED="1522068905219"/>
</node>
</node>
</node>
<node TEXT="Hypercube Latin (LHD) : 1 point par ligne et par colonne" ID="ID_1057080714" CREATED="1522069072077" MODIFIED="1522069723118">
<node TEXT="s&apos;assurer qu&apos;on couvre bien l&apos;espace (pas diagonale par exemple)" ID="ID_1350186234" CREATED="1522069546568" MODIFIED="1522069567724"/>
<node TEXT="algorithme de Morris Mitchell" ID="ID_443049128" CREATED="1522069568445" MODIFIED="1522069693404"/>
</node>
<node TEXT="Suites &#xe0; faible dicr&#xe9;pance (LDS) : s&#xe9;quences d&#xe9;terministes..." FOLDED="true" ID="ID_573786364" CREATED="1522069694528" MODIFIED="1522403301690">
<node TEXT="S&#xe9;quence de Halton" ID="ID_603421444" CREATED="1522069708009" MODIFIED="1522069916930">
<node TEXT="meilleure discr&#xe9;pence que tirer au hasard dans la distrib uniforme" ID="ID_46939718" CREATED="1522069970584" MODIFIED="1522069994723"/>
</node>
</node>
<node TEXT="Tesselation centrale de de Voronoi, 2 algorithmes :" ID="ID_97106849" CREATED="1522070353449" MODIFIED="1522071029673">
<node TEXT="k-means" ID="ID_1246718606" CREATED="1522070878849" MODIFIED="1522070884155"/>
<node TEXT="Lloyd" ID="ID_1724955450" CREATED="1522071033663" MODIFIED="1522071037216"/>
</node>
</node>
<node TEXT="Introduction 4. Calibration de mod&#xe8;les (S. Mah&#xe9;vas)" ID="ID_1368391050" CREATED="1522160203036" MODIFIED="1524513350053" LINK="Pl&#xe9;ni&#xe8;re/06_mexicoEC2018-expose_IntroductionCalibration.pdf"/>
<node TEXT="Introduction 5. Optimisation algorithms for multiobjective, noisy, complex problems (J. Bect)" FOLDED="true" ID="ID_939633150" CREATED="1522393606469" MODIFIED="1524513355606" LINK="Pl&#xe9;ni&#xe8;re/12_mexicoEC2018-expose_Extensions.pdf">
<node TEXT="SUR (Stepwise Uncertainty Reduction)" FOLDED="true" ID="ID_1909584234" CREATED="1522395071917" MODIFIED="1522395088188">
<node TEXT="type EGO Expected Improvement" ID="ID_1387481112" CREATED="1522395090029" MODIFIED="1522395124921"/>
</node>
<node TEXT="avec contraintes : &quot;EFI&quot; (Expected Feasible Improvement) dans DiceOptim (R)" ID="ID_1949329889" CREATED="1522395923146" MODIFIED="1522397231514"/>
<node TEXT="multiobective (multicrit&#xe8;res) : EHVI (Expected HyperVolume Improvements) - &quot;EHI&quot; dans Gpareto (R)" ID="ID_267532628" CREATED="1522396807961" MODIFIED="1522397214300"/>
<node TEXT="stochastique" ID="ID_232987979" CREATED="1522398198763" MODIFIED="1522398205443">
<node TEXT="crit&#xe8;re AKG" ID="ID_1404304734" CREATED="1522398611043" MODIFIED="1522398790580"/>
</node>
</node>
</node>
<node TEXT="Cours" POSITION="right" ID="ID_1269525791" CREATED="1522142300988" MODIFIED="1522403069020">
<edge COLOR="#007c00"/>
<cloud COLOR="#ccff99" SHAPE="ARC"/>
<node TEXT="Cours 1. M&#xe9;thodes d&apos;analyse de sensibilit&#xe9; globale (R. Faivre)" ID="ID_246658956" CREATED="1522071053370" MODIFIED="1524072154183" LINK="Pl&#xe9;ni&#xe8;re/04_mexicoEC2018-Cours1_AnanlyseSensibilite.pdf"/>
<node TEXT="Cours 2. Processus Gaussiens (N. Durrande)" FOLDED="true" ID="ID_1886503164" CREATED="1522142215672" MODIFIED="1524072167946" LINK="Pl&#xe9;ni&#xe8;re/05_mexicoEC2018-Cours2_Metamodele.pdf">
<node TEXT="Comment b&#xe2;tir un mod&#xe8;le statistique ?" FOLDED="true" ID="ID_1093209529" CREATED="1522142887749" MODIFIED="1522142899255">
<node TEXT="Mod&#xe8;le lin&#xe9;aire" ID="ID_1731557885" CREATED="1522142900976" MODIFIED="1522142908710">
<node TEXT="H0 : notre fonction est une combinaison lin&#xe9;aire de fonctions de base + erreur" ID="ID_250082370" CREATED="1522142998364" MODIFIED="1522143075652"/>
<node TEXT="on peut &#xe9;valuer cette erreur (incertitude)" ID="ID_769503509" CREATED="1522143078331" MODIFIED="1522143371319"/>
<node TEXT="l&apos;incertitude se propage, on peut d&#xe9;terminer l&apos;incertitude sur les param&#xe8;tres (ex : incertitude sur le minimum d&apos;une fonction)" ID="ID_295217274" CREATED="1522143373067" MODIFIED="1522143409444"/>
<node TEXT="pros :" ID="ID_1170394772" CREATED="1522143637807" MODIFIED="1522143643117">
<node TEXT="bon filtrage ddu bruit" ID="ID_649943081" CREATED="1522143656331" MODIFIED="1522143666178"/>
<node TEXT="faciles &#xe0; interpr&#xe9;ter" ID="ID_937729837" CREATED="1522143667045" MODIFIED="1522143675378"/>
</node>
<node TEXT="cons :" ID="ID_596678994" CREATED="1522143647138" MODIFIED="1522143651231">
<node TEXT="peu flexibles" ID="ID_1299639542" CREATED="1522143679856" MODIFIED="1522143684996"/>
<node TEXT="..." ID="ID_908870811" CREATED="1522143686465" MODIFIED="1522143687922"/>
<node TEXT="..." ID="ID_1889396717" CREATED="1522143688446" MODIFIED="1522143689645"/>
</node>
</node>
</node>
<node TEXT="Mod&#xe8;le Gaussien" ID="ID_699210737" CREATED="1522143702783" MODIFIED="1522143726460">
<node TEXT="Distribution Normale" FOLDED="true" ID="ID_251131099" CREATED="1522143731728" MODIFIED="1522143743418">
<node TEXT="2 param&#xe8;tres" ID="ID_444928164" CREATED="1522143818270" MODIFIED="1522143823289">
<node TEXT="moyenne : E[X]" ID="ID_1698191359" CREATED="1522143825414" MODIFIED="1522143860502"/>
<node TEXT="variance : E[X&#xb2;]-E[X]&#xb2;" ID="ID_989831487" CREATED="1522143829226" MODIFIED="1522143914720"/>
</node>
</node>
<node TEXT="distrib. normale multivari&#xe9;e" ID="ID_958676156" CREATED="1522143930210" MODIFIED="1522143945804">
<node TEXT="vecteur al&#xe9;atoire Y suit une loi normale si toute combinaison lin&#xe9;aire de Y suit loi normale" ID="ID_293079453" CREATED="1522143948450" MODIFIED="1522144034632"/>
</node>
</node>
</node>
<node TEXT="Cours 3. Approche Bay&#xe9;sienne (J. Papa&#xef;x)" ID="ID_124666855" CREATED="1522160234710" MODIFIED="1524072185578" LINK="Pl&#xe9;ni&#xe8;re/07_mexicoEC2018-Cours3_Bayesien.pdf"/>
<node TEXT="Cours 4. Optimisation bas&#xe9;e sur mod&#xe8;le (N. Durrande, V. Picheny)" FOLDED="true" ID="ID_1785063831" CREATED="1522305361223" MODIFIED="1524072219345" LINK="Pl&#xe9;ni&#xe8;re/10_mexicoEC2018-Cours4_OptimAvecMetamodel.pdf">
<node TEXT="sert &#xe0; :" FOLDED="true" ID="ID_905550435" CREATED="1522305546638" MODIFIED="1522305557246">
<node TEXT="aider &#xe0; la calibration (minimiser &#xe9;crat sorties mod&#xe8;le / donn&#xe9;es)" ID="ID_1227248324" CREATED="1522305557258" MODIFIED="1522305622205"/>
<node TEXT="optimiser le mod&#xe8;le pour une r&#xe9;ponse en particulier (optimiser une performance, minimiser un co&#xfb;t)" ID="ID_1591549734" CREATED="1522305647248" MODIFIED="1522305695009"/>
</node>
<node TEXT="diff&#xe9;rents algorithmes selon type de probl&#xe8;me" ID="ID_269156501" CREATED="1522305773073" MODIFIED="1522305843731"/>
<node TEXT="compromis exploration/exploitation" ID="ID_1680622478" CREATED="1522305844825" MODIFIED="1522309228265">
<node TEXT="optimisation" ID="ID_173405410" CREATED="1522305973493" MODIFIED="1522306018416">
<node TEXT="locale" ID="ID_1773986670" CREATED="1522305998400" MODIFIED="1522306006510">
<node TEXT="&quot;Trust regions&quot; (m&#xe9;tamod&#xe8;les quadratiques, optimisation sans d&#xe9;riv&#xe9;es)" FOLDED="true" ID="ID_917355464" CREATED="1522306873073" MODIFIED="1522307656876">
<node TEXT="pros :" ID="ID_578862238" CREATED="1522307584986" MODIFIED="1522307593503">
<node TEXT="garantie de convergence" ID="ID_1756209234" CREATED="1522307593510" MODIFIED="1522307599169"/>
<node TEXT="methodes assez parcimonieuses" ID="ID_25292055" CREATED="1522307599395" MODIFIED="1522307607270"/>
<node TEXT="robuste" ID="ID_1748068898" CREATED="1522307608591" MODIFIED="1522307611320"/>
<node TEXT="accepte un tr&#xe8;s grand nb de variables" ID="ID_803658400" CREATED="1522307614433" MODIFIED="1522307729841"/>
</node>
</node>
</node>
<node TEXT="globale" ID="ID_339147135" CREATED="1522306007046" MODIFIED="1522306010239">
<node TEXT="algo &quot;DIRECT&quot; (DIviding RECTangle)" ID="ID_1773839996" CREATED="1522306032228" MODIFIED="1522306056930">
<node TEXT="d&#xe9;coupage en hyperrectangles" ID="ID_1133676457" CREATED="1522306063935" MODIFIED="1522306075799"/>
<node TEXT="1 &#xe9;chantillon au centre de chaque rectangle" ID="ID_870797861" CREATED="1522306076375" MODIFIED="1522306125375"/>
<node TEXT="se fait avec un m&#xe9;tamod&#xe8;le cach&#xe9; (constante de Lipschitz)" ID="ID_1748695606" CREATED="1522306457001" MODIFIED="1522306481225"/>
<node TEXT="pros : exploration de tout l&apos;espace, strat&#xe9;gie robuste" ID="ID_1123030219" CREATED="1522306390620" MODIFIED="1522306413007"/>
<node TEXT="cons : limit&#xe9; aux petites dimensions (&lt;5), exploitation limit&#xe9;e de l&apos;information" ID="ID_1490417013" CREATED="1522306413517" MODIFIED="1522306608378"/>
<node TEXT="il existe des algos Lipschitziens plus efficaces (dimensions 5-20)" ID="ID_1095222536" CREATED="1522306770974" MODIFIED="1522306851811"/>
</node>
<node TEXT="M&#xe9;thodes bas&#xe9;e sur krigeage" ID="ID_303257166" CREATED="1522307867762" MODIFIED="1522307887213">
<node TEXT="&quot;Efficient Global Optimization&quot;" ID="ID_449070226" CREATED="1522308708056" MODIFIED="1522308721218">
<node TEXT="bas&#xe9; sur l&apos;&quot;Expected Improvement&quot;" ID="ID_431150489" CREATED="1522308724703" MODIFIED="1522308750550"/>
<node TEXT="on cherche les zones de meilleur potentiel d&apos;am&#xe9;lioration (zones o&#xf9; l&apos;inrvalle de confiance est grand, et o&#xf9; sa borne inf&#xe9;rieure est inf&#xe9;rieure au minimum actuel)" ID="ID_2015963" CREATED="1522308939571" MODIFIED="1522309026091"/>
<node TEXT="pros" FOLDED="true" ID="ID_1289647463" CREATED="1522309310712" MODIFIED="1522309312564">
<node TEXT="fonctionne bien sur un nb de dimensions assez grandes (~30)" ID="ID_1494761134" CREATED="1522308878834" MODIFIED="1522308901285"/>
<node TEXT="bonne balance exploitation/exploration" ID="ID_299934132" CREATED="1522309237981" MODIFIED="1522309259513"/>
<node TEXT="n&#xe9;cessite peu de d&apos;observations pour trouver une valeur optimale satisfaisante" ID="ID_1858845293" CREATED="1522309270283" MODIFIED="1522309302507"/>
</node>
<node TEXT="cons" FOLDED="true" ID="ID_1568918058" CREATED="1522309561773" MODIFIED="1522309577033">
<node TEXT="limit&#xe9; par l&apos;utilisation de matrices donc n~1000 points" ID="ID_1511461111" CREATED="1522309577038" MODIFIED="1522309594546"/>
<node TEXT="si trop de param&#xe8;tres, n augmente (cf au dessus)" ID="ID_1966438444" CREATED="1522309627103" MODIFIED="1522309643795"/>
</node>
<node TEXT="package R &quot;DiceOptim&quot;" ID="ID_1281765415" CREATED="1522309754679" MODIFIED="1522309768643"/>
</node>
</node>
</node>
<node TEXT="&#xe0; budget fixe, on divise le budget en 2" FOLDED="true" ID="ID_1857604843" CREATED="1522307103069" MODIFIED="1522307118158">
<node TEXT="1. plan d&apos;exp&#xe9; initial, construction m&#xe9;tamod&#xe8;le" ID="ID_765956771" CREATED="1522307119838" MODIFIED="1522307169595"/>
<node TEXT="2. processus d&apos;optimisation avec donn&#xe9;es" ID="ID_1885178903" CREATED="1522307136785" MODIFIED="1522307174303"/>
<node TEXT="3. on fait des allers-retours 1/2" ID="ID_709587216" CREATED="1522307175178" MODIFIED="1522307187185"/>
</node>
</node>
</node>
</node>
<node TEXT="Cours 5. Optimisation sans mod&#xe8;le (R. Le Riche)" FOLDED="true" ID="ID_62233944" CREATED="1522326653168" MODIFIED="1524072232502" LINK="Pl&#xe9;ni&#xe8;re/11_mexicoEC2018-Cours5_OptimSansMetamodele.pdf">
<node TEXT="2 algos classiques, m&#xe9;thodes locales &#xe0; la base" ID="ID_1789046871" CREATED="1522327831641" MODIFIED="1522333486802">
<node TEXT="Nelder-Mead (d&#xe9;terministe, &quot;pattern-search&quot;)" ID="ID_892316128" CREATED="1522327842272" MODIFIED="1522328235125">
<node TEXT="un pattern est un simplex (figure g&#xe9;om&#xe9;trique) de dim+1 points" ID="ID_819069630" CREATED="1522328311000" MODIFIED="1522328340933"/>
</node>
<node TEXT="CMA-ES (stochastique)" ID="ID_1983825371" CREATED="1522327850316" MODIFIED="1522330307356"/>
</node>
</node>
</node>
<node TEXT="Exemples" POSITION="left" ID="ID_1742968084" CREATED="1522228667029" MODIFIED="1524072595184">
<edge COLOR="#ff0000"/>
<cloud COLOR="#ccccff" SHAPE="ARC"/>
<node TEXT="Calibration d&apos;un mod&#xe8;le colonisation bassin versant par anguilles (H. Drouineau)" FOLDED="true" ID="ID_1188406838" CREATED="1522228673143" MODIFIED="1524072619154" LINK="Pl&#xe9;ni&#xe8;re/08_mexicoEC2018-expose_CalibrationAnguille.pdf">
<node TEXT="long processus avec nombreux allers-retours" ID="ID_1677602190" CREATED="1522229064296" MODIFIED="1522229083025"/>
<node TEXT="connaissances" FOLDED="true" ID="ID_142325592" CREATED="1522229115450" MODIFIED="1522229118981">
<node TEXT="th&#xe9;matiques" ID="ID_727476230" CREATED="1522229122552" MODIFIED="1522229125351"/>
<node TEXT="informatiques" ID="ID_1782192927" CREATED="1522229125733" MODIFIED="1522229140497"/>
<node TEXT="statistiques" ID="ID_122878164" CREATED="1522229129079" MODIFIED="1522229134746"/>
<node TEXT="maths appli" ID="ID_1435369779" CREATED="1522229142381" MODIFIED="1522229149217"/>
</node>
<node TEXT="Mod&#xe8;le" ID="ID_1596297476" CREATED="1522229471962" MODIFIED="1522229478568">
<node TEXT="but : simuler mortalit&#xe9; par les barrages dans les BV de France" ID="ID_1708391983" CREATED="1522229480655" MODIFIED="1522229500246"/>
<node TEXT="1 BV est d&#xe9;coup&#xe9; en tron&#xe7;ons (confluents, barrages)" ID="ID_1344306210" CREATED="1522229561757" MODIFIED="1522229577487"/>
<node TEXT="matrice de probabilit&#xe9; pour 1 individu dans un tron&#xe7;on de passer dans un autre tron&#xe7;on" ID="ID_934498724" CREATED="1522229578033" MODIFIED="1522229630729"/>
<node TEXT="pram&#xe8;tres : lambda (franchissabilit&#xe9; barrage), b = pas de temps de la marche al&#xe9;atoire" ID="ID_743041385" CREATED="1522229685550" MODIFIED="1522229716008">
<node TEXT="b (pas de temps) : log(b)" ID="ID_30686800" CREATED="1522230277947" MODIFIED="1522230351393"/>
<node TEXT="N : nombre de poissons" ID="ID_867442985" CREATED="1522230352531" MODIFIED="1522230491931"/>
</node>
<node TEXT="calibration par vraissemblance (car comptages ont distrib tr&#xe8;s asymm&#xe9;trique, bcp de 0" FOLDED="true" ID="ID_409676186" CREATED="1522229909347" MODIFIED="1522230000052">
<node TEXT="1. loi berbouilli (proba de 0 poisson) * loi lognormale (probab d&apos;observer X poissons quand X &gt; 0)" ID="ID_1991163013" CREATED="1522230040058" MODIFIED="1522230148992"/>
<node TEXT="2. loi gamma, avec comptages de 0 poissons compris comme poisson &lt; 1" ID="ID_1353402672" CREATED="1522230149712" MODIFIED="1522230176863"/>
</node>
<node TEXT="exploration" ID="ID_1900300933" CREATED="1522231150550" MODIFIED="1522231155045">
<node TEXT="plan : LHS" ID="ID_1331687534" CREATED="1522231158107" MODIFIED="1522231185824"/>
<node TEXT="mod&#xe8;le : GAM" ID="ID_536238566" CREATED="1522231170866" MODIFIED="1522231197523"/>
</node>
</node>
</node>
<node TEXT="Ajustage de param&#xe8;tres par m&#xe9;thodes ABC (N. Dumoulin)" FOLDED="true" ID="ID_1895895794" CREATED="1522232049044" MODIFIED="1524083317448" LINK="TP/TP5_Optimisation-sans-metamodele/">
<node TEXT="ABC = Approximate Bayesian Computation" ID="ID_969833449" CREATED="1522232083117" MODIFIED="1522232105653"/>
<node TEXT="package R &quot;EasyABC&quot;" ID="ID_1215902812" CREATED="1522232778114" MODIFIED="1522233195548"/>
</node>
</node>
<node TEXT="SYNTHESE : Analyse de sensibilit&#xe9;-Calibration-Optimisation (S. Lehuta)" POSITION="right" ID="ID_1464826343" CREATED="1524072695286" MODIFIED="1524513456523" LINK="Pl&#xe9;ni&#xe8;re/13_mexicoEC2018-Synth&#xe8;seCalibration.pdf">
<edge COLOR="#0000ff"/>
<cloud COLOR="#00cccc" SHAPE="ARC"/>
</node>
</node>
</map>
