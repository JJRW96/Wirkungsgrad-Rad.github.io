library(flextable)
library(dplyr)
library(officer)
library(sysfonts)

Bedingungen_data <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/Bedingungen_data.rds")
Bedingungen_data_Wirkungsgrad <- Bedingungen_data[, c("Proband", "Bedingung", "Intensität", "Wirk_Arbeit", "W*Aerob [kJ]", "WPCR [kJ]", "WBLC [kJ]", "WTOT [kJ]", "P_Tot", "P_mean", "P_Int", "nD", "Efficiency", "Pedal_Smoothness", "P_max", "HR_percent", "VO2_percent", "ΔBLC", "Wirk_Brutto", "Wirk_Netto")]

Bedingungen_data_Wirkungsgrad <- data.frame(
  `Proband` = c( "01", "01", "01", "01", "01", "01", "06", "06", "06", "06", "06", "06", "10", "10", "10", "10", "10", "10", "13", "13", "13", "13", "13", "13", "15", "15", "15", "15", "15", "15", "19", "19", "19", "19", "19", "19", "20", "20", "20", "20", "20", "20", "22", "22", "22", "22", "22", "22", "23", "23", "23", "23", "23", "23" ),
  `Bedingung` = c( "stehen", "sitzen", "sitzen", "stehen", "sitzen", "stehen", "stehen", "sitzen", "stehen", "sitzen", "stehen", "sitzen", "stehen", "sitzen", "sitzen", "stehen", "stehen", "sitzen", "stehen", "sitzen", "stehen", "sitzen", "sitzen", "stehen", "sitzen", "stehen", "sitzen", "stehen", "stehen", "sitzen", "stehen", "sitzen", "sitzen", "stehen", "sitzen", "stehen", "sitzen", "stehen", "stehen", "sitzen", "stehen", "sitzen", "sitzen", "stehen", "stehen", "sitzen", "sitzen", "stehen", "stehen", "sitzen", "sitzen", "stehen", "stehen", "sitzen" ),
  `Intensität` = c( "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer" ),
  `Wirk_Arbeit` = c( 0.236250547795132, 0.245529098650419, 0.241831107677296, 0.229307515666647, 0.231507088513069, 0.239386095196768, 0.235209480714818, 0.24055808416089, 0.247798869251307, 0.238759086731862, 0.23972659365636, 0.242831865335772, 0.252738867726384, 0.268307083745623, 0.245575380774917, 0.242525260563538, 0.237204664691458, 0.239056380966622, 0.246387783480092, 0.259516457055451, 0.236834527573119, 0.255828091313744, 0.24544621333857, 0.235863469360983, 0.255688556079271, 0.229957470478675, 0.251425139029877, 0.237573562584614, 0.235979353885126, 0.256665842609051, 0.229307973380394, 0.250439260269428, 0.239350448318425, 0.225578656653151, 0.230147441297649, 0.224449528488117, 0.267107077596869, 0.254069380174965, 0.24738229674514, 0.249881793476201, 0.238797817270077, 0.241198753629408, 0.252529879479868, 0.255851733490532, 0.261857522259196, 0.257250278309675, 0.239867962297944, 0.254658649418519, 0.235815670762024, 0.26903145216167, 0.259746381446029, 0.236667084817931, 0.242627393366792, 0.258925399208512 ),
  `W*Aerob [kJ]` = c( 364.789166022847, 341.629555246705, 380.175743058369, 397.985468023398, 434.291677759482, 433.605222297912, 324.974079503018, 328.244436599334, 338.782659218172, 345.676143718918, 364.680735327353, 257.547695226202, 373.643424879889, 359.053884392895, 449.590938122183, 437.033618580678, 472.355442968317, 502.398965566933, 358.837718601139, 352.868283295468, 394.753364696823, 386.919677670582, 414.584226507652, 422.974893188083, 336.736950672026, 327.040591917684, 374.940893404024, 376.054742927363, 407.568603441812, 413.170309433097, 301.002193914676, 268.616569204091, 293.782454560316, 323.273033581013, 338.095668739063, 356.147551686419, 301.108945263692, 280.951141839367, 322.983175747043, 336.778695169108, 374.987772949063, 386.657728278586, 266.533422098459, 251.790208549518, 278.168675291213, 296.280182766526, 322.403130586883, 294.593727223427, 228.955584309471, 211.732892340367, 242.182720592561, 251.873990890611, 269.030850954527, 263.980675271676 ),
  `WPCR [kJ]` = c( 24.9900284064952, 32.0775895440158, 29.9795048488785, 41.3716744045004, 32.6377618655689, 31.5305083275764, 36.0432831412617, 30.1719416261326, 32.9214575444102, 37.8183920444932, 39.500568345183, 28.2799688316703, 26.3609109066173, 43.0042523637793, 44.8439130878096, 47.7346578702381, 57.0096876913651, 41.969244483047, 33.8847213912965, 39.9274127385049, 47.6475924267457, 43.9358795098138, 53.680045963072, 50.5645339017918, 29.2156670750502, 27.5302631965911, 35.1640900501547, 28.9862728227075, 47.231968373152, 36.7509956598521, 26.6060807868012, 20.3626011401956, 30.3924786102224, 26.0974083470203, 32.8155350808469, 35.504909488738, 26.5713982555822, 18.8188636747982, 26.1229675173029, 31.5492419387315, 24.5539903387425, 32.3962576908153, 18.1437132144592, 19.102596445564, 15.2871248704012, 14.8557160263649, 29.9503257704009, 33.8977953221, 19.6548373833257, 11.9046929838934, 16.781875869613, 20.6985356252627, 25.292253469314, 21.066925902858 ),
  `WBLC [kJ]` = c( 10.3313513320441, 11.610940774608, 15.6579076836, 12.815395211808, 20.331190899936, 7.660330220592, 10.5760425135007, 10.736126499168, 15.178661602272, 19.019603410164, 24.804154325664, 17.399929153824, 6.87435813197033, 5.614025524128, 18.401528106864, 10.604270434464, 15.802442216064, 23.443754735016, 7.48717405349485, 10.04134646592, 9.858776530176, 9.539279142624, 15.335874602496, 13.647102696864, 6.43215059991284, 8.575715592864, 8.575715592864, 11.80365348456, 7.46761751064, 17.874103848048, 6.89074621609046, 7.49931368004, 11.08415043918, 12.9795813693, 17.01767335086, 18.00659383614, 6.65505918050507, 10.142774208, 11.10633775776, 8.31707485056, 17.90199647712, 13.23632034144, 6.60952692345146, 6.0856645248, 10.862911176768, 13.38846195456, 20.295691190208, 15.275017957248, 4.76575332014558, 4.1838943608, 7.30279742976, 10.459735902, 13.38846195456, 11.52472719384 ),
  `WTOT [kJ]` = c( 400.110545761386, 385.318085565329, 425.813155590848, 452.172537639706, 487.260630524987, 472.79606084608, 371.59340515778, 369.152504724634, 386.882778364854, 402.514139173575, 428.9854579982, 303.227593211696, 406.878693918477, 407.672162280802, 512.836379316856, 495.37254688538, 545.167572875746, 567.811964784996, 400.209614045931, 402.837042499893, 452.259733653745, 440.39483632302, 483.60014707322, 487.186529786739, 372.384768346989, 363.146570707139, 418.680699047043, 416.84466923463, 462.268189325604, 467.795408940997, 334.499020917567, 296.478484024327, 335.259083609718, 362.350023297333, 387.92887717077, 409.659055011296, 334.335402699779, 309.912779722165, 360.212481022106, 376.645011958399, 417.443759764925, 432.290306310841, 291.286662236369, 276.978469519882, 304.318711338382, 324.524360747451, 372.649147547492, 343.766540502775, 253.376175012942, 227.82147968506, 266.267393891934, 283.032262417874, 307.711566378401, 296.572328368374 ),
  `P_Tot` = c( 315.087785382455, 315.356007475201, 343.249556933665, 345.621870862815, 376.01429973294, 377.269342767856, 291.340972880708, 296.008730999173, 319.563716705383, 320.346360885814, 342.797408580074, 334.696918549422, 342.780201343152, 364.604429952782, 419.799963753223, 400.467853364667, 431.054304415492, 452.463577903494, 328.689199107333, 348.476140134227, 357.035734534098, 375.551234669824, 395.659416230325, 383.031683804794, 317.381745748518, 278.360889376064, 350.889509890097, 330.104243714921, 363.61916212901, 400.223676014955, 255.677641947778, 247.499507416179, 267.481373216035, 272.461438312168, 297.602794954409, 306.49260579391, 297.677841174368, 262.464492841036, 297.033969571712, 313.722436973434, 332.282195549595, 347.559610294167, 245.195285695477, 236.218071887388, 265.6271457606, 278.279940401837, 297.955305580942, 291.810409732378, 199.166908885976, 204.303811710974, 230.539973534982, 223.28140151954, 248.864184197347, 255.967028389931 ),
  `P_mean` = c( 286.196809943666, 279.856121744775, 308.058185037385, 316.422860962335, 340.826183919876, 347.288435475183, 277.240209227733, 271.455328132766, 304.467347747788, 298.231190808954, 325.412992360824, 309.694617397985, 323.150737156769, 316.844325140231, 371.96441826512, 380.655666884463, 411.774528793807, 405.478497575611, 300.611680063499, 294.276254054958, 330.974610194196, 323.263418911815, 347.877528040632, 356.523758791993, 264.042555125073, 264.046097801302, 296.595675684983, 312.821367725728, 338.374812212103, 340.655624979066, 222.299144497093, 217.725388710054, 237.149862110928, 238.510476030676, 268.299540676442, 270.26756043723, 259.609504812067, 246.984706949108, 281.340838183762, 276.152650787485, 316.924535968906, 309.069638160985, 220.907452101768, 221.311649982903, 250.936106797992, 254.071699215572, 274.36729230484, 276.737918099592, 175.467181480606, 173.18153762583, 199.048429239061, 199.114857295449, 224.703097248925, 224.226593365666 ),
  `P_Int` = c( 28.8909754387892, 35.4998857304255, 35.1913718962795, 29.1990099004802, 35.1881158130648, 29.9809072926731, 14.100763652975, 24.5534028664068, 15.0963689575941, 22.1151700768599, 17.3844162192499, 25.0023011514367, 19.629464186383, 47.7601048125507, 47.8355454881021, 19.8121864802047, 19.2797756216845, 46.9850803278833, 28.0775190438347, 54.1998860792695, 26.061124339902, 52.2878157580086, 47.7818881896929, 26.5079250128008, 53.3391906234455, 14.3147915747616, 54.2938342051136, 17.2828759891928, 25.244349916907, 59.5680510358891, 33.3784974506853, 29.7741187061253, 30.3315111051071, 33.950962281492, 29.3032542779672, 36.2250453566804, 38.0683363623011, 15.4797858919276, 15.6931313879501, 37.5697861859489, 15.3576595806892, 38.4899721331819, 24.2878335937096, 14.9064219044849, 14.6910389626079, 24.2082411862652, 23.5880132761018, 15.0724916327862, 23.6997274053699, 31.1222740851432, 31.4915442959206, 24.1665442240911, 24.1610869484215, 31.7404350242649 ),
  `nD` = c( 76.6564376505317, 86.4007340902643, 86.151834718222, 76.9699534010204, 86.1492001342774, 77.7545030427186, 59.6696464126312, 79.340733311971, 59.0120747230942, 78.6315332132975, 58.5778147794659, 74.8376014062797, 64.4031353722102, 93.4309539387749, 93.4798136132308, 64.6513676939208, 63.9226666971842, 92.926017486389, 76.1787191980119, 100.725884150632, 76.4965310227163, 100.706804663599, 101.493737868195, 76.2895930915614, 94.9222147345463, 67.3025317640006, 95.6908060743521, 67.7563334376017, 68.9089393183102, 97.2326216948995, 82.3157999725342, 85.1151334584554, 84.9867423386037, 82.4121919818754, 85.6636667989095, 82.5434000701904, 88.3525337219238, 59.2843333028158, 59.6458305272363, 87.9682001902262, 59.0753999888102, 88.6749836398094, 86.1743334757487, 65.9431711942811, 65.5106667200724, 86.0812543343512, 85.3489333648682, 66.2728181161696, 75.2098000386556, 87.7106668141683, 88.0529021962314, 75.7959332733154, 75.7891333821615, 88.2820788795832 ),
  `Efficiency` = c( 66.42, 80.67, 85.6, 72.95, 87.65, 78.67, 80.04, 86.41, 86.78, 88.63, 88.24, 92.41, 73.99, 73.77, 81.11, 85.39, 91.79, 84.47, 72.05, 72.34, 79.17, 76.95, 79.78, 85.17, 66.52, 69.93, 72.41, 82.58, 87.01, 79.53, 65.43, 72.54, 77.02, 70.47, 80.77, 78.76, 64.47, 62.05, 68.4, 71.35, 77.44, 75.1, 81.99, 82.22, 91.47, 89.76, 92.56, 97.11, 50.58, 59.59, 65.12, 56.31, 63.19, 72.06 ),
  `Pedal_Smoothness` = c( 39.710181579574, 50.8184514296743, 52.4428359792181, 40.2055948041026, 53.6791852731242, 41.0448765909455, 47.914882163269, 51.5335837768147, 49.217483982331, 51.1389458112447, 48.0336788998195, 52.8223258874823, 47.9302436519206, 51.9475600881879, 54.0606647605687, 50.6559277814877, 52.6335181401208, 54.2725782631366, 43.083675000186, 49.0637500123524, 44.6632810590617, 50.293760020264, 51.2896166830574, 46.4638901013413, 48.3291073423363, 44.2590599394396, 52.6720469537052, 47.4439436135393, 48.2471522363413, 57.9737846304978, 42.4301080469913, 50.2908406548503, 52.3570034648903, 42.7127745966365, 52.2685711057555, 44.7936090146139, 46.3565980135841, 38.3575726240913, 39.6615373251182, 49.3105354600425, 42.1880618813037, 51.4796995926358, 54.4350155934243, 49.538139286023, 51.844375965857, 57.5052389013286, 57.6775357922151, 52.6180510620694, 32.375968743603, 42.0671208740445, 44.8980312830359, 34.1636884387502, 36.5443594656413, 47.3128113809467 ),
  `P_max` = c( 720.71392917246, 550.697854561856, 587.417097655553, 787.012012890423, 634.931737852808, 846.118844347542, 578.609811212814, 526.754221690469, 618.616237792837, 583.178213938422, 677.46839262409, 586.294927750192, 674.210503713599, 609.931100907041, 688.050026599798, 751.453351178328, 782.342779552722, 747.114860859711, 697.739178615104, 599.783453121439, 741.044102327642, 642.75054953451, 678.261118990867, 767.313623578198, 546.342710728637, 596.59219640589, 563.098821554568, 659.349421443241, 701.336341167984, 587.602874558339, 523.918403061563, 432.932490041913, 452.947736533387, 558.40548473631, 513.309499380782, 603.361877693434, 560.027085542369, 643.90077383047, 709.354344682915, 560.027686195494, 751.218524473997, 600.371875917469, 405.818662295843, 446.750025682425, 484.017990617248, 441.823569590808, 475.691772431568, 525.937225940098, 541.967355078066, 411.679083397134, 443.334425922299, 582.825995654505, 614.87764605695, 473.923630452372 ),
  `HR_percent` = c( 87.37, 85.07, 88.4, 90.93, 93.12, 93.78, 88.05, 88.16, 92.21, 91.97, 95.39, 93.58, 84.41, 84.65, 88.64, 89.98, 91.12, 94.04, 89.19, 89.26, 92.25, 92.32, 93.98, 94.9, 86.65, 87.88, 89.95, 91.14, 93.96, 94.27, 90.9, 86.55, 87.59, 91.62, 91.13, 95.12, 86.77, 87.49, 90.8, 89.74, 93.35, 94.18, 80.64, 81.03, 84.87, 88.21, 91.54, 92.47, 91.92, 88.36, 90.08, 92.49, 93.58, 93.31 ),
  `VO2_percent` = c( 71.27, 66.6, 72.84, 76.05, 83.97, 83.1, 73.17, 73.36, 75.13, 76.76, 82.17, 80.02, 65.96, 64.11, 76.98, 75.53, 81.19, 87.7, 77.06, 75.21, 82.91, 81.14, 86.2, 88.05, 69.58, 67.18, 76.09, 77.01, 83.24, 82.99, 68.97, 61.94, 67.21, 72.61, 75.89, 79.96, 59.98, 55.95, 63.85, 65.63, 72.7, 75.69, 74.64, 71.07, 77.24, 82.44, 89.64, 82.88, 64.16, 59.83, 66.38, 68.75, 72.98, 72.62 ),
  `ΔBLC` = c( 2.14440476388244, 2.41, 3.25, 2.66, 4.22, 1.59, 2.28540699787984, 2.32, 3.28, 4.11, 5.36, 3.76, 1.32245689846256, 1.08, 3.54, 2.04, 3.04, 4.51, 1.64039583472131, 2.2, 2.16, 2.09, 3.36, 2.99, 1.3350755332152, 1.78, 1.78, 2.45, 1.55, 3.71, 1.67230744683529, 1.82, 2.69, 3.15, 4.13, 4.37, 1.31227592057723, 2, 2.19, 1.64, 3.53, 2.61, 2.17216275938861, 2, 3.57, 4.4, 6.67, 5.02, 1.25297825424965, 1.1, 1.92, 2.75, 3.52, 3.03 ),
  `Wirk_Brutto` = c( 0.194078185911739, 0.196343582735698, 0.197432400180914, 0.191982384757164, 0.193086544076386, 0.202272676016536, 0.201989591006808, 0.198954268698329, 0.213884699225049, 0.202106215866216, 0.208084017401286, 0.204796096988283, 0.21470660342162, 0.210147226731705, 0.200166739368985, 0.211468312109366, 0.209443244124458, 0.198615513390234, 0.202381086762722, 0.196954594178895, 0.199517537814579, 0.199628484687111, 0.197282981278133, 0.200824804605943, 0.191923014420311, 0.196319824485844, 0.193841594019103, 0.205267008006466, 0.201968428815483, 0.2011181163041, 0.184016788833361, 0.201354804974137, 0.195899202325303, 0.183346335561148, 0.193559204902546, 0.185296161491302, 0.210696175026735, 0.214630777141548, 0.213394375277129, 0.201104035381978, 0.209997994918517, 0.198290954861808, 0.20773452767391, 0.217886833822587, 0.226711431535107, 0.216377701831782, 0.205576997149423, 0.223473964840429, 0.180078390824672, 0.194758653203162, 0.195651132846708, 0.185525668519753, 0.194462052029459, 0.200492223682485 ),
  `Wirk_Netto` = c( 0.214588302889431, 0.217889685609367, 0.217037576922628, 0.209935036710121, 0.209842225639692, 0.22036251836809, 0.223825454418398, 0.220604214782659, 0.236092711881316, 0.222276309166135, 0.227569247134379, 0.224692004793872, 0.238265661476133, 0.233161118998842, 0.217592452446886, 0.230526905019954, 0.22659520628953, 0.214232099386535, 0.225340673621847, 0.219152825839026, 0.219547254972467, 0.220209270579214, 0.215804852508426, 0.21954040413313, 0.21271752571714, 0.218131839125291, 0.212521625448746, 0.225135205615152, 0.219596429102608, 0.218464494393124, 0.199372013604675, 0.220311490150688, 0.212208891903133, 0.197469679063573, 0.207486131968207, 0.197921337608253, 0.232948263374776, 0.239084726196702, 0.234312401434944, 0.219957234546878, 0.227760886506515, 0.214487556382132, 0.227515517263034, 0.23970633930485, 0.247374969841044, 0.23487145799815, 0.220878507929398, 0.241505107822462, 0.207754949499467, 0.228049002927954, 0.224265269205112, 0.211051760242236, 0.219071807953363, 0.226818120152281 )
  , check.names = FALSE
)

names(Bedingungen_data_Wirkungsgrad)[names(Bedingungen_data_Wirkungsgrad) == "Wirk_Arbeit"] <- "Wirk_Arbeit [%]"
names(Bedingungen_data_Wirkungsgrad)[names(Bedingungen_data_Wirkungsgrad) == "Wirk_Brutto"] <- "Wirk_Brutto [%]"
names(Bedingungen_data_Wirkungsgrad)[names(Bedingungen_data_Wirkungsgrad) == "Wirk_Netto"] <- "Wirk_Netto [%]"
names(Bedingungen_data_Wirkungsgrad)[names(Bedingungen_data_Wirkungsgrad) == "W*Aerob [kJ]"] <- "W_Aerob [kJ]"
names(Bedingungen_data_Wirkungsgrad)[names(Bedingungen_data_Wirkungsgrad) == "WPCR [kJ]"] <- "W_PCR [kJ]"
names(Bedingungen_data_Wirkungsgrad)[names(Bedingungen_data_Wirkungsgrad) == "WBLC [kJ]"] <- "W_BLC [kJ]"
names(Bedingungen_data_Wirkungsgrad)[names(Bedingungen_data_Wirkungsgrad) == "WTOT [kJ]"] <- "W_TOT [kJ]"
names(Bedingungen_data_Wirkungsgrad)[names(Bedingungen_data_Wirkungsgrad) == "P_Tot"] <- "P_Tot [W]"
names(Bedingungen_data_Wirkungsgrad)[names(Bedingungen_data_Wirkungsgrad) == "P_mean"] <- "P_mech [W]"
names(Bedingungen_data_Wirkungsgrad)[names(Bedingungen_data_Wirkungsgrad) == "P_Int"] <- "P_Int [W]"
names(Bedingungen_data_Wirkungsgrad)[names(Bedingungen_data_Wirkungsgrad) == "nD"] <- "Drehzahl [U·min⁻¹]"
names(Bedingungen_data_Wirkungsgrad)[names(Bedingungen_data_Wirkungsgrad) == "Efficiency"] <- "Efficiency [%]"
names(Bedingungen_data_Wirkungsgrad)[names(Bedingungen_data_Wirkungsgrad) == "Pedal_Smoothness"] <- "Pedal_Smoothness [%]"
names(Bedingungen_data_Wirkungsgrad)[names(Bedingungen_data_Wirkungsgrad) == "P_max"] <- "P_max [W]"
names(Bedingungen_data_Wirkungsgrad)[names(Bedingungen_data_Wirkungsgrad) == "HR_percent"] <- "HR_percent [%]"
names(Bedingungen_data_Wirkungsgrad)[names(Bedingungen_data_Wirkungsgrad) == "VO2_percent"] <- "VO2_percent [%]"
names(Bedingungen_data_Wirkungsgrad)[names(Bedingungen_data_Wirkungsgrad) == "ΔBLC"] <- "ΔBLC [mmol·l⁻¹]"



################################################################################################
# Berechnung der Statistiken
stats <- data.frame(
  Variable = c("Wirk_Arbeit [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]", 
               "W_TOT [kJ]", "W_Aerob [kJ]", "W_PCR [kJ]", 
               "W_BLC [kJ]", "P_Tot [W]",
               "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]", "ΔBLC [mmol·l⁻¹]")
)

# Berechnung von Mittelwert und SD mit Multiplikation der Wirkungsgrade mit 100
stats$`Mittelwert ± SD` <- paste(
  case_when(
    stats$Variable %in% c("Wirk_Arbeit [%]", "Wirk_Brutto [%]", "Wirk_Netto [%]") ~
      sprintf("%.2f ± %.2f", 
              100 * sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], mean, na.rm=TRUE),
              100 * sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], sd, na.rm=TRUE)),
    stats$Variable %in% c("W_Aerob [kJ]", "W_PCR [kJ]", "W_BLC [kJ]", "W_TOT [kJ]") ~
      sprintf("%.1f ± %.1f", 
              sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], mean, na.rm=TRUE),
              sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], sd, na.rm=TRUE)),
    stats$Variable %in% c("P_Tot [W]", "P_mech [W]", "P_Int [W]") ~
      sprintf("%.1f ± %.1f",
              sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], mean, na.rm=TRUE),
              sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], sd, na.rm=TRUE)),
    stats$Variable == "Drehzahl [U·min⁻¹]" ~
      sprintf("%.1f ± %.1f",
              sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], mean, na.rm=TRUE),
              sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], sd, na.rm=TRUE)),
    stats$Variable == "ΔBLC [mmol·l⁻¹]" ~
      sprintf("%.2f ± %.2f",
              sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], mean, na.rm=TRUE),
              sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], sd, na.rm=TRUE))
  )
)

# Minimum und Maximum
stats$Minimum <- case_when(
  stats$Variable %in% c("Wirk_Arbeit [%]", "Wirk_Brutto [%]", "Wirk_Netto [%]") ~
    sprintf("%.2f", 100 * sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], min, na.rm=TRUE)),
  stats$Variable %in% c("W_Aerob [kJ]", "W_PCR [kJ]", "W_BLC [kJ]", "W_TOT [kJ]") ~
    sprintf("%.1f", sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], min, na.rm=TRUE)),
  stats$Variable %in% c("P_Tot [W]", "P_mech [W]", "P_Int [W]") ~
    sprintf("%.1f", sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], min, na.rm=TRUE)),
  stats$Variable == "Drehzahl [U·min⁻¹]" ~
    sprintf("%.1f", sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], min, na.rm=TRUE)),
  stats$Variable == "ΔBLC [mmol·l⁻¹]" ~
    sprintf("%.2f", sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], min, na.rm=TRUE))
)

stats$Maximum <- case_when(
  stats$Variable %in% c("Wirk_Arbeit [%]", "Wirk_Brutto [%]", "Wirk_Netto [%]") ~
    sprintf("%.2f", 100 * sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], max, na.rm=TRUE)),
  stats$Variable %in% c("W_Aerob [kJ]", "W_PCR [kJ]", "W_BLC [kJ]", "W_TOT [kJ]") ~
    sprintf("%.1f", sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], max, na.rm=TRUE)),
  stats$Variable %in% c("P_Tot [W]", "P_mech [W]", "P_Int [W]") ~
    sprintf("%.1f", sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], max, na.rm=TRUE)),
  stats$Variable == "Drehzahl [U·min⁻¹]" ~
    sprintf("%.1f", sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], max, na.rm=TRUE)),
  stats$Variable == "ΔBLC [mmol·l⁻¹]" ~
    sprintf("%.2f", sapply(Bedingungen_data_Wirkungsgrad[stats$Variable], max, na.rm=TRUE))
)

# Namen der Spalten
names(stats) <- c("Parameter", "Mittelwert ± SD", "Min", "Max")

# Tabelle erstellen
ft_Wirkungsgrad_stats_mean <- flextable(stats) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Wirk_Arbeit [%]",
    value = as_paragraph("η", as_sub("Arbeit"), " [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Wirk_Netto [%]",
    value = as_paragraph("η", as_sub("Netto"), " [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Wirk_Brutto [%]",
    value = as_paragraph("η", as_sub("Brutto"), " [%]")
  ) %>% 
  compose(
    j = "Parameter",
    i = ~ Parameter == "W_Aerob [kJ]",
    value = as_paragraph("W", as_sub("Aerob"), " [kJ]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "W_PCR [kJ]",
    value = as_paragraph("W", as_sub("PCr"), " [kJ]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "W_BLC [kJ]",
    value = as_paragraph("W", as_sub("BLC"), " [kJ]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "W_TOT [kJ]",
    value = as_paragraph("W", as_sub("TOT"), " [kJ]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Tot [W]",
    value = as_paragraph("P", as_sub("Tot"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech [W]",
    value = as_paragraph("P", as_sub("mech"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Int [W]",
    value = as_paragraph("P", as_sub("Int"), " [W]")
  ) %>%
  theme_zebra(odd_header = "grey92", 
              even_header = "#EFEFEF", 
              odd_body = "#F9F9F9", 
              even_body = "#FFFFFF") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  add_footer_row(
    values = as_paragraph(
      "η", as_sub("Arbeit"), " [%]: Arbeitwirkungsgrad; ",
      "η", as_sub("Netto"), " [%]: Nettowirkungsgrad; ",
      "η", as_sub("Brutto"), " [%]: Bruttowirkungsgrad; ",
      "W", as_sub("TOT"), " [kJ]: Gesamtenergieumsatz aus allen Stoffwechselwegen; ",
      "W", as_sub("Aerob"), " [kJ]: aerobe Energiebereitstellung; ",
      "W", as_sub("PCr"), " [kJ]: Energiebereitstellung aus PCr; ",
      "W", as_sub("BLC"), " [kJ]: Energiebereitstellung aus dem Blutlaktat; ",
      "P", as_sub("Tot"), " [W]: Summe aus mechanischer und interner Leistung; ",
      "P", as_sub("mech"), " [W]: mittlere mechanische Leistung; ",
      "P", as_sub("Int"), " [W]: interne Leistung; ",
      "Drehzahl [U·min⁻¹]: mittlere Trittrate; ",
      "ΔBLC [mmol·l⁻¹]: Netto-Blutlaktatakkumulation (Post - Pre)"
    ),
    colwidths = 4
  ) %>%
  line_spacing(space = 1.2, part = "footer") %>%
  font(fontname = "Source Sans Pro", part = "all") %>%
  fontsize(size = 13, part = "header") %>%
  fontsize(size = 13, part = "body") %>%
  fontsize(size = 11, part = "footer") %>%
  padding(padding = 4, part = "all") %>%
  border_outer(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  border_inner_h(part = "body", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  border_inner_v(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  hline_top(part = "footer", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  hline_bottom(part = "footer", border = fp_border(color = "darkgrey", width = 0.5))

# Setzen der Tabelle auf volle Breite
ft_Wirkungsgrad_stats_mean <- set_table_properties(ft_Wirkungsgrad_stats_mean, width = 1, layout = "autofit")

# Anzeigen der Tabelle
ft_Wirkungsgrad_stats_mean

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_Wirkungsgrad_stats_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_Wirkungsgrad_stats_mean.rds")
ft_Wirkungsgrad_stats_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_Wirkungsgrad_stats_mean.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_Wirkungsgrad_stats_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_Wirkungsgrad_stats_mean.rds")
ft_Wirkungsgrad_stats_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_Wirkungsgrad_stats_mean.rds")




################################################################################################

# Berechnung der Statistiken
stats <- data.frame(
  Variable = c("Wirk_Arbeit [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]", 
               "W_TOT [kJ]", "W_Aerob [kJ]", "W_PCR [kJ]", 
               "W_BLC [kJ]", "P_Tot [W]",
               "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]", "ΔBLC [mmol·l⁻¹]")
)

# Berechnung für sitzend und t-tests
stats$`Sitzen (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_sit <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "sitzen")
  data_stand <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "stehen")
  
  if(var %in% c("Wirk_Arbeit [%]", "Wirk_Brutto [%]", "Wirk_Netto [%]")) {
    # T-Test durchführen
    t_test <- t.test(data_sit[[var]], data_stand[[var]])
    p_value <- t_test$p.value
    
    # Statistische Ausgabe
    print(sprintf("\n--- Statistik für %s ---", var))
    print(sprintf("t-Wert: %.3f", t_test$statistic))
    print(sprintf("p-Wert: %.4f", p_value))
    print(sprintf("Freiheitsgrade: %.1f", t_test$parameter))
    print(sprintf("95%% Konfidenzintervall: [%.3f, %.3f]", 
                  t_test$conf.int[1], 
                  t_test$conf.int[2]))
    
    # Sternchen basierend auf p-Wert
    stars <- if(p_value < 0.001) "***" else if(p_value < 0.01) "**" else if(p_value < 0.05) "*" else ""
    
    sprintf("%.2f ± %.2f%s", 
            100 * mean(data_sit[[var]], na.rm=TRUE),
            100 * sd(data_sit[[var]], na.rm=TRUE),
            stars)
  } else if(var %in% c("W_Aerob [kJ]", "W_PCR [kJ]", "W_BLC [kJ]", "W_TOT [kJ]",
                       "P_Tot [W]", "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]")) {
    sprintf("%.1f ± %.1f",
            mean(data_sit[[var]], na.rm=TRUE),
            sd(data_sit[[var]], na.rm=TRUE))
  } else if(var == "ΔBLC [mmol·l⁻¹]") {
    sprintf("%.2f ± %.2f",
            mean(data_sit[[var]], na.rm=TRUE),
            sd(data_sit[[var]], na.rm=TRUE))
  }
})

# Berechnung für stehen
stats$`Stehen (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_stand <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "stehen")
  
  if(var %in% c("Wirk_Arbeit [%]", "Wirk_Brutto [%]", "Wirk_Netto [%]")) {
    sprintf("%.2f ± %.2f", 
            100 * mean(data_stand[[var]], na.rm=TRUE),
            100 * sd(data_stand[[var]], na.rm=TRUE))
  } else if(var %in% c("W_Aerob [kJ]", "W_PCR [kJ]", "W_BLC [kJ]", "W_TOT [kJ]",
                       "P_Tot [W]", "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]")) {
    sprintf("%.1f ± %.1f",
            mean(data_stand[[var]], na.rm=TRUE),
            sd(data_stand[[var]], na.rm=TRUE))
  } else if(var == "ΔBLC [mmol·l⁻¹]") {
    sprintf("%.2f ± %.2f",
            mean(data_stand[[var]], na.rm=TRUE),
            sd(data_stand[[var]], na.rm=TRUE))
  }
})

# Namen der Spalten
names(stats) <- c("Parameter", "Sitzen (MW ± SD)", "Stehen (MW ± SD)")

# Tabelle erstellen
ft_Wirkungsgrad_stats_Bedingung_mean <- flextable(stats) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Wirk_Arbeit [%]",
    value = as_paragraph("η", as_sub("Arbeit"), " [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Wirk_Netto [%]",
    value = as_paragraph("η", as_sub("Netto"), " [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Wirk_Brutto [%]",
    value = as_paragraph("η", as_sub("Brutto"), " [%]")
  ) %>% 
  compose(
    j = "Parameter",
    i = ~ Parameter == "W_Aerob [kJ]",
    value = as_paragraph("W", as_sub("Aerob"), " [kJ]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "W_PCR [kJ]",
    value = as_paragraph("W", as_sub("PCr"), " [kJ]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "W_BLC [kJ]",
    value = as_paragraph("W", as_sub("BLC"), " [kJ]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "W_TOT [kJ]",
    value = as_paragraph("W", as_sub("TOT"), " [kJ]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Tot [W]",
    value = as_paragraph("P", as_sub("Tot"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech [W]",
    value = as_paragraph("P", as_sub("mech"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Int [W]",
    value = as_paragraph("P", as_sub("Int"), " [W]")
  ) %>%
  theme_zebra(odd_header = "grey92", 
              even_header = "#EFEFEF", 
              odd_body = "#F9F9F9", 
              even_body = "#FFFFFF") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  add_footer_row(
    values = as_paragraph(
      "Signifikanzniveaus: *** p < 0.001; ** p < 0.01; * p < 0.05\n",
      "η", as_sub("Arbeit"), " [%]: Arbeitwirkungsgrad; ",
      "η", as_sub("Netto"), " [%]: Nettowirkungsgrad; ",
      "η", as_sub("Brutto"), " [%]: Bruttowirkungsgrad; ",
      "W", as_sub("TOT"), " [kJ]: Gesamtenergieumsatz aus allen Stoffwechselwegen; ",
      "W", as_sub("Aerob"), " [kJ]: aerobe Energiebereitstellung; ",
      "W", as_sub("PCr"), " [kJ]: Energiebereitstellung aus PCr; ",
      "W", as_sub("BLC"), " [kJ]: Energiebereitstellung aus dem Blutlaktat; ",
      "P", as_sub("Tot"), " [W]: Summe aus mechanischer und interner Leistung; ",
      "P", as_sub("mech"), " [W]: mittlere mechanische Leistung; ",
      "P", as_sub("Int"), " [W]: interne Leistung; ",
      "Drehzahl [U·min⁻¹]: mittlere Trittrate; ",
      "ΔBLC [mmol·l⁻¹]: Netto-Blutlaktatakkumulation (Post - Pre)"
    ),
    colwidths = 3
  ) %>%
  line_spacing(space = 1.2, part = "footer") %>%
  font(fontname = "Source Sans Pro", part = "all") %>%
  fontsize(size = 13, part = "header") %>%
  fontsize(size = 13, part = "body") %>%
  fontsize(size = 11, part = "footer") %>%
  padding(padding = 4, part = "all") %>%
  border_outer(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  border_inner_h(part = "body", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  border_inner_v(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  hline_top(part = "footer", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  hline_bottom(part = "footer", border = fp_border(color = "darkgrey", width = 0.5))

# Setzen der Tabelle auf volle Breite
ft_Wirkungsgrad_stats_Bedingung_mean <- set_table_properties(ft_Wirkungsgrad_stats_Bedingung_mean, width = 1, layout = "autofit")

# Anzeigen der Tabelle
ft_Wirkungsgrad_stats_Bedingung_mean

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_Wirkungsgrad_stats_Bedingung_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_Wirkungsgrad_stats_Bedingung_mean.rds")
ft_Wirkungsgrad_stats_Bedingung_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_Wirkungsgrad_stats_Bedingung_mean.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_Wirkungsgrad_stats_Bedingung_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_Wirkungsgrad_stats_Bedingung_mean.rds")
ft_Wirkungsgrad_stats_Bedingung_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_Wirkungsgrad_stats_Bedingung_mean.rds")


######################################################################################################################

# Berechnung der Statistiken
stats <- data.frame(
  Variable = c("Wirk_Arbeit [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]", 
               "W_TOT [kJ]", "W_Aerob [kJ]", "W_PCR [kJ]", 
               "W_BLC [kJ]", "P_Tot [W]",
               "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]", "ΔBLC [mmol·l⁻¹]")
)

# Funktion zur Formatierung von Mittelwert ± SD
format_mean_sd <- function(data, var, multiply_by_100 = FALSE) {
  mean_val <- mean(data[[var]], na.rm = TRUE)
  sd_val <- sd(data[[var]], na.rm = TRUE)
  
  if(multiply_by_100) {
    mean_val <- mean_val * 100
    sd_val <- sd_val * 100
    return(sprintf("%.2f ± %.2f", mean_val, sd_val))
  }
  
  if(var == "ΔBLC [mmol·l⁻¹]") {
    return(sprintf("%.2f ± %.2f", mean_val, sd_val))
  } else if(var %in% c("W_Aerob [kJ]", "W_PCR [kJ]", "W_BLC [kJ]", "W_TOT [kJ]",
                       "P_Tot [W]", "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]")) {
    return(sprintf("%.1f ± %.1f", mean_val, sd_val))
  }
}

# Berechnung für leicht
stats$`Leicht (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_light <- filter(Bedingungen_data_Wirkungsgrad, Intensität == "leicht")
  format_mean_sd(data_light, var, 
                 multiply_by_100 = var %in% c("Wirk_Arbeit [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]"))
})

# Berechnung für moderat
stats$`Moderat (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_moderate <- filter(Bedingungen_data_Wirkungsgrad, Intensität == "moderat")
  format_mean_sd(data_moderate, var, 
                 multiply_by_100 = var %in% c("Wirk_Arbeit [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]"))
})

# Berechnung für schwer
stats$`Schwer (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_heavy <- filter(Bedingungen_data_Wirkungsgrad, Intensität == "schwer")
  format_mean_sd(data_heavy, var, 
                 multiply_by_100 = var %in% c("Wirk_Arbeit [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]"))
})

# Namen der Spalten
names(stats) <- c("Parameter", "Leicht (MW ± SD)", "Moderat (MW ± SD)", "Schwer (MW ± SD)")

# Tabelle erstellen
ft_Wirkungsgrad_stats_Intensitaet_mean <- flextable(stats) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Wirk_Arbeit [%]",
    value = as_paragraph("η", as_sub("Arbeit"), " [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Wirk_Netto [%]",
    value = as_paragraph("η", as_sub("Netto"), " [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Wirk_Brutto [%]",
    value = as_paragraph("η", as_sub("Brutto"), " [%]")
  ) %>% 
  compose(
    j = "Parameter",
    i = ~ Parameter == "W_Aerob [kJ]",
    value = as_paragraph("W", as_sub("Aerob"), " [kJ]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "W_PCR [kJ]",
    value = as_paragraph("W", as_sub("PCr"), " [kJ]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "W_BLC [kJ]",
    value = as_paragraph("W", as_sub("BLC"), " [kJ]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "W_TOT [kJ]",
    value = as_paragraph("W", as_sub("TOT"), " [kJ]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Tot [W]",
    value = as_paragraph("P", as_sub("Tot"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech [W]",
    value = as_paragraph("P", as_sub("mech"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Int [W]",
    value = as_paragraph("P", as_sub("Int"), " [W]")
  ) %>%
  theme_zebra(odd_header = "grey92", 
              even_header = "#EFEFEF", 
              odd_body = "#F9F9F9", 
              even_body = "#FFFFFF") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  add_footer_row(
    values = as_paragraph(
      "η", as_sub("Arbeit"), " [%]: Arbeitwirkungsgrad; ",
      "η", as_sub("Netto"), " [%]: Nettowirkungsgrad; ",
      "η", as_sub("Brutto"), " [%]: Bruttowirkungsgrad; ",
      "W", as_sub("TOT"), " [kJ]: Gesamtenergieumsatz aus allen Stoffwechselwegen; ",
      "W", as_sub("Aerob"), " [kJ]: aerobe Energiebereitstellung; ",
      "W", as_sub("PCr"), " [kJ]: Energiebereitstellung aus PCr; ",
      "W", as_sub("BLC"), " [kJ]: Energiebereitstellung aus dem Blutlaktat; ",
      "P", as_sub("Tot"), " [W]: Summe aus mechanischer und interner Leistung; ",
      "P", as_sub("mech"), " [W]: mittlere mechanische Leistung; ",
      "P", as_sub("Int"), " [W]: interne Leistung; ",
      "Drehzahl [U·min⁻¹]: mittlere Trittrate; ",
      "ΔBLC [mmol·l⁻¹]: Netto-Blutlaktatakkumulation (Post - Pre)"
    ),
    colwidths = 4
  ) %>%
  line_spacing(space = 1.2, part = "footer") %>%
  font(fontname = "Source Sans Pro", part = "all") %>%
  fontsize(size = 13, part = "header") %>%
  fontsize(size = 13, part = "body") %>%
  fontsize(size = 11, part = "footer") %>%
  padding(padding = 4, part = "all") %>%
  border_outer(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  border_inner_h(part = "body", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  border_inner_v(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  hline_top(part = "footer", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  hline_bottom(part = "footer", border = fp_border(color = "darkgrey", width = 0.5))

# Setzen der Tabelle auf volle Breite
ft_Wirkungsgrad_stats_Intensitaet_mean <- set_table_properties(ft_Wirkungsgrad_stats_Intensitaet_mean, width = 1, layout = "autofit")

# Anzeigen der Tabelle
ft_Wirkungsgrad_stats_Intensitaet_mean

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_Wirkungsgrad_stats_Intensitaet_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_Wirkungsgrad_stats_Intensitaet_mean.rds")
ft_Wirkungsgrad_stats_Intensitaet_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_Wirkungsgrad_stats_Intensitaet_mean.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_Wirkungsgrad_stats_Intensitaet_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_Wirkungsgrad_stats_Intensitaet_mean.rds")
ft_Wirkungsgrad_stats_Intensitaet_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_Wirkungsgrad_stats_Intensitaet_mean.rds")


###########################################################################################################
# Berechnung der Statistiken
stats <- data.frame(
  Variable = c("Wirk_Arbeit [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]", 
               "W_TOT [kJ]", "W_Aerob [kJ]", "W_PCR [kJ]", 
               "W_BLC [kJ]", "P_Tot [W]",
               "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]", "ΔBLC [mmol·l⁻¹]")
)

# Funktion zur Formatierung von Mittelwert ± SD
format_mean_sd <- function(data, var, multiply_by_100 = FALSE) {
  mean_val <- mean(data[[var]], na.rm = TRUE)
  sd_val <- sd(data[[var]], na.rm = TRUE)
  
  if(multiply_by_100) {
    mean_val <- mean_val * 100
    sd_val <- sd_val * 100
    return(sprintf("%.2f ± %.2f", mean_val, sd_val))
  }
  
  if(var == "ΔBLC [mmol·l⁻¹]") {
    return(sprintf("%.2f ± %.2f", mean_val, sd_val))
  } else if(var %in% c("W_Aerob [kJ]", "W_PCR [kJ]", "W_BLC [kJ]", "W_TOT [kJ]",
                       "P_Tot [W]", "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]")) {
    return(sprintf("%.1f ± %.1f", mean_val, sd_val))
  }
}

# Daten für Bedingung und Intensität zusammenstellen
stats_bed_int <- data.frame(
  Variable = stats$Variable,
  
  # Sitzen Leicht
  `Sitzen Leicht (MW ± SD)` = sapply(stats$Variable, function(var) {
    data_subset <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "sitzen", Intensität == "leicht")
    format_mean_sd(data_subset, var, 
                   multiply_by_100 = var %in% c("Wirk_Arbeit [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]"))
  }),
  
  # Sitzen Moderat
  `Sitzen Moderat (MW ± SD)` = sapply(stats$Variable, function(var) {
    data_subset <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "sitzen", Intensität == "moderat")
    format_mean_sd(data_subset, var, 
                   multiply_by_100 = var %in% c("Wirk_Arbeit [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]"))
  }),
  
  # Sitzen Schwer
  `Sitzen Schwer (MW ± SD)` = sapply(stats$Variable, function(var) {
    data_subset <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "sitzen", Intensität == "schwer")
    format_mean_sd(data_subset, var, 
                   multiply_by_100 = var %in% c("Wirk_Arbeit [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]"))
  }),
  
  # Stehen Leicht
  `Stehen Leicht (MW ± SD)` = sapply(stats$Variable, function(var) {
    data_subset <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "stehen", Intensität == "leicht")
    format_mean_sd(data_subset, var, 
                   multiply_by_100 = var %in% c("Wirk_Arbeit [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]"))
  }),
  
  # Stehen Moderat
  `Stehen Moderat (MW ± SD)` = sapply(stats$Variable, function(var) {
    data_subset <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "stehen", Intensität == "moderat")
    format_mean_sd(data_subset, var, 
                   multiply_by_100 = var %in% c("Wirk_Arbeit [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]"))
  }),
  
  # Stehen Schwer
  `Stehen Schwer (MW ± SD)` = sapply(stats$Variable, function(var) {
    data_subset <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "stehen", Intensität == "schwer")
    format_mean_sd(data_subset, var, 
                   multiply_by_100 = var %in% c("Wirk_Arbeit [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]"))
  })
)

# Tabelle erstellen
ft_Wirkungsgrad_stats_Bedingung_Intensitaet_mean <- flextable(stats_bed_int) %>%
  set_header_labels(
    Variable = "Parameter",
    Sitzen.Leicht..MW...SD. = "Leicht",
    Sitzen.Moderat..MW...SD. = "Moderat",
    Sitzen.Schwer..MW...SD. = "Schwer",
    Stehen.Leicht..MW...SD. = "Leicht",
    Stehen.Moderat..MW...SD. = "Moderat",
    Stehen.Schwer..MW...SD. = "Schwer"
  ) %>%
  add_header_row(
    values = c("", "Sitzen (MW ± SD)", "Stehen (MW ± SD)"),
    colwidths = c(1, 3, 3)
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "Wirk_Arbeit [%]",
    value = as_paragraph("η", as_sub("Arbeit"), " [%]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "Wirk_Netto [%]",
    value = as_paragraph("η", as_sub("Netto"), " [%]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "Wirk_Brutto [%]",
    value = as_paragraph("η", as_sub("Brutto"), " [%]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "W_Aerob [kJ]",
    value = as_paragraph("W", as_sub("Aerob"), " [kJ]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "W_PCR [kJ]",
    value = as_paragraph("W", as_sub("PCr"), " [kJ]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "W_BLC [kJ]",
    value = as_paragraph("W", as_sub("BLC"), " [kJ]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "W_TOT [kJ]",
    value = as_paragraph("W", as_sub("TOT"), " [kJ]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_Tot [W]",
    value = as_paragraph("P", as_sub("Tot"), " [W]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_mech [W]",
    value = as_paragraph("P", as_sub("mech"), " [W]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_Int [W]",
    value = as_paragraph("P", as_sub("Int"), " [W]")
  ) %>%
  theme_zebra(
    odd_header = "grey92",
    even_header = "grey92",
    odd_body = "#F9F9F9",
    even_body = "#FFFFFF"
  ) %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  add_footer_row(
    values = as_paragraph(
      "η", as_sub("Arbeit"), " [%]: Arbeitwirkungsgrad; ",
      "η", as_sub("Netto"), " [%]: Nettowirkungsgrad; ",
      "η", as_sub("Brutto"), " [%]: Bruttowirkungsgrad; ",
      "W", as_sub("TOT"), " [kJ]: Gesamtenergieumsatz aus allen Stoffwechselwegen; ",
      "W", as_sub("Aerob"), " [kJ]: aerobe Energiebereitstellung; ",
      "W", as_sub("PCr"), " [kJ]: Energiebereitstellung aus PCr; ",
      "W", as_sub("BLC"), " [kJ]: Energiebereitstellung aus dem Blutlaktat; ",
      "P", as_sub("Tot"), " [W]: Summe aus mechanischer und interner Leistung; ",
      "P", as_sub("mech"), " [W]: mittlere mechanische Leistung; ",
      "P", as_sub("Int"), " [W]: interne Leistung; ",
      "Drehzahl [U·min⁻¹]: mittlere Trittrate; ",
      "ΔBLC [mmol·l⁻¹]: Netto-Blutlaktatakkumulation (Post - Pre)"
    ),
    colwidths = 7
  ) %>%
  line_spacing(space = 1.2, part = "footer") %>%
  font(fontname = "Source Sans Pro", part = "all") %>%
  fontsize(size = 13, part = "header") %>%
  fontsize(size = 13, part = "body") %>%
  fontsize(size = 11, part = "footer") %>%
  padding(padding = 4, part = "all") %>%
  border_outer(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  border_inner_h(part = "body", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  border_inner_v(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  hline(i = 1, part = "header", j = 2:7, border = fp_border(color = "darkgrey", width = 0.5)) %>%
  hline_top(part = "footer", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  hline_bottom(part = "footer", border = fp_border(color = "darkgrey", width = 0.5))

# Setzen der Tabelle auf volle Breite
ft_Wirkungsgrad_stats_Bedingung_Intensitaet_mean <- set_table_properties(
  ft_Wirkungsgrad_stats_Bedingung_Intensitaet_mean, 
  width = 1, 
  layout = "autofit"
)

# Anzeigen der Tabelle
ft_Wirkungsgrad_stats_Bedingung_Intensitaet_mean


# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_Wirkungsgrad_stats_Bedingung_Intensitaet_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_Wirkungsgrad_stats_Bedingung_Intensitaet_mean.rds")
ft_Wirkungsgrad_stats_Bedingung_Intensitaet_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_Wirkungsgrad_stats_Bedingung_Intensitaet_mean.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_Wirkungsgrad_stats_Bedingung_Intensitaet_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_Wirkungsgrad_stats_Bedingung_Intensitaet_mean.rds")
ft_Wirkungsgrad_stats_Bedingung_Intensitaet_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_Wirkungsgrad_stats_Bedingung_Intensitaet_mean.rds")
