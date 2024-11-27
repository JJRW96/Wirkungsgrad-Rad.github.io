library(flextable)
library(dplyr)
library(officer)
library(sysfonts)
library(car)
library(ez)

Bedingungen_data <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/Bedingungen_data.rds")
Bedingungen_data_Wirkungsgrad <- Bedingungen_data[, c("Proband", "Bedingung", "Intensität", "Wirk_Muskulär", "W*Aerob [kJ]", "WPCR [kJ]", "WBLC [kJ]", "WTOT [kJ]", "P_Tot", "P_mean", "P_Int", "nD", "Efficiency", "Pedal_Smoothness", "P_max", "HR_percent", "VO2_percent", "ΔBLC", "Wirk_Brutto", "Wirk_Netto", "Wirk_Arbeit")]

Bedingungen_data_Wirkungsgrad <- data.frame(
  `Proband` = c( "1", "1", "1", "1", "1", "1", "6", "6", "6", "6", "6", "6", "10", "10", "10", "10", "10", "10", "13", "13", "13", "13", "13", "13", "15", "15", "15", "15", "15", "15", "19", "19", "19", "19", "19", "19", "20", "20", "20", "20", "20", "20", "22", "22", "22", "22", "22", "22", "23", "23", "23", "23", "23", "23" ),
  `Bedingung` = c( "stehen", "sitzen", "sitzen", "stehen", "sitzen", "stehen", "stehen", "sitzen", "stehen", "sitzen", "stehen", "sitzen", "stehen", "sitzen", "sitzen", "stehen", "stehen", "sitzen", "stehen", "sitzen", "stehen", "sitzen", "sitzen", "stehen", "sitzen", "stehen", "sitzen", "stehen", "stehen", "sitzen", "stehen", "sitzen", "sitzen", "stehen", "sitzen", "stehen", "sitzen", "stehen", "stehen", "sitzen", "stehen", "sitzen", "sitzen", "stehen", "stehen", "sitzen", "sitzen", "stehen", "stehen", "sitzen", "sitzen", "stehen", "stehen", "sitzen" ),
  `Intensität` = c( "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer" ),
  `Wirk_Muskulär` = c( 0.236759931308537, 0.246060440608401, 0.242278598743508, 0.229631619431488, 0.231738880422805, 0.239606281503452, 0.235519448379419, 0.240751242958341, 0.248138483274488, 0.238973954766177, 0.239905278154682, 0.242987590176426, 0.253197378567756, 0.268813524126113, 0.246034674712508, 0.243007190386035, 0.237707989694929, 0.239357891786449, 0.246622974378944, 0.259752672010051, 0.237157651346767, 0.256082449948023, 0.248047661517146, 0.237373974289561, 0.25603524395526, 0.230241128289079, 0.252398922767891, 0.237853768450825, 0.236244609842595, 0.256844301131177, 0.229409310695982, 0.250592406029218, 0.239641017649487, 0.225830203382915, 0.230325301880732, 0.22455381609002, 0.2675313748833, 0.254378184934767, 0.247600833815352, 0.250156276904196, 0.239017863265009, 0.241462546103667, 0.252453992795637, 0.255683248260526, 0.261633090232167, 0.257132037314701, 0.242104726162168, 0.256054300732916, 0.235970111612527, 0.269069913213186, 0.259743624458201, 0.236785575354483, 0.242749525282335, 0.259138280507211 ),
  `W*Aerob [kJ]` = c( 364.789166022847, 341.629555246705, 380.175743058369, 397.985468023398, 434.291677759482, 433.605222297912, 324.974079503018, 328.244436599334, 338.782659218172, 345.676143718918, 364.680735327353, 257.547695226202, 373.643424879889, 359.053884392895, 449.590938122183, 437.033618580678, 472.355442968317, 502.398965566933, 358.837718601139, 352.868283295468, 394.753364696823, 386.919677670582, 414.624059322391, 422.974893188083, 336.736950672026, 327.040591917684, 374.940893404024, 376.054742927363, 407.568603441812, 413.170309433097, 301.002193914676, 268.616569204091, 293.782454560316, 323.273033581013, 338.095668739063, 356.147551686419, 301.108945263692, 280.951141839367, 322.983175747043, 336.778695169108, 374.987772949063, 386.657728278586, 266.533422098459, 251.790208549518, 278.168675291213, 296.280182766526, 322.403130586883, 294.593727223427, 228.955584309471, 211.732892340367, 242.182720592561, 251.873990890611, 269.030850954527, 263.980675271676 ),
  `WPCR [kJ]` = c( 24.1291998261524, 31.245535169392, 29.1930235666251, 40.7334747429318, 32.1503897341314, 31.0960321619306, 35.5542282425247, 29.8757643229022, 32.3919515398355, 37.4564805468453, 39.181054527315, 28.0856376372225, 25.6241011760421, 42.2362045296838, 43.8865576245131, 46.7522391886717, 55.8553450423781, 41.2539914795555, 33.5030632789753, 39.5610791298099, 47.0313952643199, 43.4984491855838, 48.5683623835727, 47.4643724094369, 28.7114346014479, 27.082865453765, 33.5487723155105, 28.4952050444725, 46.7129326531457, 36.4259657605345, 26.4583220374129, 20.1814127976726, 29.9859705398912, 25.6937955351603, 32.5159707007087, 35.3146550491163, 26.041151647366, 18.4426421779503, 25.8050373268292, 31.1359690227005, 24.1696808725351, 31.9239901224492, 18.2312728474559, 19.2851143909574, 15.5481731391339, 15.0049470739965, 26.5074844823288, 32.024059076605, 19.4890043814307, 11.8721280138382, 16.7847021024629, 20.5569026538069, 25.1374379120318, 20.8232926492238 ),
  `WBLC [kJ]` = c( 10.3313513320441, 11.610940774608, 15.6579076836, 12.815395211808, 20.331190899936, 7.660330220592, 10.5760425135007, 10.736126499168, 15.178661602272, 19.019603410164, 24.804154325664, 17.399929153824, 6.87435813197033, 5.614025524128, 18.401528106864, 10.604270434464, 15.802442216064, 23.443754735016, 7.48717405349485, 10.04134646592, 9.858776530176, 9.539279142624, 15.335874602496, 13.647102696864, 6.43215059991284, 8.575715592864, 8.575715592864, 11.80365348456, 7.46761751064, 17.874103848048, 6.89074621609046, 7.49931368004, 11.08415043918, 12.9795813693, 17.01767335086, 18.00659383614, 6.65505918050507, 10.142774208, 11.10633775776, 8.31707485056, 17.90199647712, 13.23632034144, 6.60952692345146, 6.0856645248, 10.862911176768, 13.38846195456, 20.295691190208, 15.275017957248, 4.76575332014558, 4.1838943608, 7.30279742976, 10.459735902, 13.38846195456, 11.52472719384 ),
  `WTOT [kJ]` = c( 399.249717181043, 384.486031190706, 425.026674308594, 451.534337978137, 486.77325839355, 472.361584680434, 371.104350259043, 368.856327421404, 386.353272360279, 402.152227675927, 428.665944180332, 303.033262017248, 406.141884187901, 406.904114446707, 511.87902385356, 494.390128203813, 544.013230226759, 567.096711781505, 399.827955933609, 402.470708891198, 451.643536491319, 439.95740599879, 478.52829630846, 484.086368294384, 371.880535873387, 362.699172964313, 417.065381312399, 416.353601456395, 461.749153605597, 467.47037904168, 334.351262168179, 296.297295681804, 334.852575539387, 361.946410485473, 387.629312790631, 409.468800571675, 333.805156091563, 309.536558225317, 359.894550831632, 376.231739042368, 417.059450298718, 431.818038742475, 291.374221869366, 277.160987465275, 304.579759607115, 324.673591795083, 369.20630625942, 341.89280425728, 253.210342011047, 227.788914715005, 266.270220124784, 282.890629446418, 307.556750821119, 296.32869511474 ),
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
  `Wirk_Brutto` = c( 0.194456567480702, 0.196726385412574, 0.197764678141397, 0.192230498680745, 0.193264417226447, 0.202443439619024, 0.202229780630316, 0.199098331890053, 0.214150225292568, 0.20227158048794, 0.208225827389819, 0.204915793995188, 0.215057540311526, 0.210504669469585, 0.200511074112805, 0.211853723422249, 0.20985395625048, 0.198847735958903, 0.202554570737451, 0.197115691234667, 0.19976488329692, 0.199808399665771, 0.199192755370588, 0.202000634977887, 0.192157772550784, 0.196537747406888, 0.19452613083889, 0.205487721457738, 0.202177211103634, 0.20124684244861, 0.18409184496177, 0.201467334518711, 0.196118723676215, 0.183536150365779, 0.193698741978436, 0.185376762660182, 0.210998847405602, 0.214864935591682, 0.213566044685699, 0.201305985423474, 0.210176398290093, 0.198491427535928, 0.207677528245011, 0.217756402480937, 0.226533340542139, 0.216286074992791, 0.207360045036577, 0.224606804322476, 0.1801806079014, 0.194782431151652, 0.195649321136869, 0.185607315201515, 0.194548937671563, 0.200637916698556 ),
  `Wirk_Netto` = c( 0.215050980096665, 0.218361213965118, 0.21743918934395, 0.210231759369089, 0.210052326032456, 0.220565206870156, 0.224120419796381, 0.220781351398079, 0.236416282347832, 0.222476343746092, 0.227738869937329, 0.224836096783589, 0.238697915485563, 0.233601219961414, 0.217999410562797, 0.230984992520443, 0.227076019064188, 0.214502300481599, 0.225555773878964, 0.219352301338166, 0.219846792958958, 0.220428214984546, 0.218092136279684, 0.220946373711054, 0.213005949212925, 0.218400909748379, 0.213344733685403, 0.225400741075485, 0.219843269599878, 0.218616391702131, 0.199460121420396, 0.220446212520149, 0.212466511624337, 0.197689880977766, 0.207646479631449, 0.198013299225654, 0.233318299679759, 0.239375318086974, 0.234519392583452, 0.220198847250673, 0.227970762256012, 0.214722135551155, 0.227447147538819, 0.239548486250032, 0.247162950474793, 0.234763503071659, 0.2229381955183, 0.24282867143177, 0.207891012768686, 0.228081605080525, 0.224262888819237, 0.211157426124462, 0.219182082639067, 0.227004603734556 ),
  `Wirk_Arbeit` = c( 0.247862487404545, 0.253160968214816, 0.248317305395203, 0.238101384924385, 0.235636793902554, 0.248353056021652, 0.252223124451487, 0.248655390436469, 0.264750516922094, 0.247972342497358, 0.252051352897696, 0.249835173535799, 0.290674339857892, 0.284351853248305, 0.254041969959835, 0.27075806004536, 0.262059956436982, 0.246006374208853, 0.282917168134632, 0.274677400862769, 0.267938862065388, 0.270217570609923, 0.262573343325185, 0.265388250568887, 0.265857359551926, 0.274313879330951, 0.259309573409811, 0.274063975273831, 0.261750935900445, 0.259684339862698, 0.23098710636663, 0.26058009978106, 0.245991096843795, 0.22621104152906, 0.235354360073126, 0.222849715844585, 0.286385011940742, 0.299154252396369, 0.283190110066154, 0.263522755836858, 0.267668336610019, 0.250621087215401, 0.257193553520699, 0.272706537461809, 0.277912008983077, 0.261953055017254, 0.24533102151784, 0.269380995629809, 0.25452140555158, 0.286410297975261, 0.271577778890731, 0.252576522311175, 0.258114707808469, 0.269138120186613 )
  , check.names = FALSE
)

# Stehen Werte in Wirk_Arbeit auf NA
Bedingungen_data_Wirkungsgrad$Wirk_Arbeit[Bedingungen_data_Wirkungsgrad$Bedingung == "stehen"] <- NA
names(Bedingungen_data_Wirkungsgrad)[names(Bedingungen_data_Wirkungsgrad) == "Wirk_Muskulär"] <- "Wirk_muskulär [%]"
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
  Variable = c("Wirk_muskulär [%]","Wirk_Arbeit [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]", 
               "W_TOT [kJ]", "W_Aerob [kJ]", "W_PCR [kJ]", 
               "W_BLC [kJ]", "P_Tot [W]",
               "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]", "ΔBLC [mmol·l⁻¹]")
)

# Berechnung von Mittelwert und SD mit Multiplikation der Wirkungsgrade mit 100
stats$`Mittelwert ± SD` <- paste(
  case_when(
    stats$Variable %in% c("Wirk_muskulär [%]", "Wirk_Arbeit [%]","Wirk_Brutto [%]", "Wirk_Netto [%]") ~
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
  stats$Variable %in% c("Wirk_muskulär [%]", "Wirk_Arbeit [%]","Wirk_Brutto [%]", "Wirk_Netto [%]") ~
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
  stats$Variable %in% c("Wirk_muskulär [%]", "Wirk_Arbeit [%]","Wirk_Brutto [%]", "Wirk_Netto [%]") ~
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
    i = ~ Parameter == "Wirk_muskulär [%]",
    value = as_paragraph("η", as_sub("muskulär"), " [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Wirk_Arbeit [%]",
    value = as_paragraph("η", as_sub("Arbeit,sitzen"), " [%]")
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
  bold(i = ~ Parameter == "Wirk_muskulär [%]") %>%
  add_footer_row(
    values = as_paragraph(
      "η", as_sub("muskulär"), " [%]: muskulärer Wirkungsgrad; ",
      "η", as_sub("Arbeit,sitzen"), " [%]: Arbeitswirkungsgrad (sitzende Position); ",
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
  Variable = c("Wirk_muskulär [%]","Wirk_Arbeit [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]", 
               "W_TOT [kJ]", "W_Aerob [kJ]", "W_PCR [kJ]", 
               "W_BLC [kJ]", "P_Tot [W]",
               "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]", "ΔBLC [mmol·l⁻¹]")
)

# Berechnung für sitzend und ANOVAs
stats$`Sitzen (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_sit <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "sitzen")
  data_stand <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "stehen")
  
  check_all_na <- function(x) all(is.na(x))
  
  if(var %in% c("Wirk_muskulär [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]", 
                "W_TOT [kJ]", "W_Aerob [kJ]", "W_PCR [kJ]", "W_BLC [kJ]",
                "P_Tot [W]", "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]", "ΔBLC [mmol·l⁻¹]")) {  
    
    if(check_all_na(data_sit[[var]])) {
      return("-")
    }
    
    temp_data <- Bedingungen_data_Wirkungsgrad
    
    if(grepl("Wirk", var)) {
      temp_data$dv <- temp_data[[var]] * 100
    } else {
      temp_data$dv <- temp_data[[var]]
    }
    
    # Normalverteilungstests
    shapiro_sit <- shapiro.test(data_sit[[var]])
    shapiro_stand <- shapiro.test(data_stand[[var]])
    normal_dist <- shapiro_sit$p.value > 0.05 && shapiro_stand$p.value > 0.05
    
    # Varianzhomogenitätstest
    temp_data$Bedingung <- as.factor(temp_data$Bedingung)
    levene_result <- car::leveneTest(temp_data$dv ~ temp_data$Bedingung)
    var_homogen <- levene_result$`Pr(>F)`[1] > 0.05
    
    # Testauswahl und Durchführung mit detaillierter Nachricht
    test_message <- sprintf("\nVariable: %s\n", var)
    test_message <- paste0(test_message,
                           sprintf("Normalverteilung: %s\n", ifelse(normal_dist, "ja", "nein")))
    test_message <- paste0(test_message,
                           sprintf("Varianzhomogenität: %s\n", ifelse(var_homogen, "ja", "nein")))
    
    if(normal_dist && var_homogen) {
      # Parametrischer Test (ezANOVA)
      test_result <- ezANOVA(
        data = temp_data,
        dv = dv,
        wid = Proband,
        within = Bedingung,
        detailed = TRUE
      )
      p_value <- test_result$ANOVA$p[2]
      
      # Berechnung des partiellen Eta-Quadrat
      F_value <- test_result$ANOVA$F[2]
      df_effect <- test_result$ANOVA$DFn[2]
      df_error <- test_result$ANOVA$DFd[2]
      partial_eta_sq <- (F_value * df_effect) / (F_value * df_effect + df_error)
      
      test_message <- paste0(test_message,
                             "\n--- ANOVA Ergebnisse ---\n",
                             sprintf("F(%d,%d) = %.3f\n",
                                     test_result$ANOVA$DFn[2],
                                     test_result$ANOVA$DFd[2],
                                     test_result$ANOVA$F[2]),
                             sprintf("p-Wert = %.4f\n", p_value),
                             sprintf("Partielles Eta-Quadrat (η²p) = %.3f\n",
                                     partial_eta_sq),
                             "\nVerwendeter Test: Parametrischer Test (ezANOVA)\n")
    } else {
      # Nicht-parametrischer Test (Wilcoxon)
      sit_values <- temp_data$dv[temp_data$Bedingung == "sitzen"]
      stand_values <- temp_data$dv[temp_data$Bedingung == "stehen"]
      wilcox_result <- wilcox.test(sit_values, stand_values, 
                                   paired = TRUE, 
                                   exact = FALSE)
      p_value <- wilcox_result$p.value
      
      # Berechnung der Effektstärke r
      z_score <- qnorm(p_value/2) # Umrechnung des p-Werts in z-Score
      N <- length(sit_values) # Stichprobengröße
      effect_size_r <- abs(z_score)/sqrt(N) # Effektstärke r
      
      # Interpretation der Effektstärke
      effect_interpretation <- case_when(
        effect_size_r < 0.1 ~ "vernachlässigbar",
        effect_size_r < 0.3 ~ "klein",
        effect_size_r < 0.5 ~ "mittel",
        TRUE ~ "groß"
      )
      
      test_message <- paste0(test_message,
                             "\n--- Wilcoxon Test Ergebnisse ---\n",
                             sprintf("V = %.3f\n", wilcox_result$statistic),
                             sprintf("p-Wert = %.4f\n", p_value),
                             sprintf("Effektstärke r = %.3f (%s)\n", 
                                     effect_size_r, effect_interpretation),
                             "\nVerwendeter Test: Nicht-parametrischer Test (Wilcoxon)\n")
    }
    
    
    cat(test_message)  # Print die detaillierten Testergebnisse
    
    # Sternchen basierend auf p-Wert
    stars <- if(p_value < 0.001) "***" else if(p_value < 0.01) "**" else if(p_value < 0.05) "*" else ""
    
    # Format basierend auf Variablentyp
    if(grepl("Wirk", var)) {
      result <- sprintf("%.2f ± %.2f%s", 
                        100 * mean(data_sit[[var]], na.rm=TRUE),
                        100 * sd(data_sit[[var]], na.rm=TRUE),
                        stars)
    } else if(var == "ΔBLC [mmol·l⁻¹]") {
      result <- sprintf("%.2f ± %.2f%s",
                        mean(data_sit[[var]], na.rm=TRUE),
                        sd(data_sit[[var]], na.rm=TRUE),
                        stars)
    } else {
      result <- sprintf("%.1f ± %.1f%s",
                        mean(data_sit[[var]], na.rm=TRUE),
                        sd(data_sit[[var]], na.rm=TRUE),
                        stars)
    }
    
    return(as.character(result))
    
  } else if(var == "Wirk_Arbeit [%]") {
    if(check_all_na(data_sit[[var]])) {
      return("-")
    }
    sprintf("%.2f ± %.2f", 
            100 * mean(data_sit[[var]], na.rm=TRUE),
            100 * sd(data_sit[[var]], na.rm=TRUE))
  }
})


# Berechnung für stehen
stats$`Stehen (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_stand <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "stehen")
  
  # Hilfsfunktion zur Überprüfung auf nur NA-Werte
  check_all_na <- function(x) all(is.na(x))
  
  if(var %in% c("Wirk_muskulär [%]", "Wirk_Arbeit [%]","Wirk_Brutto [%]", "Wirk_Netto [%]")) {
    if(check_all_na(data_stand[[var]])) {
      return("-")
    }
    sprintf("%.2f ± %.2f", 
            100 * mean(data_stand[[var]], na.rm=TRUE),
            100 * sd(data_stand[[var]], na.rm=TRUE))
  } else if(var %in% c("W_Aerob [kJ]", "W_PCR [kJ]", "W_BLC [kJ]", "W_TOT [kJ]",
                       "P_Tot [W]", "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]")) {
    if(check_all_na(data_stand[[var]])) {
      return("-")
    }
    sprintf("%.1f ± %.1f",
            mean(data_stand[[var]], na.rm=TRUE),
            sd(data_stand[[var]], na.rm=TRUE))
  } else if(var == "ΔBLC [mmol·l⁻¹]") {
    if(check_all_na(data_stand[[var]])) {
      return("-")
    }
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
    i = ~ Parameter == "Wirk_muskulär [%]",
    value = as_paragraph("η", as_sub("muskulär"), " [%]", as_sup(" a, †"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Wirk_Arbeit [%]",
    value = as_paragraph("η", as_sub("Arbeit,sitzen"), " [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Wirk_Netto [%]",
    value = as_paragraph("η", as_sub("Netto"), " [%]", as_sup(" b"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Wirk_Brutto [%]",
    value = as_paragraph("η", as_sub("Brutto"), " [%]", as_sup(" b"))
  ) %>% 
  compose(
    j = "Parameter",
    i = ~ Parameter == "W_Aerob [kJ]",
    value = as_paragraph("W", as_sub("Aerob"), " [kJ]", as_sup(" a"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "W_PCR [kJ]",
    value = as_paragraph("W", as_sub("PCr"), " [kJ]", as_sup(" a"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "W_BLC [kJ]",
    value = as_paragraph("W", as_sub("BLC"), " [kJ]", as_sup(" a"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "W_TOT [kJ]",
    value = as_paragraph("W", as_sub("TOT"), " [kJ]", as_sup(" a"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Tot [W]",
    value = as_paragraph("P", as_sub("Tot"), " [W]", as_sup(" a, †"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech [W]",
    value = as_paragraph("P", as_sub("mech"), " [W]", as_sup(" a, †"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Int [W]",
    value = as_paragraph("P", as_sub("Int"), " [W]", as_sup(" b, †"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Drehzahl [U·min⁻¹]",
    value = as_paragraph("Drehzahl [U·min⁻¹]", as_sup(" b, †"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "ΔBLC [mmol·l⁻¹]",
    value = as_paragraph("ΔBLC [mmol·l⁻¹]", as_sup(" b"))
  ) %>%
  theme_zebra(odd_header = "grey92", 
              even_header = "#EFEFEF", 
              odd_body = "#F9F9F9", 
              even_body = "#FFFFFF") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  bold(i = ~ Parameter == "Wirk_muskulär [%]") %>%
  add_footer_row(
    values = as_paragraph(
      as_sup("a"), " : Parametrische Analyse mittels einfaktorieller Varianzanalyse (ANOVA) mit Messwiederholung.\n ",
      as_sup("b"), " : Nicht-parametrische Analyse mittels Wilcoxon-Test.\n ",
      as_sup("†"), " : Signifikanter Haupteffekt der Bedingung; ",
      "Signifikanzniveaus: *** p < 0.001; ** p < 0.01; * p < 0.05\n",
      "η", as_sub("muskulär"), " [%]: muskulärer Wirkungsgrad; ",
      "η", as_sub("Arbeit,sitzen"), " [%]: Arbeitswirkungsgrad (sitzende Position); ",
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
  Variable = c("Wirk_muskulär [%]","Wirk_Arbeit [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]", 
               "W_TOT [kJ]", "W_Aerob [kJ]", "W_PCR [kJ]", 
               "W_BLC [kJ]", "P_Tot [W]",
               "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]", "ΔBLC [mmol·l⁻¹]")
)

# Post-hoc Ergebnisse erstellen
posthoc_ergebnisse <- NULL

# Berechnung für leicht und ANOVAs
stats$`Leicht (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_light <- filter(Bedingungen_data_Wirkungsgrad, Intensität == "leicht")
  data_moderate <- filter(Bedingungen_data_Wirkungsgrad, Intensität == "moderat")
  data_heavy <- filter(Bedingungen_data_Wirkungsgrad, Intensität == "schwer")
  
  if(var %in% c("Wirk_muskulär [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]")) {  
    # Temporäre Datenkopie erstellen
    temp_data <- Bedingungen_data_Wirkungsgrad
    
    # Variable dynamisch zuweisen basierend auf var
    if(var == "Wirk_muskulär [%]") {
      temp_data$dv <- temp_data$`Wirk_muskulär [%]` * 100
      current_data_light <- data_light$`Wirk_muskulär [%]`
      current_data_moderate <- data_moderate$`Wirk_muskulär [%]`
      current_data_heavy <- data_heavy$`Wirk_muskulär [%]`
    } else if(var == "Wirk_Netto [%]") {
      temp_data$dv <- temp_data$`Wirk_Netto [%]` * 100
      current_data_light <- data_light$`Wirk_Netto [%]`
      current_data_moderate <- data_moderate$`Wirk_Netto [%]`
      current_data_heavy <- data_heavy$`Wirk_Netto [%]`
    } else if(var == "Wirk_Brutto [%]") {
      temp_data$dv <- temp_data$`Wirk_Brutto [%]` * 100
      current_data_light <- data_light$`Wirk_Brutto [%]`
      current_data_moderate <- data_moderate$`Wirk_Brutto [%]`
      current_data_heavy <- data_heavy$`Wirk_Brutto [%]`
    }
    
    # Berechnung der gepoolten Standardabweichung und Cohen's d
    sd_light <- sd(current_data_light)
    sd_moderate <- sd(current_data_moderate)
    sd_heavy <- sd(current_data_heavy)
    n <- length(current_data_light)
    
    pooled_sd <- sqrt(((n-1)*(sd_light^2 + sd_moderate^2 + sd_heavy^2))/(3*(n-1)))
    
    cohens_d_light_mod <- abs(mean(current_data_moderate) - mean(current_data_light)) / pooled_sd
    cohens_d_light_heavy <- abs(mean(current_data_heavy) - mean(current_data_light)) / pooled_sd
    cohens_d_mod_heavy <- abs(mean(current_data_heavy) - mean(current_data_moderate)) / pooled_sd
    
    # Shapiro-Wilk Tests
    shapiro_light <- shapiro.test(current_data_light)
    shapiro_moderate <- shapiro.test(current_data_moderate)
    shapiro_heavy <- shapiro.test(current_data_heavy)
    
    # Interpretation der Shapiro-Wilk Tests
    shapiro_interpret_light <- ifelse(shapiro_light$p.value > 0.05, "normalverteilt", "nicht normalverteilt")
    shapiro_interpret_moderate <- ifelse(shapiro_moderate$p.value > 0.05, "normalverteilt", "nicht normalverteilt")
    shapiro_interpret_heavy <- ifelse(shapiro_heavy$p.value > 0.05, "normalverteilt", "nicht normalverteilt")
    
    normal_dist <- all(c(shapiro_light$p.value, shapiro_moderate$p.value, shapiro_heavy$p.value) > 0.05)
    
    # Levene Test
    temp_data$Intensität <- as.factor(temp_data$Intensität)
    levene_result <- car::leveneTest(temp_data$dv ~ temp_data$Intensität)
    levene_interpret <- ifelse(levene_result$`Pr(>F)`[1] > 0.05, "homogen", "nicht homogen")
    var_homogen <- levene_result$`Pr(>F)`[1] > 0.05
    
    # Testauswahl und Durchführung
    test_message <- sprintf("\nVariable: %s\nNormalverteilung: %s\nVarianzhomogenität: %s\n",
                            var,
                            ifelse(normal_dist, "ja", "nein"),
                            ifelse(var_homogen, "ja", "nein"))
    
    if(normal_dist && var_homogen) {
      # Parametrischer Test (ezANOVA)
      ez_result <- ezANOVA(
        data = temp_data,
        dv = dv,
        wid = Proband,
        within = Intensität,
        detailed = TRUE
      )
      p_value <- ez_result$ANOVA$p[2]
      
      # Berechnung des partiellen Eta-Quadrat
      f_value <- ez_result$ANOVA$F[2]
      df_effect <- ez_result$ANOVA$DFn[2]
      df_error <- ez_result$ANOVA$DFd[2]
      partial_eta_sq <- (f_value * df_effect) / (f_value * df_effect + df_error)
      
      cat(test_message, "Verwendeter Test: Parametrischer Test (ezANOVA)\n")
      
      if(var == "Wirk_muskulär [%]") {
        aov_model <- aov(dv ~ Intensität + Error(Proband/Intensität), data = temp_data)
        post_hoc <- TukeyHSD(aov(dv ~ Intensität, data = temp_data))
        
        # Cohen's d zu den Post-hoc Ergebnissen hinzufügen
        post_hoc_matrix <- as.matrix(post_hoc$Intensität)
        post_hoc_matrix <- cbind(post_hoc_matrix, 
                                 "Cohen's d" = c(cohens_d_light_mod, 
                                                 cohens_d_light_heavy, 
                                                 cohens_d_mod_heavy))
        
        posthoc_ergebnisse <<- list(Intensität = post_hoc_matrix)
        
        # Post-hoc p-Werte extrahieren
        p_leicht_moderat <- post_hoc$Intensität["moderat-leicht", "p adj"]
        p_leicht_schwer <- post_hoc$Intensität["schwer-leicht", "p adj"]
        p_moderat_schwer <- post_hoc$Intensität["schwer-moderat", "p adj"]
      }
      
      # ANOVA Statistiken
      f_value <- ez_result$ANOVA$F[2]
      df_effect <- ez_result$ANOVA$DFn[2]
      df_error <- ez_result$ANOVA$DFd[2]
      ges <- ez_result$ANOVA$ges[2]
      
    } else {
      # Nicht-parametrischer Test (Friedman)
      friedman_result <- friedman.test(temp_data$dv, temp_data$Intensität, temp_data$Proband)
      p_value <- friedman_result$p.value
      cat(test_message, "Verwendeter Test: Nicht-parametrischer Test (Friedman)\n")
      
      # Friedman Statistiken
      f_value <- friedman_result$statistic
      df_effect <- friedman_result$parameter["df"]
      df_error <- NA
      ges <- NA
      
      if(var == "Wirk_muskulär [%]") {
        post_hoc <- pairwise.wilcox.test(temp_data$dv, temp_data$Intensität, 
                                         p.adjust.method = "bonferroni", paired = TRUE)
        
        # Cohen's d Werte zu den Wilcoxon Ergebnissen hinzufügen
        wilcox_matrix <- matrix(c(NA, post_hoc$p.value[1,1], post_hoc$p.value[2,1],
                                  NA, NA, post_hoc$p.value[2,2],
                                  NA, NA, NA),
                                nrow = 3, ncol = 3)
        
        cohens_d_matrix <- matrix(c(NA, cohens_d_light_mod, cohens_d_light_heavy,
                                    NA, NA, cohens_d_mod_heavy,
                                    NA, NA, NA),
                                  nrow = 3, ncol = 3)
        
        posthoc_ergebnisse <<- list(p.value = wilcox_matrix,
                                    "Cohen's d" = cohens_d_matrix)
      }
    }
    
    # Ausgabe der Ergebnisse für die aktuelle Variable
    print(paste("\n=== Ergebnisse für", var, "==="))
    
    # Deskriptive Statistik
    print("Deskriptive Statistik (in %):")
    print(sprintf("Gesamtmittelwert: %.2f%%", mean(temp_data$dv)))
    print("\nMittelwerte nach Intensität:")
    aggregate(dv ~ Intensität, data = temp_data, 
              FUN = function(x) c(mean = mean(x), sd = sd(x))) %>% print()
    
    # Cohen's d Effektstärken
    print("\n=== Cohen's d Effektstärken ===")
    print(sprintf("Leicht vs. Moderat: d = %.3f", cohens_d_light_mod))
    print(sprintf("Leicht vs. Schwer: d = %.3f", cohens_d_light_heavy))
    print(sprintf("Moderat vs. Schwer: d = %.3f", cohens_d_mod_heavy))
    
    # Test Ergebnisse
    if(normal_dist && var_homogen) {
      print("\n--- ANOVA Ergebnisse ---")
      print(sprintf("F(%d,%d) = %.3f", df_effect, df_error, f_value))
    } else {
      print("\n--- Friedman Test Ergebnisse ---")
      print(sprintf("Chi-Quadrat(%d) = %.3f", df_effect, f_value))
    }
    print(sprintf("p-Wert = %.4f", p_value))
    if(!is.na(partial_eta_sq)) print(sprintf("Partielles Eta-Quadrat (η²p) = %.3f", partial_eta_sq))
    
    if(var == "Wirk_muskulär [%]") {
      print("\n--- Post-hoc Analyse ---")
      print("Paarweise Vergleiche:")
      print(posthoc_ergebnisse)
    }
    
    print("\nNormalverteilung:")
    print(sprintf("Leicht: W = %.3f, p = %.4f (%s)", 
                  shapiro_light$statistic, shapiro_light$p.value, shapiro_interpret_light))
    print(sprintf("Moderat: W = %.3f, p = %.4f (%s)", 
                  shapiro_moderate$statistic, shapiro_moderate$p.value, shapiro_interpret_moderate))
    print(sprintf("Schwer: W = %.3f, p = %.4f (%s)", 
                  shapiro_heavy$statistic, shapiro_heavy$p.value, shapiro_interpret_heavy))
    
    print("\nVarianzhomogenität:")
    print(sprintf("Levene's Test: F = %.3f, p = %.4f (%s)", 
                  levene_result$`F value`[1], levene_result$`Pr(>F)`[1], levene_interpret))
    
    # Ergebnis für leichte Intensität ohne Signifikanzsternchen
    result <- sprintf("%.2f ± %.2f", 
                      100 * mean(current_data_light, na.rm=TRUE),
                      100 * sd(current_data_light, na.rm=TRUE))
    
    return(as.character(result))
    
  } else if(var %in% c("Wirk_Arbeit [%]")) {
    sprintf("%.2f ± %.2f", 
            100 * mean(data_light[[var]], na.rm=TRUE),
            100 * sd(data_light[[var]], na.rm=TRUE))
  } else if(var %in% c("W_Aerob [kJ]", "W_PCR [kJ]", "W_BLC [kJ]", "W_TOT [kJ]",
                       "P_Tot [W]", "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]")) {
    sprintf("%.1f ± %.1f",
            mean(data_light[[var]], na.rm=TRUE),
            sd(data_light[[var]], na.rm=TRUE))
  } else if(var == "ΔBLC [mmol·l⁻¹]") {
    sprintf("%.2f ± %.2f",
            mean(data_light[[var]], na.rm=TRUE),
            sd(data_light[[var]], na.rm=TRUE))
  }
})


# Berechnung für moderat
stats$`Moderat (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_moderate <- filter(Bedingungen_data_Wirkungsgrad, Intensität == "moderat")
  
  if(var %in% c("Wirk_muskulär [%]", "Wirk_Arbeit [%]","Wirk_Brutto [%]", "Wirk_Netto [%]")) {
    sprintf("%.2f ± %.2f", 
            100 * mean(data_moderate[[var]], na.rm=TRUE),
            100 * sd(data_moderate[[var]], na.rm=TRUE))
  } else if(var %in% c("W_Aerob [kJ]", "W_PCR [kJ]", "W_BLC [kJ]", "W_TOT [kJ]",
                       "P_Tot [W]", "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]")) {
    sprintf("%.1f ± %.1f",
            mean(data_moderate[[var]], na.rm=TRUE),
            sd(data_moderate[[var]], na.rm=TRUE))
  } else if(var == "ΔBLC [mmol·l⁻¹]") {
    sprintf("%.2f ± %.2f",
            mean(data_moderate[[var]], na.rm=TRUE),
            sd(data_moderate[[var]], na.rm=TRUE))
  }
})

# Berechnung für schwer
stats$`Schwer (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_heavy <- filter(Bedingungen_data_Wirkungsgrad, Intensität == "schwer")
  
  if(var %in% c("Wirk_muskulär [%]", "Wirk_Arbeit [%]","Wirk_Brutto [%]", "Wirk_Netto [%]")) {
    sprintf("%.2f ± %.2f", 
            100 * mean(data_heavy[[var]], na.rm=TRUE),
            100 * sd(data_heavy[[var]], na.rm=TRUE))
  } else if(var %in% c("W_Aerob [kJ]", "W_PCR [kJ]", "W_BLC [kJ]", "W_TOT [kJ]",
                       "P_Tot [W]", "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]")) {
    sprintf("%.1f ± %.1f",
            mean(data_heavy[[var]], na.rm=TRUE),
            sd(data_heavy[[var]], na.rm=TRUE))
  } else if(var == "ΔBLC [mmol·l⁻¹]") {
    sprintf("%.2f ± %.2f",
            mean(data_heavy[[var]], na.rm=TRUE),
            sd(data_heavy[[var]], na.rm=TRUE))
  }
})

### Wirk_Arbeit - Statistische Tests ###
temp_data <- Bedingungen_data_Wirkungsgrad
temp_data$dv <- temp_data$`Wirk_Arbeit [%]` * 100
temp_data <- temp_data[complete.cases(temp_data$dv, temp_data$Proband, temp_data$Intensität), ]

# ANOVA
ez_result <- ezANOVA(
  data = temp_data,
  dv = dv,
  wid = Proband,
  within = Intensität,
  detailed = TRUE
)

# Partielles Eta-Quadrat berechnen
f_value <- ez_result$ANOVA$F[2]
df_effect <- ez_result$ANOVA$DFn[2]
df_error <- ez_result$ANOVA$DFd[2]
partial_eta_sq <- (f_value * df_effect) / (f_value * df_effect + df_error)

print("\n--- ANOVA Ergebnisse ---")
print(sprintf("F(%d,%d) = %.3f, p = %.4f, η²p = %.3f", 
              df_effect, df_error, f_value, ez_result$ANOVA$p[2], partial_eta_sq))

# Post-hoc Tests mit Cohens d
leicht_data <- temp_data$dv[temp_data$Intensität == "leicht"]
moderat_data <- temp_data$dv[temp_data$Intensität == "moderat"]
schwer_data <- temp_data$dv[temp_data$Intensität == "schwer"]

# Cohens d Funktion für gepaarte Daten
cohens_d <- function(x, y) {
  diff <- x - y
  d <- mean(diff, na.rm = TRUE) / sd(diff, na.rm = TRUE)
  return(d)
}

# Paarweise t-Tests durchführen
t_ml <- t.test(moderat_data, leicht_data, paired = TRUE)
t_sl <- t.test(schwer_data, leicht_data, paired = TRUE)
t_sm <- t.test(schwer_data, moderat_data, paired = TRUE)

# Posthoc Ergebnisse speichern
posthoc_ergebnisse_Wirk_Arbeit <- data.frame(
  diff = c(mean(moderat_data - leicht_data),
           mean(schwer_data - leicht_data),
           mean(schwer_data - moderat_data)),
  lwr = c(t_ml$conf.int[1], t_sl$conf.int[1], t_sm$conf.int[1]),
  upr = c(t_ml$conf.int[2], t_sl$conf.int[2], t_sm$conf.int[2]),
  p_adj = c(t_ml$p.value, t_sl$p.value, t_sm$p.value) * 3,
  `Cohen's d` = c(
    cohens_d(moderat_data, leicht_data),
    cohens_d(schwer_data, leicht_data),
    cohens_d(schwer_data, moderat_data)
  ),
  row.names = c("moderat-leicht", "schwer-leicht", "schwer-moderat")
)

# Paarweise Tests und Effektstärken
print("\n--- Post-hoc Tests (Bonferroni) mit Cohens d ---")
print("schwer vs. leicht:")
print(sprintf("p = %.4f, d = %.3f", 
              t.test(schwer_data, leicht_data, paired = TRUE)$p.value * 3,
              cohens_d(schwer_data, leicht_data)))

print("moderat vs. leicht:")
print(sprintf("p = %.4f, d = %.3f", 
              t.test(moderat_data, leicht_data, paired = TRUE)$p.value * 3,
              cohens_d(moderat_data, leicht_data)))

print("schwer vs. moderat:")
print(sprintf("p = %.4f, d = %.3f", 
              t.test(schwer_data, moderat_data, paired = TRUE)$p.value * 3,
              cohens_d(schwer_data, moderat_data)))
#######################################

# Namen der Spalten
names(stats) <- c("Parameter", "Leicht (MW ± SD)", "Moderat (MW ± SD)", "Schwer (MW ± SD)")

# Tabelle erstellen
ft_Wirkungsgrad_stats_Intensitaet_mean <- flextable(stats) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Wirk_muskulär [%]",
    value = as_paragraph("η", as_sub("muskulär"), " [%]", as_sup(" a, †"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Wirk_Arbeit [%]",
    value = as_paragraph("η", as_sub("Arbeit,sitzen"), " [%]", as_sup(" a, †"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Wirk_Netto [%]",
    value = as_paragraph("η", as_sub("Netto"), " [%]", as_sup(" a"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Wirk_Brutto [%]",
    value = as_paragraph("η", as_sub("Brutto"), " [%]", as_sup(" a"))
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
  bold(i = ~ Parameter == "Wirk_muskulär [%]") %>%
  add_footer_row(
    values = as_paragraph(
      as_sup("a"), " : Parametrische Analyse mittels einfaktorieller Varianzanalyse (ANOVA) mit Messwiederholung.\n ",
      as_sup("†"), " : Signifikanter Haupteffekt der Intensität (p < .05). \n",
      "η", as_sub("muskulär"), " [%]: muskulärer Wirkungsgrad; ",
      "η", as_sub("Arbeit,sitzen"), " [%]: Arbeitswirkungsgrad (sitzende Position); ",
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

##### Post-hoc für Intensitäten und Wirk_muskulär #####
# Post-hoc Daten mit Cohen's d
post_hoc_data <- data.frame(
  Vergleich = rownames(posthoc_ergebnisse$Intensität),
  Differenz = posthoc_ergebnisse$Intensität[, "diff"],
  KI_unten = posthoc_ergebnisse$Intensität[, "lwr"],
  KI_oben = posthoc_ergebnisse$Intensität[, "upr"],
  p_Wert = posthoc_ergebnisse$Intensität[, "p adj"],
  Cohens_d = posthoc_ergebnisse$Intensität[, "Cohen's d"]
)

# Neuordnung und Formatierung
post_hoc_data <- post_hoc_data[c(2, 1, 3), ]
post_hoc_data$Vergleich <- gsub("moderat", "Moderat", post_hoc_data$Vergleich)
post_hoc_data$Vergleich <- gsub("leicht", "Leicht", post_hoc_data$Vergleich)
post_hoc_data$Vergleich <- gsub("schwer", "Schwer", post_hoc_data$Vergleich)

# Funktion zum Hinzufügen der Signifikanzsymbole
add_significance_symbols <- function(p_value) {
  if (p_value < 0.001) return("***")
  else if (p_value < 0.01) return("**")
  else if (p_value < 0.05) return("*")
  else return("")
}

# Signifikanzsymbole hinzufügen
post_hoc_data$p_Wert <- paste0(format(round(post_hoc_data$p_Wert, 3), nsmall = 3),
                               sapply(post_hoc_data$p_Wert, add_significance_symbols))

# Direkte Ausgabe der Post-hoc-Ergebnisse
cat("\nErgebnisse des Post-hoc-Tests:\n")
cat("----------------------------\n")

for(i in 1:nrow(post_hoc_data)) {
  p_val <- as.numeric(sub("[*]+$", "", post_hoc_data$p_Wert[i]))
  sig_level <- if(p_val < 0.05) "signifikant" else "nicht signifikant"
  
  cat(sprintf("%s:\n", post_hoc_data$Vergleich[i]))
  cat(sprintf("  Mittlere Differenz: %.2f\n", post_hoc_data$Differenz[i]))
  cat(sprintf("  p-Wert: %s (%s)\n", post_hoc_data$p_Wert[i], sig_level))
  cat("----------------------------\n")
}


# Erstellen der Flextable
ft_post_hoc <- flextable(post_hoc_data) %>%
  set_header_labels(
    Vergleich = " ",
    Differenz = "Mittlere Differenz",
    KI_unten = "95% KI Untergrenze",
    KI_oben = "95% KI Obergrenze",
    p_Wert = "Signifikanzniveau",
    Cohens_d = "Cohen's d"  # Neue Spaltenüberschrift
  ) %>%
  compose(
    part = "header",  
    i = 1, j = "Vergleich",
    value = as_paragraph("Intensitäten: Post-hoc Vergleich von ", "η", as_sub("muskulär"))
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
      "Signifikanzniveaus: *** p < 0.001; ** p < 0.01; * p < 0.05"
    ),
    colwidths = 6
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
  hline(i = 1, part = "header", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  hline_top(part = "footer", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  hline_bottom(part = "footer", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  colformat_double(
    j = c("Differenz", "KI_unten", "KI_oben", "Cohens_d"),  # Cohens_d zur Formatierung hinzufügen
    digits = 2
  )

# Setzen der Tabelle auf volle Breite
ft_post_hoc <- set_table_properties(
  ft_post_hoc, 
  width = 1, 
  layout = "autofit"
)

# Anzeigen der Tabelle
ft_post_hoc

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_post_hoc, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_post_hoc.rds")
ft_post_hoc <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_post_hoc.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_post_hoc, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_post_hoc.rds")
ft_post_hoc <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_post_hoc.rds")



##################################################################################
##### Post-hoc für Intensitäten und Wirk_Arbeit #####
# Post-hoc Daten mit Cohen's d
post_hoc_data_Wirk_Arbeit <- data.frame(
  Vergleich = rownames(posthoc_ergebnisse_Wirk_Arbeit),
  Differenz = posthoc_ergebnisse_Wirk_Arbeit$diff,
  KI_unten = posthoc_ergebnisse_Wirk_Arbeit$lwr,
  KI_oben = posthoc_ergebnisse_Wirk_Arbeit$upr,
  p_Wert = posthoc_ergebnisse_Wirk_Arbeit$p_adj,
  Cohens_d = posthoc_ergebnisse_Wirk_Arbeit$Cohen.s.d
)

# Neuordnung und Formatierung
post_hoc_data_Wirk_Arbeit <- post_hoc_data_Wirk_Arbeit[c(2, 1, 3), ]
post_hoc_data_Wirk_Arbeit$Vergleich <- gsub("moderat", "Moderat", post_hoc_data_Wirk_Arbeit$Vergleich)
post_hoc_data_Wirk_Arbeit$Vergleich <- gsub("leicht", "Leicht", post_hoc_data_Wirk_Arbeit$Vergleich)
post_hoc_data_Wirk_Arbeit$Vergleich <- gsub("schwer", "Schwer", post_hoc_data_Wirk_Arbeit$Vergleich)

# Funktion zum Hinzufügen der Signifikanzsymbole
add_significance_symbols <- function(p_value) {
  if (p_value < 0.001) return("***")
  else if (p_value < 0.01) return("**")
  else if (p_value < 0.05) return("*")
  else return("")
}

# Signifikanzsymbole hinzufügen
post_hoc_data_Wirk_Arbeit$p_Wert <- paste0(format(round(post_hoc_data_Wirk_Arbeit$p_Wert, 3), nsmall = 3),
                               sapply(post_hoc_data_Wirk_Arbeit$p_Wert, add_significance_symbols))

# Direkte Ausgabe der Post-hoc-Ergebnisse
cat("\nErgebnisse des Post-hoc-Tests:\n")
cat("----------------------------\n")

for(i in 1:nrow(post_hoc_data_Wirk_Arbeit)) {
  p_val <- as.numeric(sub("[*]+$", "", post_hoc_data_Wirk_Arbeit$p_Wert[i]))
  sig_level <- if(p_val < 0.05) "signifikant" else "nicht signifikant"
  
  cat(sprintf("%s:\n", post_hoc_data_Wirk_Arbeit$Vergleich[i]))
  cat(sprintf("  Mittlere Differenz: %.2f\n", post_hoc_data_Wirk_Arbeit$Differenz[i]))
  cat(sprintf("  p-Wert: %s (%s)\n", post_hoc_data_Wirk_Arbeit$p_Wert[i], sig_level))
  cat("----------------------------\n")
}


# Erstellen der Flextable
ft_post_hoc_Wirk_Arbeit <- flextable(post_hoc_data_Wirk_Arbeit) %>%
  set_header_labels(
    Vergleich = " ",
    Differenz = "Mittlere Differenz",
    KI_unten = "95% KI Untergrenze",
    KI_oben = "95% KI Obergrenze",
    p_Wert = "Signifikanzniveau",
    Cohens_d = "Cohen's d"  # Neue Spaltenüberschrift
  ) %>%
  compose(
    part = "header",  
    i = 1, j = "Vergleich",
    value = as_paragraph("Intensitäten: Post-hoc Vergleich von ", "η", as_sub("Arbeit,sitzen"))
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
      "Signifikanzniveaus: *** p < 0.001; ** p < 0.01; * p < 0.05"
    ),
    colwidths = 6
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
  hline(i = 1, part = "header", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  hline_top(part = "footer", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  hline_bottom(part = "footer", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  colformat_double(
    j = c("Differenz", "KI_unten", "KI_oben", "Cohens_d"),  # Cohens_d zur Formatierung hinzufügen
    digits = 2
  )

# Setzen der Tabelle auf volle Breite
ft_post_hoc_Wirk_Arbeit <- set_table_properties(
  ft_post_hoc_Wirk_Arbeit, 
  width = 1, 
  layout = "autofit"
)

# Anzeigen der Tabelle
ft_post_hoc_Wirk_Arbeit

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_post_hoc_Wirk_Arbeit, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_post_hoc_Wirk_Arbeit.rds")
ft_post_hoc_Wirk_Arbeit <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_post_hoc_Wirk_Arbeit.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_post_hoc_Wirk_Arbeit, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_post_hoc_Wirk_Arbeit.rds")
ft_post_hoc_Wirk_Arbeit <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_post_hoc_Wirk_Arbeit.rds")


#################################################################################
# Berechnung der Statistiken
stats <- data.frame(
  Variable = c("Wirk_muskulär [%]","Wirk_Arbeit [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]", 
               "W_TOT [kJ]", "W_Aerob [kJ]", "W_PCR [kJ]", 
               "W_BLC [kJ]", "P_Tot [W]",
               "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]", "ΔBLC [mmol·l⁻¹]")
)

# Post-hoc Ergebnisse erstellen
posthoc_ergebnisse_sitzen <- NULL

# Funktion zur Berechnung von Cohen's d
calculate_cohens_d <- function(group1, group2) {
  # Pooled standard deviation
  n1 <- length(group1)
  n2 <- length(group2)
  s1 <- sd(group1)
  s2 <- sd(group2)
  pooled_sd <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2))
  
  # Cohen's d
  d <- (mean(group1) - mean(group2)) / pooled_sd
  return(d)
}

# Sitzen Leicht
stats$`Sitzen Leicht (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_sitzen_light <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "sitzen", Intensität == "leicht")
  data_sitzen_moderate <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "sitzen", Intensität == "moderat")
  data_sitzen_heavy <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "sitzen", Intensität == "schwer")
  
  if(var %in% c("Wirk_muskulär [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]")) {  
    # Temporäre Datenkopie erstellen
    temp_data <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "sitzen")
    temp_data$dv <- temp_data[[var]] * 100
    
    # Shapiro-Wilk Tests
    shapiro_light <- shapiro.test(filter(temp_data, Intensität == "leicht")$dv)
    shapiro_moderate <- shapiro.test(filter(temp_data, Intensität == "moderat")$dv)
    shapiro_heavy <- shapiro.test(filter(temp_data, Intensität == "schwer")$dv)
    
    # ANOVA für Sitzen bleibt gleich
    ez_result_sitzen <- ezANOVA(
      data = temp_data,
      dv = dv,
      wid = Proband,
      within = Intensität,
      detailed = TRUE
    )
    
    # Berechnung des partiellen Eta-Quadrats
    SSeffect <- ez_result_sitzen$ANOVA$SSn[2]
    SSerror <- ez_result_sitzen$ANOVA$SSd[2]
    partial_eta_squared <- SSeffect / (SSeffect + SSerror)
    
    # Vergleich Sitzen vs. Stehen für leichte Intensität
    data_stehen_light <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "stehen", Intensität == "leicht")
    ttest_light <- t.test(
      filter(temp_data, Intensität == "leicht")$dv,
      filter(Bedingungen_data_Wirkungsgrad, Bedingung == "stehen", Intensität == "leicht")[[var]] * 100,
      paired = TRUE
    )
    
    result <- sprintf("%.2f ± %.2f", 
                      100 * mean(data_sitzen_light[[var]], na.rm=TRUE),
                      100 * sd(data_sitzen_light[[var]], na.rm=TRUE))
    
    # Post-hoc Test für Sitzen durchführen
    aov_model_sitzen <- aov(dv ~ Intensität + Error(Proband/Intensität), data = temp_data)
    post_hoc_sitzen <- TukeyHSD(aov(dv ~ Intensität, data = temp_data))
    
    # Post-hoc Ergebnisse und Cohen's d für Wirk_muskulär und Wirk_Netto speichern
    if(var %in% c("Wirk_muskulär [%]", "Wirk_Netto [%]")) {
      # Cohen's d Berechnung für alle Paarvergleiche
      cohens_d <- data.frame(
        comparison = rownames(post_hoc_sitzen$Intensität),
        d = NA
      )
      
      # Daten für verschiedene Intensitäten
      data_light <- filter(temp_data, Intensität == "leicht")$dv
      data_moderate <- filter(temp_data, Intensität == "moderat")$dv
      data_heavy <- filter(temp_data, Intensität == "schwer")$dv
      
      # Cohen's d Berechnungen
      d_lm <- calculate_cohens_d(data_light, data_moderate)
      d_ls <- calculate_cohens_d(data_light, data_heavy)
      d_ms <- calculate_cohens_d(data_moderate, data_heavy)
      
      cohens_d$d[cohens_d$comparison == "moderat-leicht"] <- d_lm
      cohens_d$d[cohens_d$comparison == "schwer-leicht"] <- d_ls
      cohens_d$d[cohens_d$comparison == "schwer-moderat"] <- d_ms
      
      # Ergebnisse speichern
      posthoc_ergebnisse_sitzen[[var]] <<- list(
        posthoc = post_hoc_sitzen$Intensität,
        cohens_d = cohens_d
      )
    }
    
    # Post-hoc p-Werte extrahieren
    p_sitzen_leicht_moderat <- post_hoc_sitzen$Intensität["moderat-leicht", "p adj"]
    p_sitzen_leicht_schwer <- post_hoc_sitzen$Intensität["schwer-leicht", "p adj"]
    p_sitzen_moderat_schwer <- post_hoc_sitzen$Intensität["schwer-moderat", "p adj"]
    
    # Deskriptive Statistik für Sitzen
    print(sprintf("\nDeskriptive Statistik Sitzen für %s (in %%)", var))
    print(sprintf("Gesamtmittelwert Sitzen: %.2f%%", mean(temp_data$dv)))
    print("\nMittelwerte nach Intensität (Sitzen):")
    aggregate(dv ~ Intensität, data = temp_data, 
              FUN = function(x) c(mean = mean(x), sd = sd(x))) %>% print()
    
    # ANOVA Ergebnisse Sitzen
    f_value_sitzen <- ez_result_sitzen$ANOVA$F[2]
    p_value_sitzen <- ez_result_sitzen$ANOVA$p[2]
    df_effect_sitzen <- ez_result_sitzen$ANOVA$DFn[2]
    df_error_sitzen <- ez_result_sitzen$ANOVA$DFd[2]
    ges_sitzen <- ez_result_sitzen$ANOVA$ges[2]
    
    # Statistische Ausgabe Sitzen
    print(sprintf("\n--- ANOVA Ergebnisse Sitzen für %s ---", var))
    print(sprintf("F(%d,%d) = %.3f", df_effect_sitzen, df_error_sitzen, f_value_sitzen))
    print(sprintf("p-Wert = %.4f", p_value_sitzen))
    print(sprintf("Partielles Eta-Quadrat = %.3f", partial_eta_squared))
    
    print(sprintf("\n--- Post-hoc Analyse Sitzen (Tukey HSD) für %s ---", var))
    print("Paarweise Vergleiche:")
    print(post_hoc_sitzen$Intensität)
    
    if(var %in% c("Wirk_muskulär [%]", "Wirk_Netto [%]")) {
      print("\nCohen's d für Paarvergleiche:")
      print(cohens_d)
    }
    
    print(sprintf("\nNormalverteilung Sitzen für %s:", var))
    print(sprintf("Leicht: W = %.3f, p = %.4f", 
                  shapiro_light$statistic, shapiro_light$p.value))
    print(sprintf("Moderat: W = %.3f, p = %.4f", 
                  shapiro_moderate$statistic, shapiro_moderate$p.value))
    print(sprintf("Schwer: W = %.3f, p = %.4f", 
                  shapiro_heavy$statistic, shapiro_heavy$p.value))
    
    as.character(result)
    
  } else if(var == "Wirk_Arbeit [%]") {
    sprintf("%.2f ± %.2f", 
            100 * mean(data_sitzen_light[[var]], na.rm=TRUE),
            100 * sd(data_sitzen_light[[var]], na.rm=TRUE))
  } else if(var %in% c("W_Aerob [kJ]", "W_PCR [kJ]", "W_BLC [kJ]", "W_TOT [kJ]",
                       "P_Tot [W]", "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]")) {
    sprintf("%.1f ± %.1f",
            mean(data_sitzen_light[[var]], na.rm=TRUE),
            sd(data_sitzen_light[[var]], na.rm=TRUE))
  } else if(var == "ΔBLC [mmol·l⁻¹]") {
    sprintf("%.2f ± %.2f",
            mean(data_sitzen_light[[var]], na.rm=TRUE),
            sd(data_sitzen_light[[var]], na.rm=TRUE))
  }
})

# Sitzen Moderat
stats$`Sitzen Moderat (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_sitzen_moderate <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "sitzen", Intensität == "moderat")
  
  if(var %in% c("Wirk_muskulär [%]")) {
    # Vergleich Sitzen vs. Stehen für moderate Intensität
    data_stehen_moderate <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "stehen", Intensität == "moderat")
    ttest_moderate <- t.test(data_sitzen_moderate$`Wirk_muskulär [%]`, 
                             data_stehen_moderate$`Wirk_muskulär [%]`, 
                             paired = TRUE)
    
    result <- sprintf("%.2f ± %.2f", 
                      100 * mean(data_sitzen_moderate[[var]], na.rm=TRUE),
                      100 * sd(data_sitzen_moderate[[var]], na.rm=TRUE))
    
    if(ttest_moderate$p.value < 0.05) result <- paste0(result, "*")
    
    as.character(result)
    
  } else if(var %in% c("Wirk_Arbeit [%]", "Wirk_Brutto [%]", "Wirk_Netto [%]")) {
    sprintf("%.2f ± %.2f", 
            100 * mean(data_sitzen_moderate[[var]], na.rm=TRUE),
            100 * sd(data_sitzen_moderate[[var]], na.rm=TRUE))
  } else if(var %in% c("W_Aerob [kJ]", "W_PCR [kJ]", "W_BLC [kJ]", "W_TOT [kJ]",
                       "P_Tot [W]", "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]")) {
    sprintf("%.1f ± %.1f",
            mean(data_sitzen_moderate[[var]], na.rm=TRUE),
            sd(data_sitzen_moderate[[var]], na.rm=TRUE))
  } else if(var == "ΔBLC [mmol·l⁻¹]") {
    sprintf("%.2f ± %.2f",
            mean(data_sitzen_moderate[[var]], na.rm=TRUE),
            sd(data_sitzen_moderate[[var]], na.rm=TRUE))
  }
})

# Sitzen Schwer
stats$`Sitzen Schwer (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_sitzen_heavy <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "sitzen", Intensität == "schwer")
  
  if(var %in% c("Wirk_muskulär [%]")) {
    # Vergleich Sitzen vs. Stehen für schwere Intensität
    data_stehen_heavy <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "stehen", Intensität == "schwer")
    ttest_heavy <- t.test(data_sitzen_heavy$`Wirk_muskulär [%]`, 
                          data_stehen_heavy$`Wirk_muskulär [%]`, 
                          paired = TRUE)
    
    result <- sprintf("%.2f ± %.2f", 
                      100 * mean(data_sitzen_heavy[[var]], na.rm=TRUE),
                      100 * sd(data_sitzen_heavy[[var]], na.rm=TRUE))
    
    if(ttest_heavy$p.value < 0.05) result <- paste0(result, "*")
    
    as.character(result)
    
  } else if(var %in% c("Wirk_Arbeit [%]", "Wirk_Brutto [%]", "Wirk_Netto [%]")) {
    sprintf("%.2f ± %.2f", 
            100 * mean(data_sitzen_heavy[[var]], na.rm=TRUE),
            100 * sd(data_sitzen_heavy[[var]], na.rm=TRUE))
  } else if(var %in% c("W_Aerob [kJ]", "W_PCR [kJ]", "W_BLC [kJ]", "W_TOT [kJ]",
                       "P_Tot [W]", "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]")) {
    sprintf("%.1f ± %.1f",
            mean(data_sitzen_heavy[[var]], na.rm=TRUE),
            sd(data_sitzen_heavy[[var]], na.rm=TRUE))
  } else if(var == "ΔBLC [mmol·l⁻¹]") {
    sprintf("%.2f ± %.2f",
            mean(data_sitzen_heavy[[var]], na.rm=TRUE),
            sd(data_sitzen_heavy[[var]], na.rm=TRUE))
  }
})

# Stehen Leicht
stats$`Stehen Leicht (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_stehen_light <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "stehen", Intensität == "leicht")
  data_stehen_moderate <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "stehen", Intensität == "moderat")
  data_stehen_heavy <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "stehen", Intensität == "schwer")
  
  if(all(is.na(data_stehen_light[[var]]))) {
    return("-")
  }
  
  if(var %in% c("Wirk_muskulär [%]", "Wirk_Netto [%]", "Wirk_Brutto [%]")) {
    # Temporäre Datenkopie erstellen
    temp_data <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "stehen")
    temp_data$dv <- temp_data[[var]] * 100
    
    # Shapiro-Wilk Tests
    shapiro_light <- shapiro.test(filter(temp_data, Intensität == "leicht")$dv)
    shapiro_moderate <- shapiro.test(filter(temp_data, Intensität == "moderat")$dv)
    shapiro_heavy <- shapiro.test(filter(temp_data, Intensität == "schwer")$dv)
    
    # Interpretation der Shapiro-Wilk Tests
    shapiro_interpret_light <- ifelse(shapiro_light$p.value > 0.05, "normalverteilt", "nicht normalverteilt")
    shapiro_interpret_moderate <- ifelse(shapiro_moderate$p.value > 0.05, "normalverteilt", "nicht normalverteilt")
    shapiro_interpret_heavy <- ifelse(shapiro_heavy$p.value > 0.05, "normalverteilt", "nicht normalverteilt")
    
    # Levene Test
    temp_data$Intensität <- as.factor(temp_data$Intensität)
    levene_result <- car::leveneTest(temp_data$dv ~ temp_data$Intensität)
    levene_interpret <- ifelse(levene_result$`Pr(>F)`[1] > 0.05, "homogen", "nicht homogen")
    
    # ANOVA für Stehen bleibt gleich
    ez_result_stehen <- ezANOVA(
      data = temp_data,
      dv = dv,
      wid = Proband,
      within = Intensität,
      detailed = TRUE
    )
    
    # Berechnung des partiellen Eta-Quadrats
    SSeffect <- ez_result_stehen$ANOVA$SSn[2]
    SSerror <- ez_result_stehen$ANOVA$SSd[2]
    partial_eta_squared <- SSeffect / (SSeffect + SSerror)
    
    # Post-hoc Test für Stehen
    aov_model_stehen <- aov(dv ~ Intensität + Error(Proband/Intensität), data = temp_data)
    post_hoc_stehen <- TukeyHSD(aov(dv ~ Intensität, data = temp_data))
    
    # Post-hoc p-Werte extrahieren
    p_stehen_leicht_moderat <- post_hoc_stehen$Intensität["moderat-leicht", "p adj"]
    p_stehen_leicht_schwer <- post_hoc_stehen$Intensität["schwer-leicht", "p adj"]
    p_stehen_moderat_schwer <- post_hoc_stehen$Intensität["schwer-moderat", "p adj"]
    
    # Deskriptive Statistik für Stehen
    print(sprintf("\nDeskriptive Statistik Stehen für %s (in %%)", var))
    print(sprintf("Gesamtmittelwert Stehen: %.2f%%", mean(temp_data$dv)))
    print("\nMittelwerte nach Intensität (Stehen):")
    aggregate(dv ~ Intensität, data = temp_data, 
              FUN = function(x) c(mean = mean(x), sd = sd(x))) %>% print()
    
    # ANOVA Ergebnisse
    f_value_stehen <- ez_result_stehen$ANOVA$F[2]
    p_value_stehen <- ez_result_stehen$ANOVA$p[2]
    df_effect_stehen <- ez_result_stehen$ANOVA$DFn[2]
    df_error_stehen <- ez_result_stehen$ANOVA$DFd[2]
    ges_stehen <- ez_result_stehen$ANOVA$ges[2]
    
    # Statistische Ausgabe
    print(sprintf("\n--- ANOVA Ergebnisse Stehen für %s ---", var))
    print(sprintf("F(%d,%d) = %.3f", df_effect_stehen, df_error_stehen, f_value_stehen))
    print(sprintf("p-Wert = %.4f", p_value_stehen))
    print(sprintf("Partielles Eta-Quadrat = %.3f", partial_eta_squared))
    
    print(sprintf("\n--- Post-hoc Analyse Stehen (Tukey HSD) für %s ---", var))
    print("Paarweise Vergleiche:")
    print(post_hoc_stehen$Intensität)
    
    print(sprintf("\nNormalverteilung Stehen für %s:", var))
    print(sprintf("Leicht: W = %.3f, p = %.4f (%s)", 
                  shapiro_light$statistic, shapiro_light$p.value, shapiro_interpret_light))
    print(sprintf("Moderat: W = %.3f, p = %.4f (%s)", 
                  shapiro_moderate$statistic, shapiro_moderate$p.value, shapiro_interpret_moderate))
    print(sprintf("Schwer: W = %.3f, p = %.4f (%s)", 
                  shapiro_heavy$statistic, shapiro_heavy$p.value, shapiro_interpret_heavy))
    
    print(sprintf("\nVarianzhomogenität für %s:", var))
    print(sprintf("Levene-Test: F = %.3f, p = %.4f (%s)", 
                  levene_result$`F value`[1], levene_result$`Pr(>F)`[1], levene_interpret))
    
    result <- sprintf("%.2f ± %.2f", 
                      100 * mean(data_stehen_light[[var]], na.rm=TRUE),
                      100 * sd(data_stehen_light[[var]], na.rm=TRUE))
    
    as.character(result)
    
  } else if(var == "Wirk_Arbeit [%]") {
    sprintf("%.2f ± %.2f", 
            100 * mean(data_stehen_light[[var]], na.rm=TRUE),
            100 * sd(data_stehen_light[[var]], na.rm=TRUE))
  } else if(var %in% c("W_Aerob [kJ]", "W_PCR [kJ]", "W_BLC [kJ]", "W_TOT [kJ]",
                       "P_Tot [W]", "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]")) {
    sprintf("%.1f ± %.1f",
            mean(data_stehen_light[[var]], na.rm=TRUE),
            sd(data_stehen_light[[var]], na.rm=TRUE))
  } else if(var == "ΔBLC [mmol·l⁻¹]") {
    sprintf("%.2f ± %.2f",
            mean(data_stehen_light[[var]], na.rm=TRUE),
            sd(data_stehen_light[[var]], na.rm=TRUE))
  }
})

# Stehen Moderat
stats$`Stehen Moderat (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_stehen_moderate <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "stehen", Intensität == "moderat")
  
  if(all(is.na(data_stehen_moderate[[var]]))) {
    return("-")
  }
  
  if(var %in% c("Wirk_muskulär [%]", "Wirk_Arbeit [%]","Wirk_Brutto [%]", "Wirk_Netto [%]")) {
    sprintf("%.2f ± %.2f", 
            100 * mean(data_stehen_moderate[[var]], na.rm=TRUE),
            100 * sd(data_stehen_moderate[[var]], na.rm=TRUE))
  } else if(var %in% c("W_Aerob [kJ]", "W_PCR [kJ]", "W_BLC [kJ]", "W_TOT [kJ]",
                       "P_Tot [W]", "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]")) {
    sprintf("%.1f ± %.1f",
            mean(data_stehen_moderate[[var]], na.rm=TRUE),
            sd(data_stehen_moderate[[var]], na.rm=TRUE))
  } else if(var == "ΔBLC [mmol·l⁻¹]") {
    sprintf("%.2f ± %.2f",
            mean(data_stehen_moderate[[var]], na.rm=TRUE),
            sd(data_stehen_moderate[[var]], na.rm=TRUE))
  }
})

# Stehen Schwer
stats$`Stehen Schwer (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_stehen_heavy <- filter(Bedingungen_data_Wirkungsgrad, Bedingung == "stehen", Intensität == "schwer")
  
  if(all(is.na(data_stehen_heavy[[var]]))) {
    return("-")
  }
  
  if(var %in% c("Wirk_muskulär [%]", "Wirk_Arbeit [%]", "Wirk_Brutto [%]", "Wirk_Netto [%]")) {
    sprintf("%.2f ± %.2f", 
            100 * mean(data_stehen_heavy[[var]], na.rm=TRUE),
            100 * sd(data_stehen_heavy[[var]], na.rm=TRUE))
  } else if(var %in% c("W_Aerob [kJ]", "W_PCR [kJ]", "W_BLC [kJ]", "W_TOT [kJ]",
                       "P_Tot [W]", "P_mech [W]", "P_Int [W]", "Drehzahl [U·min⁻¹]")) {
    sprintf("%.1f ± %.1f",
            mean(data_stehen_heavy[[var]], na.rm=TRUE),
            sd(data_stehen_heavy[[var]], na.rm=TRUE))
  } else if(var == "ΔBLC [mmol·l⁻¹]") {
    sprintf("%.2f ± %.2f",
            mean(data_stehen_heavy[[var]], na.rm=TRUE),
            sd(data_stehen_heavy[[var]], na.rm=TRUE))
  }
})

sitzen_header <- as_paragraph("Sitzen (MW ± SD)", as_sup(" †"))

# Tabelle erstellen
ft_Wirkungsgrad_stats_Bedingung_Intensitaet_mean <- flextable(stats) %>%
  set_header_labels(
    Variable = "Parameter",
    `Sitzen Leicht (MW ± SD)` = "Leicht",
    `Sitzen Moderat (MW ± SD)` = "Moderat",
    `Sitzen Schwer (MW ± SD)` = "Schwer",
    `Stehen Leicht (MW ± SD)` = "Leicht",
    `Stehen Moderat (MW ± SD)` = "Moderat",
    `Stehen Schwer (MW ± SD)` = "Schwer"
  ) %>%
  add_header_row(
    values = c("", "Sitzen (MW ± SD)", "Stehen (MW ± SD)"),
    colwidths = c(1, 3, 3)
  ) %>%
  compose(
    part = "header",
    i = 1,
    j = 2,
    value = as_paragraph("Sitzen (MW ± SD)", as_sup(" †"))
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "Wirk_muskulär [%]",
    value = as_paragraph("η", as_sub("muskulär"), " [%]", as_sup(" a"))
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "Wirk_Arbeit [%]",
    value = as_paragraph("η", as_sub("Arbeit,sitzen"), " [%]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "Wirk_Netto [%]",
    value = as_paragraph("η", as_sub("Netto"), " [%]", as_sup(" a"))
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "Wirk_Brutto [%]",
    value = as_paragraph("η", as_sub("Brutto"), " [%]", as_sup(" a"))
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
  bold(i = ~ Variable == "Wirk_muskulär [%]") %>%
  add_footer_row(
    values = as_paragraph(
      as_sup("a"), " : Parametrische Analyse mittels einfaktorieller Varianzanalyse (ANOVA) mit Messwiederholung und Tukey HSD Post-hoc-Test; ",
      as_sup("†"), " : Signifikanter Haupteffekt der Intensität für η", as_sub("muskulär"), " innerhalb der Bedingung Sitzen (p < .05)\n",
      "η", as_sub("muskulär"), " [%]: muskulärer Wirkungsgrad; ",
      "η", as_sub("Arbeit,sitzen"), " [%]: Arbeitswirkungsgrad (sitzende Position); ",
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


####################################################################
# Post-hoc Tests mit Cohen's d
post_hoc_data <- data.frame(
  Vergleich = rownames(posthoc_ergebnisse_sitzen$`Wirk_muskulär [%]`$posthoc),
  Differenz = posthoc_ergebnisse_sitzen$`Wirk_muskulär [%]`$posthoc[, "diff"],
  KI_unten = posthoc_ergebnisse_sitzen$`Wirk_muskulär [%]`$posthoc[, "lwr"],
  KI_oben = posthoc_ergebnisse_sitzen$`Wirk_muskulär [%]`$posthoc[, "upr"],
  p_Wert = posthoc_ergebnisse_sitzen$`Wirk_muskulär [%]`$posthoc[, "p adj"],
  Cohens_d = posthoc_ergebnisse_sitzen$`Wirk_muskulär [%]`$cohens_d$d
)

# Neuordnung der Zeilen
post_hoc_data <- post_hoc_data[c(2, 1, 3), ]

# Groß-/Kleinschreibung anpassen
post_hoc_data$Vergleich <- gsub("moderat", "Moderat", post_hoc_data$Vergleich)
post_hoc_data$Vergleich <- gsub("leicht", "Leicht", post_hoc_data$Vergleich)
post_hoc_data$Vergleich <- gsub("schwer", "Schwer", post_hoc_data$Vergleich)

# Funktion zum Hinzufügen der Signifikanzsymbole
add_significance_symbols <- function(p_value) {
  if (p_value < 0.001) return("***")
  else if (p_value < 0.01) return("**")
  else if (p_value < 0.05) return("*")
  else return("")
}

# Signifikanzsymbole hinzufügen
post_hoc_data$p_Wert <- paste0(format(round(post_hoc_data$p_Wert, 3), nsmall = 3),
                               sapply(post_hoc_data$p_Wert, add_significance_symbols))

# Erstellen der Flextable
ft_post_hoc_sitzen <- flextable(post_hoc_data) %>%
  set_header_labels(
    Vergleich = " ",  # Temporärer Platzhalter
    Differenz = "Mittlere Differenz",
    KI_unten = "95% KI Untergrenze",
    KI_oben = "95% KI Obergrenze",
    p_Wert = "Signifikanzniveau",
    Cohens_d = "Cohen's d"
  ) %>%
  compose(
    part = "header",
    i = 1, j = "Vergleich",
    value = as_paragraph("Sitzen: Post-hoc Vergleich von ", "η", as_sub("muskulär"))
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
      "Signifikanzniveaus: *** p < 0.001; ** p < 0.01; * p < 0.05"
    ),
    colwidths = 6
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
  hline(i = 1, part = "header", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  hline_top(part = "footer", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  hline_bottom(part = "footer", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  colformat_double(
    j = c("Differenz", "KI_unten", "KI_oben", "Cohens_d"), 
    digits = 2
  )

# Setzen der Tabelle auf volle Breite
ft_post_hoc_sitzen <- set_table_properties(
  ft_post_hoc_sitzen, 
  width = 1, 
  layout = "autofit"
)

# Anzeigen der Tabelle
ft_post_hoc_sitzen

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_post_hoc_sitzen, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_post_hoc_sitzen.rds")
ft_post_hoc_sitzen <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_post_hoc_sitzen.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_post_hoc_sitzen, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_post_hoc_sitzen.rds")
ft_post_hoc_sitzen <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_post_hoc_sitzen.rds")


###############################################################################
##### Intensitäten der Bedingungen verlgeichen #####

# Daten für die drei Intensitätsvergleiche vorbereiten
# Leicht
data_leicht <- filter(Bedingungen_data_Wirkungsgrad, 
                      Intensität == "leicht") %>%
  mutate(dv = `Wirk_muskulär [%]` * 100)

# Moderat
data_moderat <- filter(Bedingungen_data_Wirkungsgrad, 
                       Intensität == "moderat") %>%
  mutate(dv = `Wirk_muskulär [%]` * 100)

# Schwer
data_schwer <- filter(Bedingungen_data_Wirkungsgrad, 
                      Intensität == "schwer") %>%
  mutate(dv = `Wirk_muskulär [%]` * 100)

# ANOVA für jede Intensität
# Leicht
anova_leicht <- ezANOVA(
  data = data_leicht,
  dv = dv,
  wid = Proband,
  within = Bedingung,
  detailed = TRUE
)

# Moderat
anova_moderat <- ezANOVA(
  data = data_moderat,
  dv = dv,
  wid = Proband,
  within = Bedingung,
  detailed = TRUE
)

# Schwer
anova_schwer <- ezANOVA(
  data = data_schwer,
  dv = dv,
  wid = Proband,
  within = Bedingung,
  detailed = TRUE
)

# Berechnung der partiellen Eta-Quadrate
partial_eta_leicht <- anova_leicht$ANOVA$SSn[2] / (anova_leicht$ANOVA$SSn[2] + anova_leicht$ANOVA$SSd[2])
partial_eta_moderat <- anova_moderat$ANOVA$SSn[2] / (anova_moderat$ANOVA$SSn[2] + anova_moderat$ANOVA$SSd[2])
partial_eta_schwer <- anova_schwer$ANOVA$SSn[2] / (anova_schwer$ANOVA$SSn[2] + anova_schwer$ANOVA$SSd[2])

print(sprintf("\n--- ANOVA Ergebnisse ---\n
Leichte Intensität:
F(%d,%d) = %.3f, p = %.4f, partielles η² = %.3f\n
Moderate Intensität:
F(%d,%d) = %.3f, p = %.4f, partielles η² = %.3f\n
Schwere Intensität:
F(%d,%d) = %.3f, p = %.4f, partielles η² = %.3f",
              anova_leicht$ANOVA$DFn[2],
              anova_leicht$ANOVA$DFd[2],
              anova_leicht$ANOVA$F[2],
              anova_leicht$ANOVA$p[2],
              partial_eta_leicht,
              anova_moderat$ANOVA$DFn[2],
              anova_moderat$ANOVA$DFd[2],
              anova_moderat$ANOVA$F[2],
              anova_moderat$ANOVA$p[2],
              partial_eta_moderat,
              anova_schwer$ANOVA$DFn[2],
              anova_schwer$ANOVA$DFd[2],
              anova_schwer$ANOVA$F[2],
              anova_schwer$ANOVA$p[2],
              partial_eta_schwer))

# Erstellen eines Dataframes für die Ergebnistabelle
anova_results <- data.frame(
  Vergleich = c("Leicht: Sitzen vs. Stehen", 
                "Moderat: Sitzen vs. Stehen", 
                "Schwer: Sitzen vs. Stehen"),
  F_Wert = c(anova_leicht$ANOVA$F[2], 
             anova_moderat$ANOVA$F[2], 
             anova_schwer$ANOVA$F[2]),
  df1 = c(anova_leicht$ANOVA$DFn[2], 
          anova_moderat$ANOVA$DFn[2], 
          anova_schwer$ANOVA$DFn[2]),
  df2 = c(anova_leicht$ANOVA$DFd[2], 
          anova_moderat$ANOVA$DFd[2], 
          anova_schwer$ANOVA$DFd[2]),
  p_Wert = c(anova_leicht$ANOVA$p[2], 
             anova_moderat$ANOVA$p[2], 
             anova_schwer$ANOVA$p[2]),
  partial_eta = c(partial_eta_leicht, 
                  partial_eta_moderat, 
                  partial_eta_schwer)
)

# Funktion zum Hinzufügen der Signifikanzsymbole
add_significance_symbols <- function(p_value) {
  if (p_value < 0.001) return("***")
  else if (p_value < 0.01) return("**")
  else if (p_value < 0.05) return("*")
  else return("")
}

# Signifikanzsymbole hinzufügen
anova_results$p_Wert <- paste0(
  format(round(anova_results$p_Wert, 3), nsmall = 3),
  sapply(anova_results$p_Wert, add_significance_symbols)
)

# Erstellen der Flextable
ft_anova <- flextable(anova_results) %>%
  set_header_labels(
    Vergleich = " ",  # Temporärer Platzhalter
    F_Wert = "F-Statistik",
    df1 = "df",  
    df2 = "df", 
    p_Wert = "Signifikanzniveau",
    partial_eta = "η²p"  # Geändert von ges zu partial_eta
  ) %>%
  compose(
    part = "header",
    i = 1, j = "Vergleich",
    value = as_paragraph("ANOVA: Vergleiche von ", "η", as_sub("muskulär"))
  ) %>%
  compose(
    part = "header",
    i = 1,
    j = "partial_eta",  # Geändert von ges zu partial_eta
    value = as_paragraph("η", as_sup("2"))  
  ) %>%
  compose(  # df mit Zähler
    part = "header",
    j = "df1",
    value = as_paragraph("df", as_sub("Zähler"))
  ) %>%
  compose(  # df mit Nenner
    part = "header",
    j = "df2",
    value = as_paragraph("df", as_sub("Nenner"))
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
      "Signifikanzniveaus: *** p < 0.001; ** p < 0.01; * p < 0.05"
    ),
    colwidths = 6
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
  hline(i = 1, part = "header", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  hline_top(part = "footer", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  hline_bottom(part = "footer", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  colformat_double(
    j = c("F_Wert", "partial_eta"),  # Geändert von ges zu partial_eta
    digits = 3
  )

# Setzen der Tabelle auf volle Breite
ft_anova <- set_table_properties(
  ft_anova, 
  width = 1, 
  layout = "autofit"
)

# Anzeigen der Tabelle
ft_anova

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_anova, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_anova.rds")
ft_anova <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_anova.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_anova, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_anova.rds")
ft_anova <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_anova.rds")



