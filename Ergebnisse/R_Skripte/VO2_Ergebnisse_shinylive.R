library(shiny)
library(shinylive)
library(DT)
library(dplyr)

EPOC_data_df_VO2 <- data.frame(
  `Proband` = c( "20", "23", "20", "20", "10", "01", "15", "19", "15", "20", "23", "22", "23", "19", "19", "15", "20", "23", "01", "01", "20", "06", "22", "01", "06", "13", "06", "06", "23", "15", "22", "01", "19", "23", "22", "06", "01", "19", "15", "19", "10", "10", "06", "15", "10", "13", "10", "13", "13", "10", "13", "13", "22", "22" ),
  `Nr` = c( 2, 2, 5, 3, 1, 1, 2, 2, 4, 1, 3, 3, 1, 4, 1, 1, 4, 4, 6, 3, 6, 3, 4, 2, 6, 1, 1, 2, 6, 3, 2, 5, 3, 5, 1, 5, 4, 6, 6, 5, 4, 2, 4, 5, 6, 2, 3, 3, 6, 5, 4, 5, 5, 6 ),
  `Bedingung` = c( "stehen", "sitzen", "stehen", "stehen", "stehen", "stehen", "stehen", "sitzen", "stehen", "sitzen", "sitzen", "stehen", "stehen", "stehen", "stehen", "sitzen", "sitzen", "stehen", "stehen", "sitzen", "sitzen", "stehen", "sitzen", "sitzen", "sitzen", "stehen", "stehen", "sitzen", "sitzen", "sitzen", "stehen", "sitzen", "sitzen", "stehen", "sitzen", "stehen", "stehen", "stehen", "sitzen", "sitzen", "stehen", "sitzen", "sitzen", "stehen", "sitzen", "sitzen", "sitzen", "stehen", "stehen", "stehen", "sitzen", "sitzen", "sitzen", "stehen" ),
  `Intensität` = c( "leicht", "leicht", "schwer", "moderat", "leicht", "leicht", "leicht", "leicht", "moderat", "leicht", "moderat", "moderat", "leicht", "moderat", "leicht", "leicht", "moderat", "moderat", "schwer", "moderat", "schwer", "moderat", "moderat", "leicht", "schwer", "leicht", "leicht", "leicht", "schwer", "moderat", "leicht", "schwer", "moderat", "schwer", "leicht", "schwer", "moderat", "schwer", "schwer", "schwer", "moderat", "leicht", "moderat", "schwer", "schwer", "leicht", "moderat", "moderat", "schwer", "schwer", "moderat", "schwer", "schwer", "schwer" ),
  `P_Tot [W]` = c( 262.464492841036, 204.303811710974, 332.282195549595, 297.033969571712, 342.780201343152, 315.087785382455, 278.360889376064, 247.499507416179, 330.104243714921, 297.677841174368, 230.539973534982, 265.6271457606, 199.166908885976, 272.461438312168, 255.677641947778, 317.381745748518, 313.722436973434, 223.28140151954, 377.269342767856, 343.249556933665, 347.559610294167, 319.563716705383, 278.279940401837, 315.356007475201, 334.696918549422, 328.689199107333, 291.340972880708, 296.008730999173, 255.967028389931, 350.889509890097, 236.218071887388, 376.01429973294, 267.481373216035, 248.864184197347, 245.195285695477, 342.797408580074, 345.621870862815, 306.49260579391, 400.223676014955, 297.602794954409, 400.467853364667, 364.604429952782, 320.346360885814, 363.61916212901, 452.463577903494, 348.476140134227, 419.799963753223, 357.035734534098, 383.031683804794, 431.054304415492, 375.551234669824, 395.659416230325, 297.955305580942, 291.810409732378 ),
  `P_Tot_kg [W·kg⁻¹]` = c( 3.28080616051295, 3.40506352851623, 4.15352744436994, 3.7129246196464, 4.18024635784332, 4.14589191292704, 3.662643281264, 3.80768472947968, 4.34347689098581, 3.7209730146796, 3.84233289224969, 5.53389887001251, 3.31944848143294, 4.19171443557181, 3.93350218381197, 4.17607560195419, 3.92153046216792, 3.72135669199234, 4.96407029957705, 4.51644153860085, 4.34449512867708, 4.37758516034771, 5.79749875837161, 4.14942115098949, 4.58488929519755, 4.56512776537963, 3.99097223124258, 4.05491412327634, 4.26611713983218, 4.61696723539601, 4.92120983098725, 4.94755657543343, 4.11509804947746, 4.14773640328911, 5.10823511865578, 4.69585491205581, 4.54765619556336, 4.71527085836785, 5.26610100019677, 4.57850453776014, 4.88375430932521, 4.44639548722904, 4.3883063135043, 4.78446265959224, 5.51784851101822, 4.83994639075315, 5.11951175308808, 4.95882964630691, 5.31988449728881, 5.25675980994502, 5.21598937041422, 5.49526966986562, 6.20740219960296, 6.07938353609121 ),
  `Masse [kg]` = c( 80, 60, 80, 80, 82, 76, 76, 65, 76, 80, 60, 48, 60, 65, 65, 76, 80, 60, 76, 76, 80, 73, 48, 76, 73, 72, 73, 73, 60, 76, 48, 76, 65, 60, 48, 73, 76, 65, 76, 65, 82, 82, 73, 76, 82, 72, 82, 72, 72, 82, 72, 72, 48, 48 ),
  `VO2_Ruhe [l·min⁻¹]` = c( 0.415642091286353, 0.320312513138994, 0.415642091286353, 0.415642091286353, 0.398544621667437, 0.409079599507683, 0.398278038452436, 0.352584633817119, 0.398278038452436, 0.415642091286353, 0.320312513138994, 0.293734941358178, 0.320312513138994, 0.352584633817119, 0.352584633817119, 0.398278038452436, 0.415642091286353, 0.320312513138994, 0.409079599507683, 0.409079599507683, 0.415642091286353, 0.390487656996456, 0.293734941358178, 0.409079599507683, 0.390487656996456, 0.378852606093004, 0.390487656996456, 0.390487656996456, 0.320312513138994, 0.398278038452436, 0.293734941358178, 0.409079599507683, 0.352584633817119, 0.320312513138994, 0.293734941358178, 0.390487656996456, 0.409079599507683, 0.352584633817119, 0.398278038452436, 0.352584633817119, 0.398544621667437, 0.398544621667437, 0.390487656996456, 0.398278038452436, 0.398544621667437, 0.378852606093004, 0.398544621667437, 0.378852606093004, 0.378852606093004, 0.398544621667437, 0.378852606093004, 0.378852606093004, 0.293734941358178, 0.293734941358178 ),
  `tau_on [s]` = c( 32.6, 20, 51.4, 42, 29, 46.5, 39.1, 25.2, 47.2, 35.9, 21.1, 39.4, 24.3, 32.5, 28.4, 43.1, 36.7, 25.2, 46, 50.7, 45.4, 40.6, 25.9, 44.9, 49.8, 30.3, 38.5, 34.6, 25.7, 42.6, 26.6, 54.9, 30.2, 28.3, 19.9, 46.8, 34.8, 40.4, 56.8, 34.4, 30.6, 28.4, 39.6, 41.7, 48.5, 28.1, 36, 36.6, 36, 38.1, 28.8, 43.4, 24.9, 29.8 ),
  `VO2_SS_percent [%]` = c( 60.21, 63.55, 81.32, 69.7, 72.62, 78.05, 72.53, 66.2, 84.45, 65.42, 70.47, 82.97, 67.8, 78.14, 73.56, 74.8, 71.97, 72.88, 92.44, 80.93, 84.22, 81.67, 87.62, 72.88, 90.56, 82.89, 78.78, 78.47, 77.61, 81.88, 74.84, 95.41, 72.77, 78.3, 78.2, 88.98, 82.86, 86.28, 93.29, 82.38, 82.2, 72.38, 82.74, 90.17, 97.43, 80.73, 84.49, 89.87, 95.97, 89.86, 86.88, 94.87, 95.96, 88.77 ),
  `R2_on` = c( 0.82, 0.89, 0.93, 0.9, 0.91, 0.93, 0.96, 0.9, 0.91, 0.82, 0.91, 0.89, 0.88, 0.95, 0.98, 0.9, 0.93, 0.95, 0.96, 0.97, 0.95, 0.95, 0.93, 0.95, 0.98, 0.86, 0.92, 0.96, 0.96, 0.92, 0.9, 0.93, 0.9, 0.95, 0.86, 0.97, 0.96, 0.95, 0.95, 0.96, 0.98, 0.91, 0.96, 0.95, 0.99, 0.89, 0.97, 0.92, 0.93, 0.98, 0.93, 0.96, 0.87, 0.93 ),
  `VO2_Brutto_SS [l·min⁻¹]` = c( 3.343, 2.589, 4.515, 3.87, 4.49, 4.413, 3.864, 3.107, 4.499, 3.632, 2.871, 3.234, 2.762, 3.667, 3.452, 3.985, 3.996, 2.969, 5.227, 4.576, 4.676, 4.042, 3.415, 4.121, 4.482, 4.297, 3.899, 3.884, 3.162, 4.362, 2.917, 5.395, 3.415, 3.19, 3.048, 4.404, 4.685, 4.049, 4.97, 3.866, 5.082, 4.475, 4.095, 4.804, 6.024, 4.185, 5.224, 4.659, 4.975, 5.556, 4.504, 4.918, 3.74, 3.46 ),
  `VO2_Netto_SS [l·min⁻¹]` = c( 2.99, 2.211, 4.162, 3.517, 4.042, 4.004, 3.466, 2.837, 4.101, 3.279, 2.493, 2.962, 2.384, 3.397, 3.182, 3.587, 3.643, 2.591, 4.818, 4.167, 4.323, 3.652, 3.143, 3.712, 4.092, 3.858, 3.509, 3.494, 2.784, 3.964, 2.645, 4.986, 3.145, 2.812, 2.776, 4.014, 4.276, 3.779, 4.572, 3.596, 4.634, 4.027, 3.705, 4.406, 5.576, 3.746, 4.776, 4.22, 4.536, 5.108, 4.065, 4.479, 3.468, 3.188 ),
  `delta_VO2_SS` = c( 1.356, 1.229, 2.318, 2.15, 2.554, 2.122, 2.222, 1.736, 2.882, 1.869, 1.39, 1.841, 1.385, 2.058, 1.962, 1.763, 2.119, 1.501, 3.209, 2.199, 2.46, 1.925, 2.07, 2.419, 2.367, 2.211, 1.98, 1.752, 1.639, 2.177, 1.674, 3.547, 1.832, 1.561, 1.627, 2.864, 2.281, 2.364, 2.885, 2.163, 3.118, 3.728, 1.784, 3.114, 4.164, 2.172, 3.155, 2.616, 2.904, 3.882, 2.6, 3.331, 2.756, 2.821 ),
  `VO2_on_Start [l·min⁻¹]` = c( 2.012, 1.383, 2.239, 1.759, 1.983, 2.33, 1.683, 1.403, 1.67, 1.797, 1.506, 1.427, 1.403, 1.646, 1.526, 2.254, 1.916, 1.495, 2.077, 2.417, 2.261, 2.153, 1.383, 1.747, 2.158, 2.126, 1.955, 2.164, 1.553, 2.225, 1.274, 1.913, 1.616, 1.657, 1.45, 1.592, 2.446, 1.728, 2.138, 1.742, 2.021, 0.815, 2.344, 1.747, 1.937, 2.053, 2.127, 2.091, 2.124, 1.745, 1.952, 1.648, 1.035, 0.69 ),
  `VO2_Referenz [l·min⁻¹]` = c( 0.852692307692308, 0.896472222222222, 0.852692307692308, 0.852692307692308, 0.9478, 0.997235294117647, 0.898278038452436, 0.769533333333333, 0.898278038452436, 0.852692307692308, 0.896472222222222, 0.772045454545454, 0.896472222222222, 0.769533333333333, 0.769533333333333, 0.898278038452436, 0.852692307692308, 0.896472222222222, 0.997235294117647, 0.997235294117647, 0.852692307692308, 0.934166666666667, 0.772045454545454, 0.997235294117647, 0.934166666666667, 0.999544642857143, 0.934166666666667, 0.934166666666667, 0.896472222222222, 0.898278038452436, 0.772045454545454, 0.997235294117647, 0.769533333333333, 0.896472222222222, 0.772045454545454, 0.934166666666667, 0.997235294117647, 0.769533333333333, 0.898278038452436, 0.769533333333333, 0.9478, 0.9478, 0.934166666666667, 0.898278038452436, 0.9478, 0.999544642857143, 0.9478, 0.999544642857143, 0.999544642857143, 0.9478, 0.999544642857143, 0.999544642857143, 0.772045454545454, 0.772045454545454 ),
  `VO2_avg [l·min⁻¹]` = c( 3.125, 2.436, 4.053, 3.542, 4.176, 4.032, 3.61, 2.946, 4.095, 3.324, 2.702, 3.074, 2.637, 3.407, 3.232, 3.728, 3.637, 2.802, 4.705, 4.124, 4.185, 3.699, 3.208, 3.768, 3.944, 3.965, 3.609, 3.626, 2.96, 4.073, 2.802, 4.716, 3.158, 2.968, 2.929, 4.06, 4.28, 3.743, 4.485, 3.566, 4.652, 3.944, 3.783, 4.41, 5.401, 3.902, 4.733, 4.327, 4.554, 4.961, 4.195, 4.45, 3.505, 3.222 ),
  `VO2_SS_avg [l·min⁻¹]` = c( 3.237, 2.531, 4.301, 3.709, 4.363, 4.246, 3.746, 3.031, 4.324, 3.494, 2.807, 3.156, 2.716, 3.571, 3.373, 3.862, 3.846, 2.9, 4.996, 4.387, 4.454, 3.891, 3.309, 3.979, 4.198, 4.15, 3.771, 3.767, 3.077, 4.214, 2.871, 5.064, 3.308, 3.088, 2.969, 4.223, 4.517, 3.928, 4.71, 3.744, 4.916, 4.29, 3.956, 4.623, 5.766, 4.075, 5.04, 4.5, 4.804, 5.345, 4.373, 4.742, 3.642, 3.361 ),
  `VCO2_avg [l·min⁻¹]` = c( 2.79, 2.21, 3.845, 3.274, 3.616, 3.635, 3.341, 2.621, 3.945, 2.845, 2.538, 2.778, 2.373, 3.287, 3.006, 3.346, 3.309, 2.731, 4.292, 3.802, 3.918, 3.55, 3.028, 3.415, 3.77, 3.549, 3.19, 3.335, 2.893, 3.861, 2.493, 4.559, 2.907, 2.965, 2.635, 3.949, 3.915, 3.692, 4.232, 3.455, 4.475, 3.635, 3.623, 4.304, 5.473, 3.615, 4.659, 4.07, 4.33, 4.938, 4.033, 4.413, 3.414, 2.994 ),
  `VCO2_SS_avg [l·min⁻¹]` = c( 2.933, 2.291, 4.135, 3.452, 3.791, 3.873, 3.539, 2.735, 4.215, 2.998, 2.642, 2.935, 2.459, 3.457, 3.137, 3.555, 3.519, 2.832, 4.607, 4.111, 4.207, 3.767, 3.13, 3.653, 4.104, 3.711, 3.366, 3.488, 3.018, 4.085, 2.561, 4.954, 3.055, 3.09, 2.708, 4.16, 4.158, 3.901, 4.621, 3.657, 4.748, 3.976, 3.833, 4.543, 5.917, 3.775, 4.99, 4.282, 4.584, 5.323, 4.209, 4.724, 3.55, 3.124 ),
  `RQ_avg` = c( 0.89, 0.9, 0.94, 0.92, 0.88, 0.9, 0.93, 0.9, 0.95, 0.85, 0.94, 0.92, 0.91, 0.96, 0.92, 0.89, 0.9, 0.97, 0.9, 0.92, 0.92, 0.95, 0.94, 0.9, 0.94, 0.89, 0.87, 0.91, 0.97, 0.94, 0.9, 0.95, 0.91, 0.99, 0.9, 0.96, 0.9, 0.98, 0.94, 0.96, 0.95, 0.88, 0.94, 0.96, 1, 0.92, 0.97, 0.94, 0.94, 0.97, 0.95, 0.98, 0.96, 0.92 ),
  `tau_on_min [min]` = c( 0.543333333333333, 0.333333333333333, 0.856666666666667, 0.7, 0.483333333333333, 0.775, 0.651666666666667, 0.42, 0.786666666666667, 0.598333333333333, 0.351666666666667, 0.656666666666667, 0.405, 0.541666666666667, 0.473333333333333, 0.718333333333333, 0.611666666666667, 0.42, 0.766666666666667, 0.845, 0.756666666666667, 0.676666666666667, 0.431666666666667, 0.748333333333333, 0.83, 0.505, 0.641666666666667, 0.576666666666667, 0.428333333333333, 0.71, 0.443333333333333, 0.915, 0.503333333333333, 0.471666666666667, 0.331666666666667, 0.78, 0.58, 0.673333333333333, 0.946666666666667, 0.573333333333333, 0.51, 0.473333333333333, 0.66, 0.695, 0.808333333333333, 0.468333333333333, 0.6, 0.61, 0.6, 0.635, 0.48, 0.723333333333333, 0.415, 0.496666666666667 ),
  `RQ_SS_avg` = c( 0.9, 0.9, 0.96, 0.93, 0.88, 0.91, 0.94, 0.9, 0.97, 0.86, 0.94, 0.93, 0.91, 0.96, 0.93, 0.91, 0.91, 0.97, 0.92, 0.93, 0.94, 0.96, 0.94, 0.91, 0.97, 0.89, 0.89, 0.92, 0.98, 0.96, 0.9, 0.97, 0.92, 1, 0.9, 0.98, 0.91, 0.99, 0.97, 0.97, 0.96, 0.91, 0.96, 0.98, 1.02, 0.92, 0.98, 0.95, 0.95, 0.99, 0.96, 0.99, 0.97, 0.92 )
  , check.names = FALSE
)

# UI Definition
ui <- fluidPage(
  titlePanel("VO2 Daten Analyse"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      style = "height: 90vh; overflow-y: auto;",
      radioButtons("viewType", "Datenansicht:",
                   choices = c("Einzelwerte" = "individual",
                               "Mittelwerte & BP" = "means"),
                   selected = "means"),
      checkboxGroupInput("selectedBedingung", "Bedingungen:",
                         choices = unique(EPOC_data_df_VO2$Bedingung),
                         selected = unique(EPOC_data_df_VO2$Bedingung)),
      checkboxGroupInput("selectedIntensität", "Intensitäten:",
                         choices = unique(EPOC_data_df_VO2$Intensität),
                         selected = unique(EPOC_data_df_VO2$Intensität)),
      radioButtons("selectedVariable", "Variable für Boxplot:",
                   choices = c(
                     "VO2_Brutto_SS [l·min⁻¹]" = "VO2_Brutto_SS [l·min⁻¹]",
                     "VO2_Netto_SS [l·min⁻¹]" = "VO2_Netto_SS [l·min⁻¹]",
                     "VO2_on_Start [l·min⁻¹]" = "VO2_on_Start [l·min⁻¹]",
                     "tau_on [s]" = "tau_on [s]",
                     "R2_on" = "R2_on",
                     "delta_VO2_SS" = "delta_VO2_SS",
                     "VO2_avg [l·min⁻¹]" = "VO2_avg [l·min⁻¹]",
                     "VO2_SS_avg [l·min⁻¹]" = "VO2_SS_avg [l·min⁻¹]",
                     "VCO2_avg [l·min⁻¹]" = "VCO2_avg [l·min⁻¹]",
                     "VCO2_SS_avg [l·min⁻¹]" = "VCO2_SS_avg [l·min⁻¹]",
                     "RQ_avg" = "RQ_avg",
                     "RQ_SS_avg" = "RQ_SS_avg",
                     "VO2_SS_percent [%]" = "VO2_SS_percent [%]",
                     "P_Tot [W]" = "P_Tot [W]",
                     "P_Tot_kg [W·kg⁻¹]" = "P_Tot_kg [W·kg⁻¹]"
                   ),
                   selected = "VO2_Netto_SS [l·min⁻¹]"),
      checkboxGroupInput("selectedProband", "Probanden:",
                         choices = sort(unique(EPOC_data_df_VO2$Proband)),
                         selected = sort(unique(EPOC_data_df_VO2$Proband)))
    ),
    mainPanel(
      width = 10,
      conditionalPanel(
        condition = "input.viewType == 'means'",
        plotlyOutput("boxplot")
      ),
      DTOutput("epocTable")
    )
  )
)

# Server-Logik
server <- function(input, output, session) {
  
  # Hilfsfunktion für Nachkommastellen
  get_digits_for_column <- function(col_name) {
    if(col_name %in% c("VO2_Ruhe [l·min⁻¹]", "VO2_Brutto_SS [l·min⁻¹]", "VO2_Netto_SS [l·min⁻¹]",
                       "VO2_on_Start [l·min⁻¹]", "VO2_Referenz [l·min⁻¹]", "VO2_avg [l·min⁻¹]",
                       "VO2_SS_avg [l·min⁻¹]", "VCO2_avg [l·min⁻¹]", "VCO2_SS_avg [l·min⁻¹]")) {
      return(3)
    } else if(col_name %in% c("P_Tot_kg [W·kg⁻¹]", "R2_on", "delta_VO2_SS", 
                              "RQ_avg", "RQ_SS_avg", "tau_on_min")) {
      return(2)
    } else if(col_name %in% c("P_Tot [W]", "tau_on [s]", "VO2_SS_percent [%]")) {
      return(1)
    } else if(col_name %in% c("Nr", "Masse [kg]")) {
      return(0)
    }
    return(2)
  }
  
  # Reaktive gefilterte Daten für Tabelle
  filtered_data <- reactive({
    data <- EPOC_data_df_VO2
    
    if (length(input$selectedProband) > 0) {
      data <- data %>% filter(Proband %in% input$selectedProband)
    }
    
    if (length(input$selectedBedingung) > 0) {
      data <- data %>% filter(Bedingung %in% input$selectedBedingung)
    }
    
    if (length(input$selectedIntensität) > 0) {
      data <- data %>% filter(Intensität %in% input$selectedIntensität)
    }
    
    if (input$viewType == "means") {
      # Bestimme Gruppierungsvariablen basierend auf Auswahl
      group_vars <- c()
      if (length(input$selectedBedingung) > 0 && length(input$selectedIntensität) == 0) {
        group_vars <- "Bedingung"
      } else if (length(input$selectedBedingung) == 0 && length(input$selectedIntensität) > 0) {
        group_vars <- "Intensität"
      } else if (length(input$selectedBedingung) > 0 && length(input$selectedIntensität) > 0) {
        group_vars <- c("Bedingung", "Intensität")
      }
      
      if (length(group_vars) == 0) {
        grouped_data <- data %>%
          summarise(across(where(is.numeric), 
                           list(mean = ~mean(., na.rm = TRUE),
                                sd = ~sd(., na.rm = TRUE)))) %>%
          mutate(Gruppe = "Gesamt")
        group_vars <- "Gruppe"
      } else {
        grouped_data <- data %>%
          group_by(across(all_of(group_vars))) %>%
          summarise(across(where(is.numeric), 
                           list(mean = ~mean(., na.rm = TRUE),
                                sd = ~sd(., na.rm = TRUE)))) %>%
          ungroup()
      }
      
      result_data <- grouped_data %>%
        select(all_of(group_vars))
      
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      for(col in numeric_cols) {
        mean_col <- paste0(col, "_mean")
        sd_col <- paste0(col, "_sd")
        
        if(mean_col %in% names(grouped_data) && sd_col %in% names(grouped_data)) {
          digits <- get_digits_for_column(col)
          result_data[[col]] <- paste0(
            format(round(grouped_data[[mean_col]], digits), nsmall = digits),
            " ± ",
            format(round(grouped_data[[sd_col]], digits), nsmall = digits)
          )
        }
      }
      
      return(result_data)
    }
    
    return(data)
  })
  
  # Reaktive gefilterte Daten für Plots
  filtered_data_plots <- reactive({
    data <- EPOC_data_df_VO2 %>%
      filter(Proband %in% input$selectedProband)
    
    if (length(input$selectedBedingung) > 0) {
      data <- data %>% filter(Bedingung %in% input$selectedBedingung)
    }
    
    if (length(input$selectedIntensität) > 0) {
      data <- data %>% filter(Intensität %in% input$selectedIntensität)
    }
    
    # Gruppierung basierend auf Auswahl
    if (length(input$selectedBedingung) > 0 && length(input$selectedIntensität) == 0) {
      data$Gruppe <- data$Bedingung
    } else if (length(input$selectedBedingung) == 0 && length(input$selectedIntensität) > 0) {
      data$Gruppe <- data$Intensität
    } else {
      data$Gruppe <- paste(data$Intensität, data$Bedingung, sep = "_")
    }
    
    return(data)
  })
  
  # Color Map
  color_map <- reactive({
    c(
      "leicht_sitzen" = "#42BA97", "leicht_stehen" = "#62A39F",
      "moderat_sitzen" = "#1CADE4", "moderat_stehen" = "#2683C6",
      "schwer_sitzen" = "#EF5350", "schwer_stehen" = "#C8133B"
    )
  })
  
  # Tabellen-Output
  output$epocTable <- renderDT({
    data <- filtered_data()
    
    if (input$viewType == "individual") {
      columnDefs <- lapply(seq_len(ncol(data)), function(i) {
        list(
          targets = i-1,
          width = paste0(max(
            nchar(names(data)[i]),
            max(nchar(as.character(data[[i]])))
          ) * 10, "px"),
          className = "dt-nowrap"
        )
      })
      
      datatable(data,
                options = list(
                  pageLength = 10,
                  scrollX = TRUE,
                  scrollCollapse = TRUE,
                  autoWidth = FALSE,
                  columnDefs = columnDefs
                )
      ) %>%
        formatRound(
          columns = c("VO2_Ruhe [l·min⁻¹]", "VO2_Brutto_SS [l·min⁻¹]", "VO2_Netto_SS [l·min⁻¹]",
                      "VO2_on_Start [l·min⁻¹]", "VO2_Referenz [l·min⁻¹]", "VO2_avg [l·min⁻¹]",
                      "VO2_SS_avg [l·min⁻¹]", "VCO2_avg [l·min⁻¹]", "VCO2_SS_avg [l·min⁻¹]"),
          digits = 3
        ) %>%
        formatRound(
          columns = c("P_Tot_kg [W·kg⁻¹]", "R2_on", "delta_VO2_SS", 
                      "RQ_avg", "RQ_SS_avg"),
          digits = 2
        ) %>%
        formatRound(
          columns = c("P_Tot [W]", "tau_on [s]", "VO2_SS_percent [%]"),
          digits = 1
        ) %>%
        formatRound(
          columns = c("Nr", "Masse [kg]"),
          digits = 0
        )
    } else {
      columnDefs <- lapply(seq_len(ncol(data)), function(i) {
        list(
          targets = i-1,
          width = paste0(max(
            nchar(names(data)[i]),
            max(nchar(as.character(data[[i]])))
          ) * 10, "px"),
          className = "dt-nowrap"
        )
      })
      
      datatable(data,
                options = list(
                  pageLength = 10,
                  scrollX = TRUE,
                  scrollCollapse = TRUE,
                  autoWidth = FALSE,
                  columnDefs = columnDefs
                ),
                escape = FALSE
      )
    }
  })
  
  # Boxplot
  output$boxplot <- renderPlotly({
    data <- filtered_data_plots()
    color_map_values <- color_map()
    
    # Farben anpassen basierend auf Gruppierung
    if (length(input$selectedBedingung) > 0 && length(input$selectedIntensität) == 0) {
      colors <- c("sitzen" = "#42BA97", "stehen" = "#62A39F")
    } else if (length(input$selectedBedingung) == 0 && length(input$selectedIntensität) > 0) {
      colors <- c("leicht" = "#42BA97", "moderat" = "#1CADE4", "schwer" = "#EF5350")
    } else {
      colors <- color_map_values
    }
    
    p <- plot_ly(data = data, 
                 x = ~Gruppe, 
                 y = as.formula(paste0("~`", input$selectedVariable, "`")),
                 type = "box",
                 color = ~Gruppe,
                 colors = colors[unique(data$Gruppe)],
                 opacity = 0.8,
                 line = list(color = "black", width = 0.9),
                 boxpoints = "outliers",
                 pointpos = 0,
                 marker = list(color = "black", size = 4),
                 boxmean = TRUE,
                 hoverlabel = list(bgcolor = "#F5F5F5")
    ) %>%
      layout(title = paste('Boxplot:', input$selectedVariable),
             margin = list(t = 40),
             xaxis = list(title = "Gruppe"),
             yaxis = list(title = input$selectedVariable))
    
    return(p)
  })
}

# App starten
shinyApp(ui = ui, server = server)