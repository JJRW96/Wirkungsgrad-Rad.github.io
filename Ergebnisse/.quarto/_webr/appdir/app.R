library(shiny)
library(plotly)
library(minpack.lm)
library(dplyr)
library(shinyjs)
library(shinylive)
library(DT)
library(dplyr)

EPOC_data_df_VO2 <- data.frame(
  `Proband` = c( "01", "01", "01", "01", "01", "01", "06", "06", "06", "06", "06", "06", "10", "10", "10", "10", "10", "10", "13", "13", "13", "13", "13", "13", "15", "15", "15", "15", "15", "15", "19", "19", "19", "19", "19", "19", "20", "20", "20", "20", "20", "20", "22", "22", "22", "22", "22", "22", "23", "23", "23", "23", "23", "23" ),
  `Nr` = c( 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6 ),
  `Bedingung` = c( "stehen", "sitzen", "sitzen", "stehen", "sitzen", "stehen", "stehen", "sitzen", "stehen", "sitzen", "stehen", "sitzen", "stehen", "sitzen", "sitzen", "stehen", "stehen", "sitzen", "stehen", "sitzen", "stehen", "sitzen", "sitzen", "stehen", "sitzen", "stehen", "sitzen", "stehen", "stehen", "sitzen", "stehen", "sitzen", "sitzen", "stehen", "sitzen", "stehen", "sitzen", "stehen", "stehen", "sitzen", "stehen", "sitzen", "sitzen", "stehen", "stehen", "sitzen", "sitzen", "stehen", "stehen", "sitzen", "sitzen", "stehen", "stehen", "sitzen" ),
  `Intensität` = c( "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer", "leicht", "leicht", "moderat", "moderat", "schwer", "schwer" ),
  `P_Tot [W]` = c( 315.087785382455, 315.356007475201, 343.249556933665, 345.621870862815, 376.01429973294, 377.269342767856, 291.340972880708, 296.008730999173, 319.563716705383, 320.346360885814, 342.797408580074, 334.696918549422, 342.780201343152, 364.604429952782, 419.799963753223, 400.467853364667, 431.054304415492, 452.463577903494, 328.689199107333, 348.476140134227, 357.035734534098, 375.551234669824, 395.659416230325, 383.031683804794, 317.381745748518, 278.360889376064, 350.889509890097, 330.104243714921, 363.61916212901, 400.223676014955, 255.677641947778, 247.499507416179, 267.481373216035, 272.461438312168, 297.602794954409, 306.49260579391, 297.677841174368, 262.464492841036, 297.033969571712, 313.722436973434, 332.282195549595, 347.559610294167, 245.195285695477, 236.218071887388, 265.6271457606, 278.279940401837, 297.955305580942, 291.810409732378, 199.166908885976, 204.303811710974, 230.539973534982, 223.28140151954, 248.864184197347, 255.967028389931 ),
  `P_Tot_kg [W·kg⁻¹]` = c( 4.14589191292704, 4.14942115098949, 4.51644153860085, 4.54765619556336, 4.94755657543343, 4.96407029957705, 3.99097223124258, 4.05491412327634, 4.37758516034771, 4.3883063135043, 4.69585491205581, 4.58488929519755, 4.18024635784332, 4.44639548722904, 5.11951175308808, 4.88375430932521, 5.25675980994502, 5.51784851101822, 4.56512776537963, 4.83994639075315, 4.95882964630691, 5.21598937041422, 5.49526966986562, 5.31988449728881, 4.17607560195419, 3.662643281264, 4.61696723539601, 4.34347689098581, 4.78446265959224, 5.26610100019677, 3.93350218381197, 3.80768472947968, 4.11509804947746, 4.19171443557181, 4.57850453776014, 4.71527085836785, 3.7209730146796, 3.28080616051295, 3.7129246196464, 3.92153046216792, 4.15352744436994, 4.34449512867708, 5.10823511865578, 4.92120983098725, 5.53389887001251, 5.79749875837161, 6.20740219960296, 6.07938353609121, 3.31944848143294, 3.40506352851623, 3.84233289224969, 3.72135669199234, 4.14773640328911, 4.26611713983218 ),
  `Masse [kg]` = c( 76, 76, 76, 76, 76, 76, 73, 73, 73, 73, 73, 73, 82, 82, 82, 82, 82, 82, 72, 72, 72, 72, 72, 72, 76, 76, 76, 76, 76, 76, 65, 65, 65, 65, 65, 65, 80, 80, 80, 80, 80, 80, 48, 48, 48, 48, 48, 48, 60, 60, 60, 60, 60, 60 ),
  `VO2_Ruhe [l·min⁻¹]` = c( 0.409079599507683, 0.409079599507683, 0.409079599507683, 0.409079599507683, 0.409079599507683, 0.409079599507683, 0.390487656996456, 0.390487656996456, 0.390487656996456, 0.390487656996456, 0.390487656996456, 0.390487656996456, 0.398544621667437, 0.398544621667437, 0.398544621667437, 0.398544621667437, 0.398544621667437, 0.398544621667437, 0.378852606093004, 0.378852606093004, 0.378852606093004, 0.378852606093004, 0.378852606093004, 0.378852606093004, 0.398278038452436, 0.398278038452436, 0.398278038452436, 0.398278038452436, 0.398278038452436, 0.398278038452436, 0.352584633817119, 0.352584633817119, 0.352584633817119, 0.352584633817119, 0.352584633817119, 0.352584633817119, 0.415642091286353, 0.415642091286353, 0.415642091286353, 0.415642091286353, 0.415642091286353, 0.415642091286353, 0.293734941358178, 0.293734941358178, 0.293734941358178, 0.293734941358178, 0.293734941358178, 0.293734941358178, 0.320312513138994, 0.320312513138994, 0.320312513138994, 0.320312513138994, 0.320312513138994, 0.320312513138994 ),
  `tau_on [s]` = c( 46.5, 44.9, 50.7, 34.8, 54.9, 46, 38.5, 34.6, 40.6, 39.6, 46.8, 49.8, 29, 28.4, 36, 30.6, 38.1, 48.5, 30.3, 28.1, 36.6, 28.8, 43.4, 36, 43.1, 39.1, 42.6, 47.2, 41.7, 56.8, 28.4, 25.2, 30.2, 32.5, 34.4, 40.4, 35.9, 32.6, 42, 36.7, 51.4, 45.4, 19.9, 26.6, 39.4, 25.9, 24.9, 29.8, 24.3, 20, 21.1, 25.2, 28.3, 25.7 ),
  `VO2_SS_percent [%]` = c( 78.05, 72.88, 80.93, 82.86, 95.41, 92.44, 78.78, 78.47, 81.67, 82.74, 88.98, 90.56, 72.62, 72.38, 84.49, 82.2, 89.86, 97.43, 82.89, 80.73, 89.87, 86.88, 94.87, 95.97, 74.8, 72.53, 81.88, 84.45, 90.17, 93.29, 73.56, 66.2, 72.77, 78.14, 82.38, 86.28, 65.42, 60.21, 69.7, 71.97, 81.32, 84.22, 78.2, 74.84, 82.97, 87.62, 95.96, 88.77, 67.8, 63.55, 70.47, 72.88, 78.3, 77.61 ),
  `R2_on` = c( 0.93, 0.95, 0.97, 0.96, 0.93, 0.96, 0.92, 0.96, 0.95, 0.96, 0.97, 0.98, 0.91, 0.91, 0.97, 0.98, 0.98, 0.99, 0.86, 0.89, 0.92, 0.93, 0.96, 0.93, 0.9, 0.96, 0.92, 0.91, 0.95, 0.95, 0.98, 0.9, 0.9, 0.95, 0.96, 0.95, 0.82, 0.82, 0.9, 0.93, 0.93, 0.95, 0.86, 0.9, 0.89, 0.93, 0.87, 0.93, 0.88, 0.89, 0.91, 0.95, 0.95, 0.96 ),
  `VO2_Brutto_SS [l·min⁻¹]` = c( 4.413, 4.121, 4.576, 4.685, 5.395, 5.227, 3.899, 3.884, 4.042, 4.095, 4.404, 4.482, 4.49, 4.475, 5.224, 5.082, 5.556, 6.024, 4.297, 4.185, 4.659, 4.504, 4.918, 4.975, 3.985, 3.864, 4.362, 4.499, 4.804, 4.97, 3.452, 3.107, 3.415, 3.667, 3.866, 4.049, 3.632, 3.343, 3.87, 3.996, 4.515, 4.676, 3.048, 2.917, 3.234, 3.415, 3.74, 3.46, 2.762, 2.589, 2.871, 2.969, 3.19, 3.162 ),
  `VO2_Netto_SS [l·min⁻¹]` = c( 4.004, 3.712, 4.167, 4.276, 4.986, 4.818, 3.509, 3.494, 3.652, 3.705, 4.014, 4.092, 4.042, 4.027, 4.776, 4.634, 5.108, 5.576, 3.858, 3.746, 4.22, 4.065, 4.479, 4.536, 3.587, 3.466, 3.964, 4.101, 4.406, 4.572, 3.182, 2.837, 3.145, 3.397, 3.596, 3.779, 3.279, 2.99, 3.517, 3.643, 4.162, 4.323, 2.776, 2.645, 2.962, 3.143, 3.468, 3.188, 2.384, 2.211, 2.493, 2.591, 2.812, 2.784 ),
  `delta_VO2 [l·min⁻¹]` = c( 2.122, 2.419, 2.199, 2.281, 3.547, 3.209, 1.98, 1.752, 1.925, 1.784, 2.864, 2.367, 2.554, 3.728, 3.155, 3.118, 3.882, 4.164, 2.211, 2.172, 2.616, 2.6, 3.331, 2.904, 1.763, 2.222, 2.177, 2.882, 3.114, 2.885, 1.962, 1.736, 1.832, 2.058, 2.163, 2.364, 1.869, 1.356, 2.15, 2.119, 2.318, 2.46, 1.627, 1.674, 1.841, 2.07, 2.756, 2.821, 1.385, 1.229, 1.39, 1.501, 1.561, 1.639 ),
  `VO2_on_Start [l·min⁻¹]` = c( 2.33, 1.747, 2.417, 2.446, 1.913, 2.077, 1.955, 2.164, 2.153, 2.344, 1.592, 2.158, 1.983, 0.815, 2.127, 2.021, 1.745, 1.937, 2.126, 2.053, 2.091, 1.952, 1.648, 2.124, 2.254, 1.683, 2.225, 1.67, 1.747, 2.138, 1.526, 1.403, 1.616, 1.646, 1.742, 1.728, 1.797, 2.012, 1.759, 1.916, 2.239, 2.261, 1.45, 1.274, 1.427, 1.383, 1.035, 0.69, 1.403, 1.383, 1.506, 1.495, 1.657, 1.553 ),
  `VO2_Referenz [l·min⁻¹]` = c( 0.997235294117647, 0.997235294117647, 0.997235294117647, 0.997235294117647, 0.997235294117647, 0.997235294117647, 0.934166666666667, 0.934166666666667, 0.934166666666667, 0.934166666666667, 0.934166666666667, 0.934166666666667, 0.9478, 0.9478, 0.9478, 0.9478, 0.9478, 0.9478, 0.999544642857143, 0.999544642857143, 0.999544642857143, 0.999544642857143, 0.999544642857143, 0.999544642857143, 0.898278038452436, 0.898278038452436, 0.898278038452436, 0.898278038452436, 0.898278038452436, 0.898278038452436, 0.769533333333333, 0.769533333333333, 0.769533333333333, 0.769533333333333, 0.769533333333333, 0.769533333333333, 0.852692307692308, 0.852692307692308, 0.852692307692308, 0.852692307692308, 0.852692307692308, 0.852692307692308, 0.772045454545454, 0.772045454545454, 0.772045454545454, 0.772045454545454, 0.772045454545454, 0.772045454545454, 0.896472222222222, 0.896472222222222, 0.896472222222222, 0.896472222222222, 0.896472222222222, 0.896472222222222 ),
  `VO2_avg [l·min⁻¹]` = c( 4.032, 3.768, 4.124, 4.28, 4.716, 4.705, 3.609, 3.626, 3.699, 3.783, 4.06, 3.944, 4.176, 3.944, 4.733, 4.652, 4.961, 5.401, 3.965, 3.902, 4.327, 4.195, 4.45, 4.554, 3.728, 3.61, 4.073, 4.095, 4.41, 4.485, 3.232, 2.946, 3.158, 3.407, 3.566, 3.743, 3.324, 3.125, 3.542, 3.637, 4.053, 4.185, 2.929, 2.802, 3.074, 3.208, 3.505, 3.222, 2.637, 2.436, 2.702, 2.802, 2.968, 2.96 ),
  `VO2_SS_avg [l·min⁻¹]` = c( 4.246, 3.979, 4.387, 4.517, 5.064, 4.996, 3.771, 3.767, 3.891, 3.956, 4.223, 4.198, 4.363, 4.29, 5.04, 4.916, 5.345, 5.766, 4.15, 4.075, 4.5, 4.373, 4.742, 4.804, 3.862, 3.746, 4.214, 4.324, 4.623, 4.71, 3.373, 3.031, 3.308, 3.571, 3.744, 3.928, 3.494, 3.237, 3.709, 3.846, 4.301, 4.454, 2.969, 2.871, 3.156, 3.309, 3.642, 3.361, 2.716, 2.531, 2.807, 2.9, 3.088, 3.077 ),
  `VCO2_avg [l·min⁻¹]` = c( 3.635, 3.415, 3.802, 3.915, 4.559, 4.292, 3.19, 3.335, 3.55, 3.623, 3.949, 3.77, 3.616, 3.635, 4.659, 4.475, 4.938, 5.473, 3.549, 3.615, 4.07, 4.033, 4.413, 4.33, 3.346, 3.341, 3.861, 3.945, 4.304, 4.232, 3.006, 2.621, 2.907, 3.287, 3.455, 3.692, 2.845, 2.79, 3.274, 3.309, 3.845, 3.918, 2.635, 2.493, 2.778, 3.028, 3.414, 2.994, 2.373, 2.21, 2.538, 2.731, 2.965, 2.893 ),
  `VCO2_SS_avg [l·min⁻¹]` = c( 3.873, 3.653, 4.111, 4.158, 4.954, 4.607, 3.366, 3.488, 3.767, 3.833, 4.16, 4.104, 3.791, 3.976, 4.99, 4.748, 5.323, 5.917, 3.711, 3.775, 4.282, 4.209, 4.724, 4.584, 3.555, 3.539, 4.085, 4.215, 4.543, 4.621, 3.137, 2.735, 3.055, 3.457, 3.657, 3.901, 2.998, 2.933, 3.452, 3.519, 4.135, 4.207, 2.708, 2.561, 2.935, 3.13, 3.55, 3.124, 2.459, 2.291, 2.642, 2.832, 3.09, 3.018 ),
  `RQ_avg` = c( 0.9, 0.9, 0.92, 0.9, 0.95, 0.9, 0.87, 0.91, 0.95, 0.94, 0.96, 0.94, 0.88, 0.88, 0.97, 0.95, 0.97, 1, 0.89, 0.92, 0.94, 0.95, 0.98, 0.94, 0.89, 0.93, 0.94, 0.95, 0.96, 0.94, 0.92, 0.9, 0.91, 0.96, 0.96, 0.98, 0.85, 0.89, 0.92, 0.9, 0.94, 0.92, 0.9, 0.9, 0.92, 0.94, 0.96, 0.92, 0.91, 0.9, 0.94, 0.97, 0.99, 0.97 ),
  `tau_on_min [min]` = c( 0.775, 0.748333333333333, 0.845, 0.58, 0.915, 0.766666666666667, 0.641666666666667, 0.576666666666667, 0.676666666666667, 0.66, 0.78, 0.83, 0.483333333333333, 0.473333333333333, 0.6, 0.51, 0.635, 0.808333333333333, 0.505, 0.468333333333333, 0.61, 0.48, 0.723333333333333, 0.6, 0.718333333333333, 0.651666666666667, 0.71, 0.786666666666667, 0.695, 0.946666666666667, 0.473333333333333, 0.42, 0.503333333333333, 0.541666666666667, 0.573333333333333, 0.673333333333333, 0.598333333333333, 0.543333333333333, 0.7, 0.611666666666667, 0.856666666666667, 0.756666666666667, 0.331666666666667, 0.443333333333333, 0.656666666666667, 0.431666666666667, 0.415, 0.496666666666667, 0.405, 0.333333333333333, 0.351666666666667, 0.42, 0.471666666666667, 0.428333333333333 ),
  `RQ_SS_avg` = c( 0.91, 0.91, 0.93, 0.91, 0.97, 0.92, 0.89, 0.92, 0.96, 0.96, 0.98, 0.97, 0.88, 0.91, 0.98, 0.96, 0.99, 1.02, 0.89, 0.92, 0.95, 0.96, 0.99, 0.95, 0.91, 0.94, 0.96, 0.97, 0.98, 0.97, 0.93, 0.9, 0.92, 0.96, 0.97, 0.99, 0.86, 0.9, 0.93, 0.91, 0.96, 0.94, 0.9, 0.9, 0.93, 0.94, 0.97, 0.92, 0.91, 0.9, 0.94, 0.97, 1, 0.98 ),
  `HR_percent [%]` = c( 87.37, 85.07, 88.4, 90.93, 93.12, 93.78, 88.05, 88.16, 92.21, 91.97, 95.39, 93.58, 84.41, 84.65, 88.64, 89.98, 91.12, 94.04, 89.19, 89.26, 92.25, 92.32, 93.98, 94.9, 86.65, 87.88, 89.95, 91.14, 93.96, 94.27, 90.9, 86.55, 87.59, 91.62, 91.13, 95.12, 86.77, 87.49, 90.8, 89.74, 93.35, 94.18, 80.64, 81.03, 84.87, 88.21, 91.54, 92.47, 91.92, 88.36, 90.08, 92.49, 93.58, 93.31 ),
  `t_delay [s]` = c( 19.9, 8.8, 19, 23.6, 22.7, 17.6, 17.1, 22.6, 24, 24.2, 16.2, 18.2, 17.2, 19, 15.7, 17.9, 13.2, 10.5, 22, 18.6, 13.7, 18.2, 10.7, 17.6, 14.2, 12, 15.1, 13.8, 15.1, 13.3, 14.3, 16.8, 18.9, 15.4, 20.3, 12.5, 21.5, 24.8, 20, 20.4, 21.8, 25.5, 19, 10.7, 4.3, 18, 19.8, 12.8, 14.2, 17.7, 18.1, 17.4, 21.7, 20.1 )
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
                     "t_delay [s]" = "t_delay [s]",
                     "R2_on" = "R2_on",
                     "delta_VO2 [l·min⁻¹]" = "delta_VO2 [l·min⁻¹]",
                     "VO2_avg [l·min⁻¹]" = "VO2_avg [l·min⁻¹]",
                     "VCO2_avg [l·min⁻¹]" = "VCO2_avg [l·min⁻¹]",
                     "RQ_avg" = "RQ_avg",
                     "VO2_Ruhe [l·min⁻¹]" = "VO2_Ruhe [l·min⁻¹]",
                     "VO2_SS_percent [%]" = "VO2_SS_percent [%]",
                     "HR_percent [%]" = "HR_percent [%]"
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
    } else if(col_name %in% c("P_Tot [W]", "tau_on [s]", "VO2_SS_percent [%]","t_delay [s]")) {
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
          columns = c("P_Tot [W]", "tau_on [s]", "VO2_SS_percent [%]", "t_delay [s]"),
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
                 hoverlabel = list(bgcolor = "#F5F5F5"),
                 showlegend = FALSE
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
