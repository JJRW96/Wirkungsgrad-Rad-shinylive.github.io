library(shiny)
library(plotly)
library(minpack.lm)
library(dplyr)
library(shinyjs)

# Beispieldaten ohne Nachbelastung 
t_data <- c(3505.1, 3506.8, 3508.3, 3509.8, 3511.4, 3513.2, 3514.7, 3516.2, 3517.5, 3518.9, 3520.3, 3521.6, 3523.0, 3524.7, 3526.0, 3527.2, 3528.7, 3530.0, 3531.4, 3532.8, 3534.3, 3535.7, 3537.0, 3538.4, 3539.9, 3541.2, 3542.8, 3544.3, 3545.6, 3547.0, 3548.4, 3550.1, 3551.7, 3553.2, 3554.6, 3556.1, 3557.5, 3560.2, 3561.8, 3564.3, 3566.0, 3567.8, 3569.6, 3571.5, 3573.3, 3575.3, 3577.1, 3579.1, 3581.0, 3582.7, 3584.2, 3585.7, 3587.1, 3588.5, 3589.9, 3591.4, 3593.3, 3594.7, 3596.2, 3597.8, 3599.5, 3601.0, 3602.4, 3603.9, 3605.6, 3607.1, 3608.6, 3610.4, 3612.0, 3613.4, 3616.4, 3618.5, 3620.2, 3621.8, 3623.8, 3626.1, 3628.4, 3630.5, 3632.5, 3635.1, 3637.2, 3639.6, 3641.7, 3643.7, 3648.5, 3652.4, 3654.4, 3656.7, 3658.9, 3660.1, 3662.3, 3664.6, 3667.2, 3669.5, 3671.4, 3673.6, 3675.5, 3677.1, 3679.0, 3681.0, 3683.0, 3685.0, 3687.1, 3689.0, 3691.1, 3692.9, 3695.6, 3697.2, 3699.4, 3701.8, 3704.0, 3706.0, 3708.1, 3709.9, 3711.8, 3715.0, 3717.5, 3719.6, 3721.7, 3724.0, 3726.2, 3728.2, 3730.1, 3732.0, 3734.3, 3736.0, 3738.4, 3739.2, 3741.1, 3743.1, 3745.4, 3746.5, 3748.7, 3750.9, 3753.1, 3755.3, 3757.6, 3762.7, 3765.6, 3768.1, 3770.2, 3772.4, 3774.8, 3777.5, 3779.9, 3782.3, 3785.3, 3787.7, 3789.8, 3792.2, 3794.9, 3796.5, 3798.2, 3800.6, 3802.4, 3805.3, 3808.0, 3810.9, 3813.0, 3815.1, 3818.0, 3820.4, 3822.3, 3824.5, 3826.6, 3829.8, 3833.1, 3835.3, 3838.1, 3840.1, 3844.9, 3847.1, 3848.5, 3850.0, 3855.3, 3859.2, 3861.9, 3863.6, 3865.6, 3867.8, 3869.8, 3872.0, 3874.4, 3876.1, 3878.4, 3881.1, 3882.9, 3884.9, 3886.3, 3888.9, 3890.6, 3892.8, 3895.1, 3897.1, 3898.8, 3900.7, 3903.3, 3905.3, 3907.7, 3910.3, 3913.8, 3916.9, 3919.2, 3921.5, 3923.6, 3926.4, 3928.3, 3930.3, 3932.2, 3934.5, 3936.9, 3940.6, 3942.8, 3944.4, 3946.6, 3948.7, 3950.6, 3954.0, 3956.5, 3960.8, 3962.3, 3964.1, 3966.5, 3969.3, 3971.7, 3975.9, 3977.9, 3982.9, 3985.3, 3987.5, 3990.2, 3992.7, 3995.5, 3997.9, 4000.1, 4004.3, 4006.5, 4009.6, 4012.7, 4015.5, 4017.9, 4020.0, 4024.2, 4026.1, 4028.8, 4031.2, 4033.7, 4035.6, 4037.6, 4041.3, 4044.9, 4047.2, 4049.9, 4054.3, 4057.3, 4060.2, 4062.4, 4065.5, 4068.0, 4070.5, 4072.6, 4074.5, 4076.4, 4078.1, 4080.3, 4082.8, 4084.9)
VO2_data <- c(4.3916, 4.33868, 4.29972, 4.26048, 4.25328, 4.22856, 4.18768, 4.16656, 4.118, 4.06268, 4.00452, 3.9486, 3.85204, 3.81448, 3.74268, 3.67572, 3.61052, 3.5468, 3.47268, 3.37056, 3.25912, 3.20024, 3.17932, 3.13008, 3.08712, 2.97636, 2.95052, 2.891, 2.82488, 2.72496, 2.6324, 2.55344, 2.45564, 2.38944, 2.32268, 2.2388, 2.16588, 2.09852, 1.99972, 1.9584, 1.87604, 1.81076, 1.74984, 1.70644, 1.66456, 1.64564, 1.59204, 1.53048, 1.47084, 1.4078, 1.40632, 1.34116, 1.29948, 1.26596, 1.23048, 1.22116, 1.19188, 1.19668, 1.16332, 1.153, 1.15444, 1.14004, 1.14976, 1.15704, 1.15192, 1.15128, 1.14088, 1.1228, 1.11288, 1.10284, 1.07976, 1.07524, 1.0516, 1.0596, 1.0478, 1.03064, 1.03404, 1.01436, 1.01996, 1.02592, 0.99788, 1.00092, 0.96684, 0.98036, 0.96396, 0.96056, 0.94256, 0.93244, 0.92228, 0.9068, 0.90336, 0.88984, 0.87968, 0.88288, 0.85096, 0.84, 0.83584, 0.83428, 0.82228, 0.82996, 0.8382, 0.831, 0.86264, 0.83748, 0.83664, 0.8596, 0.8622, 0.86216, 0.85472, 0.85588, 0.8374, 0.84796, 0.84512, 0.85952, 0.84476, 0.8158, 0.8146, 0.80048, 0.77164, 0.7736, 0.76624, 0.77088, 0.77992, 0.80724, 0.80412, 0.80112, 0.8068, 0.78476, 0.79444, 0.79072, 0.78772, 0.79112, 0.79316, 0.79112, 0.78536, 0.7758, 0.76944, 0.77496, 0.76204, 0.7836, 0.79964, 0.81584, 0.85324, 0.85488, 0.89108, 0.904, 0.89116, 0.87828, 0.86268, 0.8416, 0.84192, 0.83324, 0.8332, 0.83228, 0.83992, 0.8094, 0.79928, 0.80512, 0.811, 0.82524, 0.82652, 0.8246, 0.80908, 0.80036, 0.80152, 0.81324, 0.79372, 0.76376, 0.78108, 0.76332, 0.75032, 0.78964, 0.78632, 0.78452, 0.78252, 0.77932, 0.78952, 0.78984, 0.7888, 0.77604, 0.79476, 0.8106, 0.79136, 0.76844, 0.73712, 0.7374, 0.74148, 0.75156, 0.74932, 0.7288, 0.72612, 0.75984, 0.76316, 0.76744, 0.76004, 0.75084, 0.70948, 0.70316, 0.68816, 0.70184, 0.6916, 0.681, 0.67276, 0.66776, 0.66792, 0.65952, 0.64368, 0.67064, 0.67144, 0.68348, 0.67592, 0.65776, 0.63996, 0.64432, 0.6382, 0.62112, 0.58896, 0.6054, 0.57796, 0.58432, 0.59236, 0.60632, 0.62372, 0.63128, 0.63308, 0.6444, 0.6382, 0.64336, 0.64684, 0.65508, 0.66076, 0.63596, 0.60768, 0.62748, 0.62472, 0.6284, 0.64236, 0.66068, 0.65004, 0.67312, 0.67836, 0.69416, 0.66368, 0.65028, 0.629, 0.62796, 0.60816, 0.61548, 0.61224, 0.62336, 0.61552, 0.62804, 0.63852, 0.64872, 0.641, 0.633, 0.64456, 0.66328, 0.6502, 0.65676, 0.66388, 0.6542, 0.63828, 0.64624, 0.63272, 0.65072, 0.64964)

# Beispieldaten mit 50 Watt Nachbelastung 
t_data_50W <- c(2552.6, 2554.0, 2555.6, 2557.1, 2558.7, 2560.3, 2562.0, 2563.7, 2565.4, 2567.0, 2568.6, 2570.0, 2571.5, 2572.8, 2574.6, 2576.1, 2577.5, 2579.0, 2580.5, 2582.7, 2584.2, 2586.3, 2587.7, 2589.0, 2590.8, 2592.2, 2594.2, 2595.8, 2597.4, 2599.1, 2600.6, 2602.1, 2603.6, 2605.3, 2606.9, 2608.5, 2610.3, 2612.6, 2613.9, 2615.6, 2617.2, 2618.8, 2621.5, 2623.0, 2625.5, 2627.3, 2628.9, 2630.3, 2631.9, 2633.3, 2635.0, 2636.5, 2638.2, 2639.9, 2641.5, 2643.0, 2644.9, 2646.6, 2648.3, 2649.8, 2651.3, 2652.9, 2654.5, 2656.3, 2658.2, 2660.1, 2661.9, 2663.7, 2665.4, 2667.0, 2669.1, 2670.9, 2672.6, 2674.4, 2676.1, 2678.0, 2679.9, 2681.6, 2683.1, 2684.7, 2685.7, 2687.1, 2690.9, 2692.6, 2695.9, 2697.7, 2698.9, 2701.2, 2703.0, 2705.0, 2706.7, 2708.5, 2710.3, 2711.8, 2713.4, 2715.1, 2716.7, 2718.2, 2719.8, 2721.3, 2723.2, 2725.4, 2727.1, 2729.0, 2730.8, 2732.4, 2733.9, 2735.4, 2737.4, 2739.1, 2741.2, 2743.1, 2746.2, 2747.8, 2749.5, 2751.3, 2753.0, 2755.0, 2757.3, 2759.5, 2761.7, 2763.6, 2765.7, 2767.8, 2769.3, 2770.8, 2772.2, 2774.3, 2776.1, 2777.7, 2779.3, 2781.1, 2783.0, 2786.2, 2787.9, 2789.5, 2791.4, 2793.2, 2795.0, 2796.8, 2798.5, 2800.4, 2802.0, 2804.0, 2805.9, 2807.8, 2809.5, 2812.5, 2814.2, 2816.4, 2818.5, 2821.0, 2822.8, 2824.7, 2826.5, 2828.5, 2830.5, 2831.9, 2833.4, 2835.0, 2836.6, 2838.8, 2840.5, 2842.4, 2844.1, 2846.2, 2848.4, 2851.5, 2853.3, 2855.3, 2858.8, 2861.1, 2863.4, 2865.8, 2868.2, 2870.2, 2872.7, 2875.1, 2877.3, 2879.4, 2883.0, 2884.8, 2887.1, 2888.6, 2891.0, 2893.1, 2895.0, 2896.8, 2898.9, 2900.8, 2903.5, 2905.3, 2907.2, 2909.2, 2911.5, 2913.4, 2915.6, 2917.4, 2919.2, 2921.4, 2923.8, 2925.5, 2927.5, 2929.7, 2932.9, 2935.1, 2937.1, 2939.6, 2941.4, 2943.8, 2945.9, 2948.6, 2950.7, 2952.7, 2956.5, 2958.8, 2961.0, 2962.6, 2964.9, 2966.5, 2967.7, 2969.9, 2971.3, 2973.1, 2975.5, 2977.4, 2979.5, 2981.4, 2983.5, 2985.5, 2988.0, 2990.2, 2992.4, 2994.5, 2998.4, 3000.1, 3002.2, 3004.5, 3007.0, 3008.6, 3010.9, 3013.3, 3015.8, 3017.5, 3019.1, 3021.1, 3023.5, 3025.5, 3027.8, 3029.6, 3030.7, 3033.6, 3035.9, 3038.1, 3040.4, 3042.4, 3046.1, 3048.6, 3050.9, 3053.4, 3055.2, 3057.3, 3059.2, 3061.3, 3063.1, 3065.5, 3067.7, 3069.6, 3071.9, 3074.3, 3076.4, 3078.2, 3080.2, 3082.2, 3084.0, 3086.9, 3089.5, 3092.9, 3095.6, 3098.2, 3100.8, 3103.1, 3106.6, 3108.5, 3110.5, 3113.1, 3115.6, 3121.1, 3124.1, 3127.0, 3129.7, 3132.6, 3134.2, 3136.4, 3138.8, 3141.6, 3144.1, 3146.2, 3149.5, 3152.2, 3154.6, 3157.2, 3159.6)
VO2_data_50W <- c(4.4001, 4.3817, 4.3121, 4.3409, 4.3439, 4.3299, 4.2480, 4.1318, 4.1017, 4.0326, 3.9826, 3.9325, 3.8651, 3.8096, 3.7446, 3.6772, 3.6132, 3.5336, 3.4854, 3.4446, 3.3776, 3.3045, 3.2331, 3.1704, 3.0984, 3.0216, 2.9686, 2.9386, 2.8810, 2.8061, 2.7353, 2.7127, 2.7239, 2.6456, 2.6377, 2.5957, 2.5540, 2.5138, 2.4619, 2.4145, 2.4251, 2.3901, 2.3788, 2.3462, 2.2943, 2.2786, 2.2648, 2.2418, 2.2427, 2.2392, 2.2386, 2.1912, 2.1595, 2.1360, 2.1111, 2.1224, 2.0777, 2.0514, 2.0310, 1.9780, 1.9727, 1.9440, 1.9551, 1.9516, 1.9462, 1.9169, 1.8937, 1.8818, 1.8560, 1.8333, 1.8018, 1.8064, 1.8136, 1.7820, 1.7528, 1.7488, 1.7494, 1.7520, 1.7599, 1.7941, 1.7951, 1.8142, 1.8182, 1.8387, 1.8880, 1.8747, 1.8848, 1.8750, 1.8730, 1.8576, 1.8532, 1.8552, 1.8566, 1.8779, 1.9174, 1.9636, 1.9640, 1.9728, 1.9767, 2.0114, 1.9866, 2.0298, 2.0401, 2.0414, 2.0417, 2.0099, 2.0261, 1.9837, 1.9544, 1.9402, 1.9313, 1.9273, 1.9265, 1.9295, 1.9441, 1.9727, 1.9943, 2.0044, 1.9720, 1.9654, 1.9225, 1.9068, 1.8688, 1.8623, 1.8394, 1.8563, 1.8187, 1.7995, 1.7750, 1.7498, 1.7746, 1.7715, 1.8122, 1.8389, 1.8352, 1.8090, 1.8296, 1.8238, 1.8328, 1.8290, 1.8123, 1.7960, 1.7923, 1.7896, 1.7622, 1.7592, 1.7674, 1.7858, 1.7950, 1.8137, 1.8496, 1.8495, 1.8664, 1.8851, 1.8865, 1.8747, 1.8502, 1.8631, 1.8322, 1.8341, 1.8650, 1.8490, 1.8762, 1.8531, 1.8437, 1.8368, 1.8438, 1.8350, 1.8540, 1.8645, 1.8486, 1.8179, 1.8006, 1.7843, 1.7765, 1.7372, 1.7319, 1.7322, 1.7202, 1.7160, 1.7033, 1.6818, 1.6498, 1.6551, 1.6361, 1.6277, 1.6271, 1.6044, 1.6204, 1.6176, 1.5965, 1.6116, 1.5948, 1.5813, 1.6099, 1.6333, 1.6588, 1.6799, 1.6902, 1.6858, 1.6819, 1.6689, 1.6550, 1.6356, 1.6488, 1.6722, 1.6864, 1.6533, 1.6580, 1.6782, 1.6916, 1.6921, 1.6927, 1.6946, 1.7046, 1.7356, 1.6985, 1.7132, 1.7104, 1.6868, 1.6894, 1.6880, 1.6834, 1.6632, 1.6246, 1.6189, 1.6194, 1.6530, 1.6906, 1.6982, 1.6821, 1.6484, 1.7026, 1.7012, 1.6998, 1.6902, 1.6894, 1.6715, 1.5961, 1.6080, 1.5906, 1.5955, 1.5864, 1.6033, 1.5882, 1.5746, 1.5551, 1.5417, 1.5668, 1.6209, 1.6475, 1.6627, 1.6290, 1.6070, 1.5852, 1.5877, 1.6354, 1.5954, 1.5910, 1.5767, 1.5835, 1.6019, 1.6368, 1.6969, 1.6874, 1.6751, 1.6753, 1.6987, 1.6867, 1.6964, 1.7220, 1.7142, 1.7196, 1.7078, 1.6907, 1.6628, 1.6343, 1.6318, 1.6129, 1.6256, 1.6123, 1.6096, 1.6335, 1.6262, 1.6195, 1.6204, 1.5709, 1.5486, 1.5559, 1.5600, 1.5710, 1.5819, 1.5609, 1.5808, 1.5985, 1.5951, 1.6267, 1.6258, 1.5771, 1.5974, 1.6163, 1.6230, 1.6052)

# Normalisierung der t_data Werte auf 0
t_data <- t_data - t_data[1]
t_data_50W <- t_data_50W - t_data_50W[1]

# Modellfunktion
model_function <- function(t_s, A, TauA, B, TauB, C, t_delay) {
  A * exp(-(t_s - t_delay) / TauA) + B * exp(-(t_s - t_delay) / TauB) + C
}

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .reduced-margin { margin-bottom: 5px !important; }
      .radio-buttons-compact { margin-top: -20px; }
    "))
  ),
  titlePanel("EPOC-Modellfunktion"),
  fluidRow(
    column(3,
           style = "height: 90vh; overflow-y: auto;",
           tags$h4(tags$strong("Modellparameter:")),
           sliderInput("A", "A", min = 0.0, max = 6, value = 2.2, step = 0.01),
           sliderInput("TauA", "TauA", min = 5, max = 90, value = 35, step = 0.10),
           sliderInput("B", "B", min = 0.0, max = 5, value = 0.8, step = 0.01),
           sliderInput("TauB", "TauB", min = 0.0, max = 1800, value = 180, step = 0.10),
           sliderInput("C", "C", min = 0.0, max = 3.0, value = 0.30, step = 0.01),
           sliderInput("O2_Store", "O2-Speicher [l]", min = 0, max = 1, value = 0.4, step = 0.01),
           sliderInput("t_delay", "Zeitverzögerung [s]", min = 0, max = 300, value = 0, step = 1),
           sliderInput("VO2_Ruhe", "VO2 Ruhe [l · min^-1]", min = 0.0, max = 1.0, value = 0.3, step = 0.001),
           sliderInput("VO2_Referenz", "VO2 Referenz (50 Watt) [l · min^-1]", min = 0.0, max = 2.0, value = 1.0, step = 0.001),
           br(), 
           tags$h4(tags$strong("Beispieldaten einfügen:")),
           actionButton("show_data_ohne", "ohne Nachbelastung"),
           br(), 
           actionButton("show_data_50W", "50 Watt Nachbelastung"),
           br(), br(),
           fileInput("file_upload", "CSV-Datei hochladen", accept = ".csv"),
           tags$h4(tags$strong("Modelanpassung:")),
           actionButton("fit_all", "nlsLM - Fit"),
           tags$h4(class = "reduced-margin", "Optionen:"),
           div(class = "radio-buttons-compact",
               radioButtons("fit_mit", "", 
                            choices = c("mit VO2 Ruhe" = "ruhe", "mit VO2 Referenz" = "referenz"),
                            selected = "referenz")
           ),
           div(class = "radio-buttons-compact",
               radioButtons("fit_steps", "", 
                            choices = c("3 Schritte" = "three_steps", "1 Schritt" = "one_step"),
                            selected = "three_steps")
           ),
           h4("Schrittweise:"),
           actionButton("fit_tau", "1. Fit: Tau"), br(),
           actionButton("fit_slow", "2. Fit: EPOC Slow"), br(),
           actionButton("fit_full", "3. Fit: EPOC Fast"),
           br(), br(),
           sliderInput("ruhe_sim_range", "Zeitraum der simulierten Ruhewerte [s]:",
                       min = 1200, max = 7200, value = c(3600, 4200), step = 100),
           actionButton("toggle_view", "Ruhe_sim anzeigen"),
           br(), br(),
           tags$h4(tags$strong("Berechnung - Ruhesauerstoffaufnahme:")),
           radioButtons("geschlecht", "Geschlecht:", choices = c("Männlich", "Weiblich")),
           sliderInput("koerpermasse", "Körpermasse [kg]:", min = 40, max = 150, value = 90),
           sliderInput("koerperlaenge", "Körperlänge [cm]:", min = 140, max = 220, value = 193),
           sliderInput("alter", "Alter [Jahre]:", min = 18, max = 100, value = 27),
           sliderInput("rq", "RQ:", min = 0.7, max = 1.0, value = 0.77, step = 0.01),
           actionButton("berechne_vo2_ruhe", "VO2 Ruhe berechnen")
    ),
    column(8,
           fluidRow(
             column(12, plotlyOutput("plot"))
           ),
           fluidRow(
             column(12, htmlOutput("instructions"))
           )
    )
  )
)

server <- function(input, output, session) {
  show_data <- reactiveVal(FALSE)
  current_data <- reactiveVal(NULL)
  uploaded_data <- reactiveVal(NULL)
  tau_estimate <- reactiveVal(NULL)
  slow_estimates <- reactiveVal(NULL)
  show_full_view <- reactiveVal(FALSE)
  max_ruhe_t_s <- reactiveVal(NULL)
  ruhe_sim <- reactiveVal(NULL)
  fit_mode <- reactive({
    input$fit_steps
  })
  
  observeEvent(input$show_data_ohne, {
    show_data(TRUE)
    current_data(data.frame(t_s = t_data, VO2_t = VO2_data))
  })
  
  observeEvent(input$show_data_50W, {
    show_data(TRUE)
    current_data(data.frame(t_s = t_data_50W, VO2_t = VO2_data_50W))
  })
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    df <- read.csv(input$file_upload$datapath)
    
    if ("t_s" %in% names(df) && "VO2_t" %in% names(df)) {
      if (df$t_s[1] != 0.0) {
        df$t_s <- df$t_s - df$t_s[1]
      }
      
      uploaded_data(df)
      current_data(df)
      show_data(TRUE)
    } else {
      showModal(modalDialog(
        title = "Invalid CSV",
        "Die CSV-Datei muss 't_s' und 'VO2_t' als Spaltennamen beinhalten.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Funktion zur Berechnung des Grundumsatzes
  berechne_grundumsatz <- function(geschlecht, masse, laenge, alter) {
    if (geschlecht == "Männlich") {
      return(66.5 + (13.75 * masse) + (5.003 * laenge) - (6.775 * alter))
    } else {
      return(655.1 + (9.563 * masse) + (1.850 * laenge) - (4.676 * alter))
    }
  }
  
  # Funktion zur Berechnung des RMR
  berechne_rmr <- function(grundumsatz, rq, geschlecht) {
    ka <- 19.946  # Annahme für RQ = 0.77, passen Sie dies an, wenn nötig
    faktor <- if(geschlecht == "Männlich") 1.287 else 1.278
    return((grundumsatz / (24 * 60 * ka)) * 4.1868 * faktor)
  }
  
  # Reaktion auf den Klick des "VO2,Ruhe berechnen" Buttons
  observeEvent(input$berechne_vo2_ruhe, {
    grundumsatz <- berechne_grundumsatz(
      input$geschlecht,
      input$koerpermasse,
      input$koerperlaenge,
      input$alter
    )
    
    rmr <- berechne_rmr(grundumsatz, input$rq, input$geschlecht)
    
    # Aktualisieren Sie den VO2_Ruhe Slider mit dem berechneten Wert
    updateSliderInput(session, "VO2_Ruhe", value = round(rmr, 3))
    
    # Zeigen Sie eine Benachrichtigung mit dem berechneten Wert an
    showNotification(paste("Berechnete Ruhesauerstoffaufnahme:", round(rmr, 3), "l/min"), type = "message")
  })
  
  observeEvent(input$fit_tau, {
    if (fit_mode() == "three_steps" && show_data()) {
      Beispieldaten <- current_data()
      if (is.null(Beispieldaten)) return(p)
      
      t_delay <- isolate(input$t_delay)
      C <- isolate(input$VO2_Ruhe)
      
      # Datenfilterung und Zeitverschiebung
      filtered_data <- Beispieldaten %>%
        filter(t_s >= t_delay) %>%
        mutate(t_s = t_s - t_delay)
      
      model_tau <- nlsLM(VO2_t ~ x * exp(-t_s/Tau) + C, 
                         data = filtered_data,
                         start = list(x = max(filtered_data$VO2_t), Tau = 45, C = C),
                         lower = c(x = 0, Tau = 10, C = 0),
                         upper = c(x = Inf, Tau = 600, C = Inf),
                         control = nls.lm.control(maxiter = 1024, ftol = 1e-10, ptol = 1e-10))
      
      tau_estimate(round(as.list(coef(model_tau))$Tau, 1))
      
      updateSliderInput(session, "TauA", value = tau_estimate())
      updateSliderInput(session, "A", value = as.list(coef(model_tau))$x)
      updateSliderInput(session, "C", value = as.list(coef(model_tau))$C)
      
      # Set B and TauB sliders to 0
      updateSliderInput(session, "B", value = 0)
      updateSliderInput(session, "TauB", value = 0)
    }
  })
  
  observeEvent(input$fit_slow, {
    if (fit_mode() == "three_steps") {
      req(tau_estimate())
      if (show_data()) {
        Beispieldaten <- current_data()
        
        VO2_Basis <- if(input$fit_mit == "ruhe") input$VO2_Ruhe else input$VO2_Referenz
        t_delay <- isolate(input$t_delay)
        max_VO2 <- max(Beispieldaten$VO2_t) * 1.1
        
        tau2 <- 2 * tau_estimate()
        tau8 <- 8 * tau_estimate()
        last_data_point <- max(Beispieldaten$t_s)
        
        actual_data_count <- sum(Beispieldaten$t_s >= tau2)
        
        ruhe_sim_start <- input$ruhe_sim_range[1]
        ruhe_sim_end <- input$ruhe_sim_range[2]
        
        simulated_time_points <- seq(ruhe_sim_start, ruhe_sim_end, length.out = actual_data_count)
        
        ruhe_sim_data <- data.frame(
          t_s = simulated_time_points,
          VO2_t = rep(VO2_Basis, actual_data_count)  # Hier VO2_Basis statt VO2_Ruhe verwenden
        )
        
        ruhe_sim(ruhe_sim_data)
        max_ruhe_t_s(max(ruhe_sim_data$t_s))
        
        Beispieldaten_extended <- rbind(Beispieldaten, ruhe_sim_data)
        
        filtered_data <- Beispieldaten_extended %>% 
          filter(t_s >= t_delay) %>%
          mutate(t_s = t_s - t_delay) %>%
          filter(t_s >= tau2)
        
        model_slow <- nlsLM(VO2_t ~ B * exp(-t_s/TauB) + VO2_Basis,
                            data = filtered_data,
                            start = list(B = 0.5, TauB = 540),
                            lower = c(B = 0.1, TauB = 90),
                            upper = c(B = (max_VO2 * 0.5), TauB = 3600),
                            control = nls.lm.control(maxiter = 1024, ftol = 1e-10, ptol = 1e-10))
        
        slow_estimates_list <- list(
          B = as.numeric(coef(model_slow)["B"]),
          TauB = round(as.numeric(coef(model_slow)["TauB"]), 1),
          C = VO2_Basis
        )
        
        slow_estimates(slow_estimates_list)
        
        updateSliderInput(session, "B", value = slow_estimates_list$B)
        updateSliderInput(session, "TauB", value = slow_estimates_list$TauB)
        updateSliderInput(session, "C", value = VO2_Basis)  # Hier VO2_Basis statt VO2_Ruhe verwenden
      }
    }
  })
  
  observeEvent(input$fit_full, {
    if (fit_mode() == "three_steps") {
      req(tau_estimate(), slow_estimates())
      if (show_data()) {
        Beispieldaten <- current_data()
        
        slow_est <- slow_estimates()
        t_delay <- isolate(input$t_delay)
        
        if (is.null(tau_estimate()) || is.null(slow_est)) {
          showNotification("Einige Schätzwerte fehlen. Bitte führen Sie Schritt 1 und 2 erneut aus.", type = "error")
          return()
        }
        
        start_params <- list(
          A = isolate(input$A),
          TauA = tau_estimate()
        )
        
        # Definiere eine Funktion mit festen Werten für B, TauB und C
        model_func <- function(t_s, A, TauA) {
          A * exp(-t_s/TauA) + slow_est$B * exp(-t_s/slow_est$TauB) + slow_est$C
        }
        
        # Berechne 2tau
        tau2 <- 2 * tau_estimate()
        actual_data_count <- sum(Beispieldaten$t_s >= tau2)
        
        # Datenfilterung und Zeitverschiebung
        Beispieldaten_gefiltert <- Beispieldaten %>%
          filter(t_s >= t_delay) %>%
          mutate(t_s = t_s - t_delay) %>%
          filter(t_s < 3600)  # Entferne simulierte Ruhewerte
        
        # Erstelle Gewichte basierend auf t_s
        weights <- ifelse(Beispieldaten_gefiltert$t_s <= tau2, 1, 1)
        
        tryCatch({
          model_full <- nlsLM(VO2_t ~ model_func(t_s, A, TauA),
                              data = Beispieldaten_gefiltert,
                              start = list(A = 2.5, TauA = 42),
                              lower = c(A = 1.0, TauA = 15),
                              upper = c(A = 4.0, TauA = 90),
                              weights = weights,
                              control = nls.lm.control(maxiter = 1024, ftol = 1e-10, ptol = 1e-10))
          
          full_estimates <- list(
            A = as.numeric(coef(model_full)["A"]),
            TauA = round(as.numeric(coef(model_full)["TauA"]), 1)
          )
          
          updateSliderInput(session, "A", value = full_estimates$A)
          updateSliderInput(session, "TauA", value = full_estimates$TauA)
          
          showNotification("Fitting erfolgreich abgeschlossen", type = "message")
        }, error = function(e) {
          showNotification(paste("Fehler beim Fitting:", e$message), type = "error")
        })
      }
    }
  })
  
  observeEvent(input$toggle_view, {
    show_full_view(!show_full_view())
  })
  
  observeEvent(input$fit_all, {
    if (show_data()) {
      if (fit_mode() == "three_steps") {
        # Trigger fit_tau
        shinyjs::click("fit_tau")
        
        # Warte kurz, bevor der nächste Fit ausgeführt wird
        Sys.sleep(0.5)
        
        # Trigger fit_slow
        shinyjs::click("fit_slow")
        
        # Warte kurz, bevor der nächste Fit ausgeführt wird
        Sys.sleep(0.5)
        
        # Trigger fit_full
        shinyjs::click("fit_full")
        
        showNotification("Alle Fits wurden nacheinander durchgeführt.", type = "message")
      } else if (fit_mode() == "one_step") {
        Beispieldaten <- current_data()
        
        t_delay <- isolate(input$t_delay)
        C <- if(input$fit_mit == "ruhe") isolate(input$VO2_Ruhe) else isolate(input$VO2_Referenz)
        
        best_fit <- NULL
        best_rss <- Inf
        
        withProgress(message = 'Fitting in progress', value = 0, {
          for (i in 1:100) {
            # Randomisierte Startwerte
            A_start <- runif(1, 1.5, 6)
            TauA_start <- runif(1, 20, 90)
            B_start <- runif(1, 0.3, 1.0)
            TauB_start <- runif(1, 300, 900)
            
            start_values <- list(A = A_start, TauA = TauA_start, B = B_start, TauB = TauB_start)
            
            model_equation <- VO2_t ~ A * exp(-(t_s - t_delay) / TauA) + B * exp(-(t_s - t_delay) / TauB) + C
            
            fit <- try(nlsLM(model_equation,
                             data = Beispieldaten,
                             start = start_values,
                             lower = c(A = 1.5, TauA = 20, B = 0.3, TauB = 90),
                             upper = c(A = 6, TauA = 90, B = 3.0, TauB = 900),
                             control = nls.control(maxiter = 1024)), silent = TRUE)
            
            if (!inherits(fit, "try-error")) {
              rss <- sum(residuals(fit)^2)
              if (rss < best_rss) {
                best_rss <- rss
                best_fit <- fit
              }
            }
            
            incProgress(1/100, detail = paste("Iteration", i))
          }
        })
        
        if (!is.null(best_fit)) {
          params <- as.list(coef(best_fit))
          updateSliderInput(session, "A", value = params$A)
          updateSliderInput(session, "TauA", value = params$TauA)
          updateSliderInput(session, "B", value = params$B)
          updateSliderInput(session, "TauB", value = params$TauB)
          
          showNotification("Fitting completed successfully!", type = "message")
        } else {
          showModal(modalDialog(
            title = "nlsLM fehlgeschlagen",
            "Die nicht-lineare Regression mittels Levenberg-Marquardt-Algorithmus konnte nicht erfolgreich durchgeführt werden.",
            easyClose = TRUE,
            footer = NULL
          ))
        }
      }
    } else {
      showNotification("Bitte fügen Sie zuerst Daten ein.", type = "warning")
    }
  })
  
  output$plot <- renderPlotly({
    A <- input$A
    TauA <- input$TauA
    B <- input$B
    TauB <- input$TauB
    C <- if(input$fit_mit == "ruhe") input$VO2_Ruhe else input$VO2_Referenz
    O2_Store <- input$O2_Store
    t_delay <- input$t_delay
    VO2_Basis <- if(input$fit_mit == "ruhe") input$VO2_Ruhe else input$VO2_Referenz
    
    # Bestimmen des x-Achsen-Bereichs
    x_range <- if (show_full_view()) {
      c(0, max_ruhe_t_s())
    } else {
      max_x_value <- max(600, if (!is.null(current_data())) max(current_data()$t_s) else 600)
      c(0, max_x_value * 1.1)  # Erhöhung um 10%
    }
    
    max_x <- max(max_ruhe_t_s(), 600 + 0.5 * TauB, max(t_data))
    
    Beispieldaten <- current_data()
    
    if (!is.null(current_data())) {
      max_y <- max((A + B + C) * 1.1, max(current_data()$VO2_t) * 1.1)
    } else {
      max_y <- (A + B + C) * 1.1
    }
    
    t_s <- seq(0, max_x, by = 1)
    
    model_values <- A * exp(-t_s / TauA) + B * exp(-t_s / TauB) + C
    model_fast <- A * exp(-t_s / TauA)
    model_slow <- B * exp(-t_s / TauB)
    model_ruhe <- rep(C, length(t_s))
    
    model_fast_func <- function(t_s) A * exp(-t_s / TauA)
    integrated_model_fast <- integrate(model_fast_func, lower = 0, upper = max(t_s))
    
    VO2_fast <- integrated_model_fast$value / 60
    CE_max <- 21.1307796
    WPCR <- VO2_fast * CE_max
    WPCR_corrected <- ifelse(VO2_fast < O2_Store, 0, (VO2_fast * CE_max) - (O2_Store * CE_max))
    
    cumulative_area <- cumsum(A * exp(-t_s / TauA)) * diff(t_s)[1]
    O2_Store_index <- which.min(abs(cumulative_area - O2_Store * 60))
    O2_Store_x <- t_s[O2_Store_index]
    
    last_data_point <- if (!is.null(current_data())) max(current_data()$t_s) else 0
    
    eq_text <- sprintf("V̇O₂ = %.2f * e<sup>-t / %.1f</sup> + %.2f * e<sup>-t / %.1f</sup> + %.2f", 
                       A, TauA, B, TauB, C)
    
    p <- plot_ly() %>%
      add_trace(x = ~t_s, y = ~model_values, type = 'scatter', mode = 'lines', 
                name = 'Modellfunktion', line = list(color = '#EF6F6A')) %>%
      add_trace(x = ~t_s, y = ~model_fast, type = 'scatter', mode = 'lines', 
                name = 'EPOC<sub>fast</sub>', line = list(color = '#42BA97')) %>%
      add_trace(x = ~t_s, y = ~model_slow, type = 'scatter', mode = 'lines', 
                name = 'EPOC<sub>slow</sub>', line = list(color = '#BB7693')) %>%
      add_trace(x = ~t_s, y = ~model_ruhe, type = 'scatter', mode = 'lines', 
                name = 'C', line = list(color = '#1CADE4')) %>%
      add_ribbons(x = ~t_s, ymin = 0, ymax = ~model_fast, 
                  name = 'EPOC<sub>fast,Integriert</sub>', fillcolor = 'rgba(66,186,151,0.5)', 
                  line = list(color = "rgba(0,0,0,0)")) %>%
      add_ribbons(x = ~t_s, ymin = 0, ymax = ~model_fast, 
                  data = data.frame(t_s = t_s[1:O2_Store_index], model_fast = model_fast[1:O2_Store_index]),
                  fillcolor = 'rgba(0,131,143,0.3)', line = list(color = "rgba(0,0,0,0)", dash = "dash"), 
                  name = 'O<sub>2</sub>-Speicher') %>%
      add_trace(x = c(O2_Store_x, O2_Store_x), y = c(0, max(model_fast[O2_Store_index])),
                type = 'scatter', mode = 'lines', 
                line = list(color = "#00838F", width = 1, dash = "dash"),
                name = 'O2 Store Linie', showlegend = FALSE) %>%
      add_segments(x = 2 * TauA, xend = 2 * TauA, y = 0, yend = max_y,
                   line = list(color = "gray", width = 0.5, dash = "dash"),
                   showlegend = FALSE) %>%
      layout(title = "EPOC-Modellfunktion",
             margin = list(t = 40),
             xaxis = list(title = "Zeit [s]", range = x_range, autorange = FALSE),
             yaxis = list(title = "V̇O<sub>2</sub> [l·min<sup>-1</sup>]", range = c(0, max_y)),
             annotations = list(
               list(
                 x = 2 * TauA, y = max_y * 0.75, text = sprintf("2tau: %.1f", 2 * TauA), showarrow = FALSE, xanchor = "left", yanchor = "bottom",
                 textangle = -90, font = list(size = 11, color = "darkgrey")
               ),
               list(
                 x = max_x * 0.10, 
                 y = max_y * 0.85,
                 text = paste("W<sub>PCR</sub>:", round(WPCR, 2), "kJ"),
                 showarrow = FALSE,
                 xanchor = 'left',
                 yanchor = 'bottom',
                 font = list(
                   size = 12,
                   color = "black"
                 )
               ),
               list(
                 x = max_x * 0.10, 
                 y = max_y * 0.75,
                 text = paste("W<sub>PCR, korrigiert</sub>:", round(WPCR_corrected, 2), "kJ"),
                 showarrow = FALSE,
                 xanchor = 'left',
                 yanchor = 'bottom',
                 font = list(
                   size = 12,
                   color = "black"
                 )
               ),
               list(
                 x = max_x * 0.10, 
                 y = max_y * 0.95,
                 text = eq_text,
                 showarrow = FALSE,
                 xanchor = 'left',
                 yanchor = 'bottom',
                 font = list(
                   size = 12,
                   color = "black"
                 )
               )
             ))
    
    if (show_data()) {
      Beispieldaten <- current_data()
      if (!is.null(Beispieldaten)) {
        # Datenfilterung und Zeitverschiebung
        Beispieldaten <- Beispieldaten %>%
          filter(t_s >= t_delay) %>%
          mutate(t_s = t_s - t_delay) %>%
          filter(t_s < 3600)  # Entferne simulierte Ruhewerte
        
        last_data_point <- max(Beispieldaten$t_s)
        
        # Füge last_data_point Linie und Annotation hinzu
        p <- p %>% add_segments(x = last_data_point, xend = last_data_point, y = 0, yend = max_y,
                                line = list(color = "gray", width = 0.5, dash = "dash"),
                                showlegend = FALSE)
        
        p$x$layout$annotations <- c(p$x$layout$annotations,
                                    list(
                                      list(
                                        x = last_data_point, y = max_y * 0.75, text = sprintf("t<sub>data_last</sub>: %.1f", last_data_point), showarrow = FALSE, xanchor = "left", yanchor = "bottom",
                                        textangle = -90, font = list(size = 11, color = "darkgrey")
                                      )
                                    ))
        
        model_values_at_data <- A * exp(-Beispieldaten$t_s / TauA) + B * exp(-Beispieldaten$t_s / TauB) + C
        
        data_subset <- Beispieldaten
        
        ss_res <- sum((data_subset$VO2_t - model_values_at_data)^2, na.rm = TRUE)
        ss_tot <- sum((data_subset$VO2_t - mean(data_subset$VO2_t, na.rm = TRUE))^2, na.rm = TRUE)
        r_squared <- 1 - (ss_res / ss_tot)
        
        p <- p %>%
          add_trace(data = Beispieldaten, x = ~t_s, y = ~VO2_t, type = 'scatter', mode = 'markers+lines',
                    name = 'V̇O<sub>2</sub>', 
                    marker = list(color = 'rgba(38, 131, 198, 0.9)', size = 5.0),
                    line = list(color = 'rgba(38, 131, 198, 1.0)', width = 0.65, dash = '4 4'))
        
        if (show_full_view()) {
          ruhe_sim <- data.frame(
            t_s = seq(input$ruhe_sim_range[1], input$ruhe_sim_range[2], by = 1),
            VO2_t = rep(VO2_Basis, input$ruhe_sim_range[2] - input$ruhe_sim_range[1] + 1)
          )
          
          p <- p %>%
            add_trace(data = ruhe_sim, x = ~t_s, y = ~VO2_t, type = 'scatter', mode = 'markers+lines',
                      name = 'Sim. Ruhewerte', 
                      marker = list(color = '#1CADE4', size = 3.0),
                      line = list(color = '#1CADE4', width = 0.5, dash = '4 4'))
        }
        
        p$x$layout$annotations <- c(p$x$layout$annotations,
                                    list(
                                      list(
                                        x = max_x * 0.10,
                                        y = max_y * 0.65,
                                        text = sprintf("R²: %.3f", r_squared),
                                        showarrow = FALSE,
                                        xanchor = 'left',
                                        yanchor = 'bottom',
                                        font = list(
                                          family = "Arial, sans-serif",
                                          size = 12,
                                          color = "black"
                                        )
                                      )
                                    ))
      }
    }
    
    p
  })
  
  output$instructions <- renderUI({
    HTML(
      "<div style='margin-top: 20px; padding: 10px; background-color: #f0f0f0; border: 1px solid #ddd; border-radius: 5px;'>
    <h4 style='color: #333;'><strong>Anleitung - Modellanpassung:</strong></h4>
    <ol style='color: #555;'>
      <li>Beispiel VO2-Daten oder eigene VO2-Daten als CSV-Datei einfügen.</li>
      <li>Ruhesauerstoffaufnahme (VO2 Ruhe) manuell eingeben oder nach Angabe der nötigen Parameter (Geschlecht, Körpermasse, Alter, RQ) berechnen lassen.</li>
      <li>O2-Speicher manuell festlegen oder auf 0 setzen, falls dieser in der Berechnung nicht beachtet werden soll.</li>
      <li>Zeitverzögerung festlegen, um Startpunkt der Modellanpassung festzulegen.</li>
      <li>Komplette Modellanpassung in 3 Schritten über 'nlsLM - Fit' oder schrittweise durchführen:</li>
      <ul>
        <li>'1. Fit: Tau' Bestimmung der Zeitkonstante tau mit einfacher Exponentialfunktion.</li>
        <li>'2. Fit: EPOC Slow' Modellanpassung der langsamen EPOC-Komponente mit simulierten Ruhewerten.
          <ul>
            <li>'Ruhe_sim anzeigen' wechselt zwischen der normalen und der erweiterten Ansicht mit der Ruhesauerstoffaufnahme.</li>
          </ul>
        </li>
        <li>'3. Fit: EPOC Fast' Modellanpassung der schnellen EPOC-Komponente.</li>
      </ul>
      <li>Alternativ: Manuelle Modellanpassung der Modellparameter mit den Schiebereglern.</li>
    </ol>
    </div>"
    )
  })
}

shinyApp(ui = ui, server = server)