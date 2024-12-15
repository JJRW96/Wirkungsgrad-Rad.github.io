# Muskulärer Wirkungsgrad

$ANOVA
Effect DFn DFd    SSn    SSd        F      p p<.05   ges
1          (Intercept)   1   8 3.2483 0.0022 12053.65 < .001     * 0.999
2            Bedingung   1   8 0.0011 0.0012     7.64  0.024     * 0.202
3           Intensität   2  16 0.0006 0.0009     5.23  0.018     * 0.114
4 Bedingung:Intensität   2  16 0.0003 0.0002     8.42  0.003     * 0.054



Vergleich t_wert p_wert p_wert_korrigiert cohens_d
t    sitzen leicht vs stehen leicht   4.08  0.004             0.011     1.36
t1 sitzen moderat vs stehen moderat   2.29  0.051             0.153     0.76
t2   sitzen schwer vs stehen schwer   1.19  0.269             0.807     0.40


### Gesamtwirkunggrad: 
  [1] "\nANOVA Ergebnisse:"
> print(formatted_anova)
$ANOVA
Effect DFn DFd    SSn    SSd       F      p p<.05   ges
1          (Intercept)   1   8 2.6586 0.0036 5924.37 < .001     * 0.998
2            Bedingung   1   8 0.0001 0.0013    0.53  0.487       0.015
3           Intensität   2  16 0.0001 0.0006    1.26  0.311       0.017
4 Bedingung:Intensität   2  16 0.0001 0.0002    2.90  0.084       0.013

1] "Vergleiche der Bedingungen bei gleicher Intensität:"
> print(ergebnisse)
Vergleich t_wert p_wert p_wert_korrigiert cohens_d
t    sitzen leicht vs stehen leicht   0.18  0.865                 1     0.06
t1 sitzen moderat vs stehen moderat  -0.81  0.439                 1    -0.27
t2   sitzen schwer vs stehen schwer  -1.55  0.161             0.483    -0.52


### Nettowirkungsgrad

$ANOVA
Effect DFn DFd    SSn    SSd       F      p p<.05   ges
1          (Intercept)   1   8 3.3591 0.0050 5393.85 < .001     * 0.998
2            Bedingung   1   8 0.0001 0.0015    0.48  0.507       0.012
3           Intensität   2  16 0.0001 0.0010    0.59  0.565       0.009
4 Bedingung:Intensität   2  16 0.0002 0.0003    4.52  0.028     * 0.019

Vergleich t_wert p_wert p_wert_korrigiert cohens_d
t    sitzen leicht vs stehen leicht   0.46  0.659                 1     0.15
t1 sitzen moderat vs stehen moderat  -0.81  0.441                 1    -0.27
t2   sitzen schwer vs stehen schwer  -1.60  0.148             0.445    -0.53


### Bruttowirkungsgrad
$ANOVA
Effect DFn DFd    SSn    SSd       F      p p<.05   ges
1          (Intercept)   1   8 2.7157 0.0048 4532.89 < .001     * 0.998
2            Bedingung   1   8 0.0001 0.0010    0.68  0.433       0.013
3           Intensität   2  16 0.0004 0.0007    5.17  0.019     * 0.062
4 Bedingung:Intensität   2  16 0.0001 0.0002    4.62  0.026     * 0.016

Vergleich t_wert p_wert p_wert_korrigiert cohens_d
t    sitzen leicht vs stehen leicht   0.38  0.711                 1     0.13
t1 sitzen moderat vs stehen moderat  -0.92  0.386                 1    -0.31
t2   sitzen schwer vs stehen schwer  -1.70  0.128             0.384    -0.57
> 