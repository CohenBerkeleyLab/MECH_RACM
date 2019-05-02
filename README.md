The RACM2\_Berkeley2.1 Mechanism 
---------------------------------

The gas phase chemistry is based on the original RACM2 (Regional
Atmospheric Chemistry Mechanism) mechanism (Goliff et al., 2013), and
its upgraded versions, which are named RACM2\_Berkeley (Browne et al.,2014) and RACM2\_Berkeley2 (Zare et al., 2018). The original RACM2 is
available in CMAQ v5.0.2 and later versions (Sarwar et al., 2013). In
RACM2\_Berkeley, Browne et al. (2014) modified RACM2 to be consistent
with the recent parameterization for hydroxyl radical (OH) oxidation of
isoprene. They also reclassified the lumped organic nitrates from
monoterpenes and anthropogenic VOC precursors into new species and added
oxidation reactions for the new organic nitrate species. The
RACM2\_Berkeley2 mechanism described in Zare et al. (2018) is an updated
version of RACM2\_Berkeley that reflects more recent advances in the
understanding of atmospheric organic chemistry in both low- and
high-NO~x~ conditions, with a focus on detailed representation of
isoprene nitrates from NO~3~ oxidation and production and fate of the
most important individual organic nitrates. The RACM2\_Berkeley2
mechanism was used within the WRF-Chem model.

Here, further explicit representation of multifunctional isoprene
nitrates that are subject to reactive uptake to the aerosol phase are
implemented to RACM2\_Berkeley2 (hereafter referred to as
RACM2\_Berkeley2.1). The newly introduced species into the mechanism
include C5 hydroxy nitrooxy dihydroperoxide (IHDPN), C5 dihydroxy
nitrooxy hydroperoxide (IDHPN), C5 hydroxy nitrooxyperoxy radical
(IHNO2), C5 hydroxy nitrooxyalkoxy radical (IHNO), C5 hydroxy
hydroperoxide nitrate (IHPN) and INO2IN. INO2IN is a ROOR from INO2+INO2
reaction, where INO2 is produced from NO~3~ addition to isoprene and
subsequent O~2~ addition.

IHDPN and IDHPN are 2nd generation nitrates produced from oxidation of δ
and β isomers of C5 nitrooxy hydroperoxide (INPD and INPB) and C5
hydroxy nitrate (IHND and IHNB), respectively (Schwantes et al.,
2015**)**. OH oxidation of the 1st generation isoprene nitrates (INPD,
INPB and IHND, IHNB) produces RO~2~ radicals (INPHO2s and IDHNO2s),
which react with HO~2~ to form IDHPN and IHDPN. Pye et al. (2015) showed
the dominant peroxy radical (RO~2~) fate is reaction with HO~2~ and to a
small extent reaction with other RO~2~. The RO~2~+NO~3~ reaction is
negligible. Therefore, in this study where we add the RO~2~+HO~2~
reactions to form IDHPN and IHDPN, we omit RO~2~+NO~3~ reactions to save
computational time.

Schwantes et al. (2015) showed that INO2 can react with itself or with
another RO~2~ radical to form C5 hydroxy nitrates, C5 carbonyl nitrates,
INO2IN and alkoxy radicals (INO). INO can either react with O~2~ or
rapidly undergo a \[1,5\]-H-shift to form a C5 dihydroxy nitrate, a C5
hydroxy carbonyl nitrate or a C5 hydroxy hydroperoxide nitrate (Kwan et
al., 2012; Schwantes et al., 2015). We implement these reactions into
the scheme and use the lumped IHPN indicator as a surrogate for these 3
products. In total the updated reaction set includes 28 lumped organic
nitrate surrogate species, in contrast to only three organic nitrate
species (two isoprene-derived nitrates and one lumped terpene nitrate)
were included in the original RACM2 mechanism. All new introduced
species and modifications implemented into the RACM2\_Berkeley2.1
mechanism are listed in Table 1 and 2.

**Table 1.** Species added to the RACM2\_Berkeley2.1.

  |Abbreviation|Description
  |--------------| ---------------------------------------------------------
  |IHDPN|        C5 hydroxy nitrooxy dihydroperoxide
  |IDHPN|          C5 dihydroxy nitrooxy hydroperoxide
  |INO2IN|         Product from INO2 and INO2
  |IHNO2|          C5 hydroxy nitrooxyperoxy radical (1,5 H shift product)
  |IHNO|           C5 hydroxy nitrooxyalkoxy radical
  |IHPN|           C5 hydroxy hydroperoxide nitrate

**Table 2.** Reactions that are added, removed or modified in the
RACM2\_ Berkeley2.1.

 |Reactants|       Products   |                                                                                           Rate (s^-1^)   |                 Status
 |---------------| -----------------------------------------------------------------------------------------------------| ---------------|----------
 | INO2+INO2     |  0.39 INO+0.67ICN+0.10 MACR+0.616 IHND+0.154 IHNB+0.035 INO2IN                                       | 5.2D-12       |                  Modified
 | ICN+NO3      |   0.1INHED + 0.1NO2     + 0.9 NC4CO3 +0.9 HNO3                                                        | 6.3D-12exp(-1860/T)      |       Modified                                                                                                                           
 | NH4CO3+NO   |    PROPNN+CO+HO2+NO2                                                                                    | 7.5D-12exp(-690/T)       |       Removed
 | NH4CO3+NH4CO3 |  0.3 R4N+0.7 PROPNN+0.7 HO2+0.7 CO                                                                    | 1.0D-11                     |    Removed
 | NC4CO3+NO3    |  PROPNN+CO+HO2+NO2                                                                                    | 4.0D-12                     |    Added
 | IDHNO2D+NO3   |  HO2+NO2+0.12 HAC+0.12 ETHLN+0.8 GLYC+0.80 PROPNN+0.08 R4N+0.08 HCHO                                  | 2.3D-12                     |    Removed
 | IDHNO2B+NO3   |  HO2+NO2+0.76 HAC+0.76 ETHLN+0.23 R4N+0.23 HCHO                                                       | 2.3D-12                     |    Removed
 | IDHNO2D+HO2   |  0.27IDHPN +0.73 OH + 0.73 HO2 + 0.09 HAC+ 0.09 ETHLN + 0.58 PROPNN+ 0.58GLYC + 0.06 R4N + 0.06 HCHO  | 2.04D-13 \*exp(1300/Temp)   |    Added
 | IDHNO2B + HO2 |  0.27IDHPN + 0.73 OH+ 0.73 HO2 + 0.56 HAC + 0.17 HCHO + 0.56 ETHLN + 0.17 R4N                         | 2.04D-13 \* exp(1300/Temp)  |    Added
 | INPD+HO       |  0.37 INHED+0.08IEPOX + 0.08NO2+0.37 HO +0.55 INPHO2D                                                 | 1.1D-10                    |     Modified
 | INPHO2B+NO3   |  NO2+HO2+HCHO+R4NO                                                                                    | 2.3D-12                    |     Removed
 | INPHO2D+NO3   |  NO2+HO2+0.92 PROPNN+0.92 GLY+0.08 HAC+0.08 ETHLN                                                     | 2.3D-12                     |    Removed
 | INPHO2B+ HO2  |  0.27IHDPN                  + 0.73 OH + 0.73 HO2 + 0.73 hcho+ 0.73 R4NO                                                                             | 2.04D-13 \* exp(1300/Temp)   |   Added                                                                                                                                                    
 | INPHO2D+ HO2  |  0.27IHDPN + 0.06 ETHLN + 0.73 OH + 0.73 HO2+ 0.67 PROPNN + 0.67 GLYC       + 0.06 HAC                                 | 2.04D-13 \* exp(1300/Temp)  |    Added
 | INHED+HO     |   0.27 HAC+0.73 CO+0.27 NO2+0.27 HCHO+0.17 PROPNN+0.17 GLY+0.46 R4N+0.1 INHED                          | 8.4D-12        |                 Modified
 | INHEB+HO   |     0.08 INHEB     0.22PROPNN+0.22 GLY    0.31 GLYC+0.31 MGLY     0.09 HAC+0.43 NO2+0.39 HCHO+0.01 ETHLN+0.01 HAC+0.12 KET+0.26 R4N                                                                               |   1.25D-11         |               Modified                                                                                                                                                     
 | INO       |      IHNO2                                                                                               |  2.0D5                      |     Added
 | IHNO2 + NO3  |   IHNO + NO2                                                                                          |  2.3D-12                     |    Added
 | IHNO2 + HO2  |   IHPN                                                                                                |  2.91D-13\*exp(1300/T )\*0.706 |  Added
 | IHNO + O2    |   IHPN + HO2                                                                                          |  2.5D-12\*exp(-300/TEMP)      |   Added
 | IHNO2 + IHNO2 |  0.46IHNO + 1.54IHPN                                                                              |   2\*5.0 D-12          |           Added
 | IHPN + hv     |  IHNO + HO                                                                                           |  j(Pj\_ch3o2h)          |         Added
  
Browne, E. C., Wooldridge, P. J., Min, K.-E. and Cohen, R. C.: On the
role of monoterpene chemistry in the remote continental boundary layer,
Atmos. Chem. Phys., 14(3), 1225–1238, doi:10.5194/acp-14-1225-2014, 2014.

Eddingsaas, N. C., VanderVelde, D. G. and Wennberg, P. O.: Kinetics and
products of the acid-catalyzed ring-opening of atmospherically relevant
butyl epoxy alcohols, J Phys Chem A, 114(31), 8106–8113,
doi:10.1021/jp103907c, 2010.

Goliff, W. S., Stockwell, W. R. and Lawson, C. V.: The regional
atmospheric chemistry mechanism, version 2, Atmospheric Environment, 68,
174–185, doi:10.1016/j.atmosenv.2012.11.038, 2013.

Jacobs, M. I., Burke, W. J. and Elrod, M. J.: Kinetics of the reactions
of isoprene-derived hydroxynitrates: gas phase epoxide formation and
solution phase hydrolysis, Atmos. Chem. Phys., 14(17), 8933–8946,
doi:10.5194/acp-14-8933-2014, 2014.

Kwan, A. J., Chan, A. W. H., Ng, N. L., Kjaergaard, H. G., Seinfeld, J.
H. and Wennberg, P. O.: Peroxy radical chemistry and OH radical
production during the NO3-initiated oxidation of isoprene, Atmos. Chem.
Phys., 12(16), 7499–7515, doi:10.5194/acp-12-7499-2012, 2012.

Paulot, F., Crounse, J. D., Kjaergaard, H. G., Kroll, J. H., Seinfeld,
J. H. and Wennberg, P. O.: Isoprene photooxidation: new insights into
the production of acids and organic nitrates, Atmos. Chem. Phys., 9(4),
1479–1501, doi:10.5194/acp-9-1479-2009, 2009.

Pye, H. O. T., Luecken, D. J., Xu, L., Boyd, C. M., Ng, N. L., Baker, K.
R., Ayres, B. R., Bash, J. O., Baumann, K., Carter, W. P. L., Edgerton,
E., Fry, J. L., Hutzell, W. T., Schwede, D. B. and Shepson, P. B.:
Modeling the Current and Future Roles of Particulate Organic Nitrates in
the Southeastern United States, Environ. Sci. Technol., 49(24),
14195–14203, doi:10.1021/acs.est.5b03738, 2015.

Sarwar, G., Godowitch, J., Henderson, B. H., Fahey, K., Pouliot, G.,
Hutzell, W. T., Mathur, R., Kang, D., Goliff, W. S. and Stockwell, W.
R.: A comparison of atmospheric composition using the Carbon Bond and
Regional Atmospheric Chemistry Mechanisms, Atmospheric Chemistry and
Physics, 13(19), 9695–9712, doi:10.5194/acp-13-9695-2013, 2013.

Schwantes, R. H., Teng, A. P., Nguyen, T. B., Coggon, M. M., Crounse, J.
D., St. Clair, J. M., Zhang, X., Schilling, K. A., Seinfeld, J. H. and
Wennberg, P. O.: Isoprene NO3 Oxidation Products from the RO2 + HO2
Pathway, J. Phys. Chem. A, doi:10.1021/acs.jpca.5b06355, 2015.

Wennberg, P. O., Bates, K. H., Crounse, J. D., Dodson, L. G., McVay, R.
C., Mertens, L. A., Nguyen, T. B., Praske, E., Schwantes, R. H., Smarte,
M. D., St Clair, J. M., Teng, A. P., Zhang, X. and Seinfeld, J. H.:
Gas-Phase Reactions of Isoprene and Its Major Oxidation Products, Chem.
Rev., 118(7), 3337–3390, doi:10.1021/acs.chemrev.7b00439, 2018.

Zare, A., Romer, P. S., Nguyen, T., Keutsch, F. N., Skog, K. and Cohen,
R. C.: A comprehensive organic nitrate chemistry: insights into the
lifetime of atmospheric organic nitrates, Atmospheric Chemistry and
Physics, 18(20), 15419–15436,
doi:https://doi.org/10.5194/acp-18-15419-2018, 2018.
