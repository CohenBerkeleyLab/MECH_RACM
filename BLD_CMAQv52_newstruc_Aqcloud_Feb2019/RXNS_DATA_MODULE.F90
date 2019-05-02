       MODULE RXNS_DATA


       IMPLICIT NONE



! --------- Photochemical Mechanism Reactions, Rates, etc. DAT ---------
! Source file: /global/scratch/azare/EPA_Project/CMAQ_v5.2/CCTM/src/MECHS/racm2_ae6_aq/mech_racm2_ae6_aq.def
! for Mechanism Name: RACM2_AE6_AQ                    

! This file is used to create mechanism data and functions

! The following are reserved symbols declared in this file:
!    MECHNAME       = Mechanism name
!    N_GAS_CHEM_SPC = Total number of gas species in chemical mechanism
!    NUMB_CHEM_SPC  = Total number of species in chemical mechanism
!    N_ACT_SP       = Number of active (determined by ODE solver) species in mechanism
!    GAS_CHEM_SPC   = Names of gas species in chemical mechanism
!    CHEMISTRY_SPC  = Names of species in chemical mechanism
!    CGRID_INDEX    = CGRID Index of species in chemical mechanism
!    SPECIES_TYPE   = Group or type of species 
!    SPECIES_MOLWT  = Molecular Weight of species (gm/mole)
!    NRXNS          = Number of mechanism reactions
!    KUNITS         = Units of mechanism reactions
!    KTYPE          = Reaction type
!    IRXBITS        = Bit test mask vector for selected reactions
!    IORDER         = Order of the reaction
!    KTN1           = Number of type 1 reactions
!    KRX1           = Reactions list pointer to type 1 reactions
!    KTN2           = Number of type 2 reactions
!    KRX2           = Reactions list pointer to type 2 reactions
!    KTN3           = Number of type 3 reactions
!    KRX3           = Reactions list pointer to type 3 reactions
!    KTN4           = Number of type 4 reactions
!    KRX4           = Reactions list pointer to type 4 reactions
!    KTN5           = Number of type 5 reactions
!    KRX5           = Reactions list pointer to type 5 reactions
!    KTN6           = Number of type 6 reactions
!    KRX6           = Reactions list pointer to type 6 reactions
!    KTN7           = Number of type 7 reactions
!    KRX7           = Reactions list pointer to type 7 reactions

! The following are reserved symbols declared in this file:
!    MECHNAME       = Mechanism name
!    N_GAS_CHEM_SPC = Total number of gas species in chemical mechanism
!    NUMB_CHEM_SPC  = Total number of species in chemical mechanism
!    N_ACT_SP       = Number of active (determined by ODE solver) species in mechanism
!    GAS_CHEM_SPC   = Names of gas species in chemical mechanism
!    CHEMISTRY_SPC  = Names of species in chemical mechanism
!    CGRID_INDEX    = CGRID Index of species in chemical mechanism
!    SPECIES_TYPE   = Group or type of species 
!    SPECIES_MOLWT  = Molecular Weight of species (gm/mole)
!    NRXNS          = Number of mechanism reactions
!    KUNITS         = Units of mechanism reactions
!    KTYPE          = Reaction type
!    IRXBITS        = Bit test mask vector for selected reactions
!    IORDER         = Order of the reaction
!    KTN1           = Number of type 1 reactions
!    KRX1           = Reactions list pointer to type 1 reactions
!    KTN2           = Number of type 2 reactions
!    KRX2           = Reactions list pointer to type 2 reactions
!    KTN3           = Number of type 3 reactions
!    KRX3           = Reactions list pointer to type 3 reactions
!    KTN4           = Number of type 4 reactions
!    KRX4           = Reactions list pointer to type 4 reactions
!    KTN5           = Number of type 5 reactions
!    KRX5           = Reactions list pointer to type 5 reactions
!    KTN6           = Number of type 6 reactions
!    KRX6           = Reactions list pointer to type 6 reactions
!    KTN7           = Number of type 7 reactions
!    KRX7           = Reactions list pointer to type 7 reactions

!    NWM       = Number of air 3-body reactions
!    NRXWM     = Reactions list pointer to air 3-body reactions
!    ATM_AIR   = air 3-body reactions concentration
!    NWW       = Number of H2O 3-body reactions
!    NRXWW     = Reactions list pointer to H2O 3-body reactions
!    NWO2      = Number of reactions with O2
!    NRXWO2    = Reactions list pointer to O2 reactions
!    ATM_O2    = Oxygen reactions concentration
!    NWN2      = Number of N2 3-body reactions
!    NRXWN2    = Reactions list pointer to N2 3-body reactions
!    ATM_N2    = Nitrogen 3-body reactions concentration
!    NWCH4     = Number of reactions with CH4
!    NRXWCH4   = Reactions list pointer to CH4 reactions
!    ATM_CH4   = Methane reactions concentration
!    NWH2      = Number of reactions with H2
!    NRXWH2    = Reactions list pointer to H2 reactions
!    ATM_H2    = Hydrogen reactions concentration

!    MXPRD     = Maximum number of mechanism reaction products
!    IRR       = Reactions list pointer to reactants and products
!    RTDAT     = Kinetic reaction rates expressions components
!    NFALLOFFF = Number of falloff reactions
!    IRRFALL   = Reactions list pointer to falloff reactions
!    RFDAT     = Falloff reaction rates expressions components
!    SC        = Stoichiometric coefficients
!    NREACT    = Number of reactants in each mechanism reaction
!    NPRDCT    = Number of products in each mechanism reaction
!    RXLABEL   = Character label list for mechanism reactions
!    NMPHOT    = Number of mechanism photolytic reactions
!    NPHOTAB   = Number of photolytic reactions tables
!    IPH       = Reactions list pointer to photolytic reactions and tables
!    MHETERO   = Number of mechanism heteorogenous reactions
!    NHETERO   = Number of unique heteorogenous rate constants
!    IHETERO   = Reactions list pointer to heteorogenous reactions and tables

      CHARACTER( 32 ), PARAMETER :: MECHNAME = 'RACM2_AE6_AQ'

      INTEGER, PARAMETER :: N_GAS_CHEM_SPC = 206
      INTEGER, PARAMETER :: NUMB_MECH_SPC  = 273

      CHARACTER( 16 ) :: GAS_CHEM_SPC( N_GAS_CHEM_SPC )
      CHARACTER( 16 ) :: CHEMISTRY_SPC( NUMB_MECH_SPC )
      CHARACTER( 16 ) :: SPECIES_TYPE(  NUMB_MECH_SPC )
      INTEGER         :: CGRID_INDEX (  NUMB_MECH_SPC )
      INTEGER         :: TYPE_INDEX  (  NUMB_MECH_SPC )
      LOGICAL         :: CONVERT_CONC(  NUMB_MECH_SPC )
      REAL            :: SPECIES_MOLWT( NUMB_MECH_SPC )

! The below character and integer arrays list the model species names used in the 
! chemical mechanism. The gas species and their order should agree with 
! the GC_SPC array for the gas phase chemistry to work correctly. 
! If present, the CHEMISTRY_SPC names and species type should agree with the CGRID_SPCS module

      DATA GAS_CHEM_SPC(   1 ) / 'O3              ' /
      DATA GAS_CHEM_SPC(   2 ) / 'O3P             ' /
      DATA GAS_CHEM_SPC(   3 ) / 'O1D             ' /
      DATA GAS_CHEM_SPC(   4 ) / 'H2O2            ' /
      DATA GAS_CHEM_SPC(   5 ) / 'HO              ' /
      DATA GAS_CHEM_SPC(   6 ) / 'NO2             ' /
      DATA GAS_CHEM_SPC(   7 ) / 'NO              ' /
      DATA GAS_CHEM_SPC(   8 ) / 'NO3             ' /
      DATA GAS_CHEM_SPC(   9 ) / 'HONO            ' /
      DATA GAS_CHEM_SPC(  10 ) / 'HNO3            ' /
      DATA GAS_CHEM_SPC(  11 ) / 'HNO4            ' /
      DATA GAS_CHEM_SPC(  12 ) / 'HO2             ' /
      DATA GAS_CHEM_SPC(  13 ) / 'HCHO            ' /
      DATA GAS_CHEM_SPC(  14 ) / 'CO              ' /
      DATA GAS_CHEM_SPC(  15 ) / 'ACD             ' /
      DATA GAS_CHEM_SPC(  16 ) / 'MO2             ' /
      DATA GAS_CHEM_SPC(  17 ) / 'ALD             ' /
      DATA GAS_CHEM_SPC(  18 ) / 'ETHP            ' /
      DATA GAS_CHEM_SPC(  19 ) / 'ACT             ' /
      DATA GAS_CHEM_SPC(  20 ) / 'ACO3            ' /
      DATA GAS_CHEM_SPC(  21 ) / 'UALD            ' /
      DATA GAS_CHEM_SPC(  22 ) / 'KET             ' /
      DATA GAS_CHEM_SPC(  23 ) / 'MEK             ' /
      DATA GAS_CHEM_SPC(  24 ) / 'HKET            ' /
      DATA GAS_CHEM_SPC(  25 ) / 'MACR            ' /
      DATA GAS_CHEM_SPC(  26 ) / 'MACP            ' /
      DATA GAS_CHEM_SPC(  27 ) / 'MVK             ' /
      DATA GAS_CHEM_SPC(  28 ) / 'OLT             ' /
      DATA GAS_CHEM_SPC(  29 ) / 'GLY             ' /
      DATA GAS_CHEM_SPC(  30 ) / 'MGLY            ' /
      DATA GAS_CHEM_SPC(  31 ) / 'DCB1            ' /
      DATA GAS_CHEM_SPC(  32 ) / 'XO2             ' /
      DATA GAS_CHEM_SPC(  33 ) / 'DCB2            ' /
      DATA GAS_CHEM_SPC(  34 ) / 'BALD            ' /
      DATA GAS_CHEM_SPC(  35 ) / 'CHO             ' /
      DATA GAS_CHEM_SPC(  36 ) / 'OP1             ' /
      DATA GAS_CHEM_SPC(  37 ) / 'OP2             ' /
      DATA GAS_CHEM_SPC(  38 ) / 'PAA             ' /
      DATA GAS_CHEM_SPC(  39 ) / 'ONIT            ' /
      DATA GAS_CHEM_SPC(  40 ) / 'PAN             ' /
      DATA GAS_CHEM_SPC(  41 ) / 'N2O5            ' /
      DATA GAS_CHEM_SPC(  42 ) / 'SO2             ' /
      DATA GAS_CHEM_SPC(  43 ) / 'SULF            ' /
      DATA GAS_CHEM_SPC(  44 ) / 'SULRXN          ' /
      DATA GAS_CHEM_SPC(  45 ) / 'ETH             ' /
      DATA GAS_CHEM_SPC(  46 ) / 'HC3             ' /
      DATA GAS_CHEM_SPC(  47 ) / 'HC3P            ' /
      DATA GAS_CHEM_SPC(  48 ) / 'HC5             ' /
      DATA GAS_CHEM_SPC(  49 ) / 'HC5P            ' /
      DATA GAS_CHEM_SPC(  50 ) / 'HC8             ' /
      DATA GAS_CHEM_SPC(  51 ) / 'HC8P            ' /
      DATA GAS_CHEM_SPC(  52 ) / 'ETE             ' /
      DATA GAS_CHEM_SPC(  53 ) / 'ETEP            ' /
      DATA GAS_CHEM_SPC(  54 ) / 'OLTP            ' /
      DATA GAS_CHEM_SPC(  55 ) / 'OLI             ' /
      DATA GAS_CHEM_SPC(  56 ) / 'OLIP            ' /
      DATA GAS_CHEM_SPC(  57 ) / 'DIEN            ' /
      DATA GAS_CHEM_SPC(  58 ) / 'ACE             ' /
      DATA GAS_CHEM_SPC(  59 ) / 'ORA1            ' /
      DATA GAS_CHEM_SPC(  60 ) / 'BENZENE         ' /
      DATA GAS_CHEM_SPC(  61 ) / 'BENP            ' /
      DATA GAS_CHEM_SPC(  62 ) / 'EPX             ' /
      DATA GAS_CHEM_SPC(  63 ) / 'PHEN            ' /
      DATA GAS_CHEM_SPC(  64 ) / 'BENZRO2         ' /
      DATA GAS_CHEM_SPC(  65 ) / 'TOL             ' /
      DATA GAS_CHEM_SPC(  66 ) / 'TR2             ' /
      DATA GAS_CHEM_SPC(  67 ) / 'TLP1            ' /
      DATA GAS_CHEM_SPC(  68 ) / 'CSL             ' /
      DATA GAS_CHEM_SPC(  69 ) / 'TOLRO2          ' /
      DATA GAS_CHEM_SPC(  70 ) / 'XYM             ' /
      DATA GAS_CHEM_SPC(  71 ) / 'XY2             ' /
      DATA GAS_CHEM_SPC(  72 ) / 'XYL1            ' /
      DATA GAS_CHEM_SPC(  73 ) / 'XYLRO2          ' /
      DATA GAS_CHEM_SPC(  74 ) / 'XYP             ' /
      DATA GAS_CHEM_SPC(  75 ) / 'XYO             ' /
      DATA GAS_CHEM_SPC(  76 ) / 'XYO2            ' /
      DATA GAS_CHEM_SPC(  77 ) / 'ISO             ' /
      DATA GAS_CHEM_SPC(  78 ) / 'ISOP            ' /
      DATA GAS_CHEM_SPC(  79 ) / 'API             ' /
      DATA GAS_CHEM_SPC(  80 ) / 'APIP            ' /
      DATA GAS_CHEM_SPC(  81 ) / 'LIM             ' /
      DATA GAS_CHEM_SPC(  82 ) / 'LIMP            ' /
      DATA GAS_CHEM_SPC(  83 ) / 'RCO3            ' /
      DATA GAS_CHEM_SPC(  84 ) / 'ACTP            ' /
      DATA GAS_CHEM_SPC(  85 ) / 'MEKP            ' /
      DATA GAS_CHEM_SPC(  86 ) / 'KETP            ' /
      DATA GAS_CHEM_SPC(  87 ) / 'MACRO2          ' /
      DATA GAS_CHEM_SPC(  88 ) / 'MVKP            ' /
      DATA GAS_CHEM_SPC(  89 ) / 'UALP            ' /
      DATA GAS_CHEM_SPC(  90 ) / 'DCB3            ' /
      DATA GAS_CHEM_SPC(  91 ) / 'BALP            ' /
      DATA GAS_CHEM_SPC(  92 ) / 'ADDC            ' /
      DATA GAS_CHEM_SPC(  93 ) / 'MCT             ' /
      DATA GAS_CHEM_SPC(  94 ) / 'MCTO            ' /
      DATA GAS_CHEM_SPC(  95 ) / 'MOH             ' /
      DATA GAS_CHEM_SPC(  96 ) / 'EOH             ' /
      DATA GAS_CHEM_SPC(  97 ) / 'ROH             ' /
      DATA GAS_CHEM_SPC(  98 ) / 'ETEG            ' /
      DATA GAS_CHEM_SPC(  99 ) / 'ISHPA           ' /
      DATA GAS_CHEM_SPC( 100 ) / 'IEPOXA          ' /
      DATA GAS_CHEM_SPC( 101 ) / 'IEPOXB          ' /
      DATA GAS_CHEM_SPC( 102 ) / 'MAHP            ' /
      DATA GAS_CHEM_SPC( 103 ) / 'ORA2            ' /
      DATA GAS_CHEM_SPC( 104 ) / 'ORAP            ' /
      DATA GAS_CHEM_SPC( 105 ) / 'PPN             ' /
      DATA GAS_CHEM_SPC( 106 ) / 'MPAN            ' /
      DATA GAS_CHEM_SPC( 107 ) / 'IHPN            ' /
      DATA GAS_CHEM_SPC( 108 ) / 'IHNO            ' /
      DATA GAS_CHEM_SPC( 109 ) / 'INO2            ' /
      DATA GAS_CHEM_SPC( 110 ) / 'ICN             ' /
      DATA GAS_CHEM_SPC( 111 ) / 'TRPRXN          ' /
      DATA GAS_CHEM_SPC( 112 ) / 'PYAC            ' /
      DATA GAS_CHEM_SPC( 113 ) / 'MCTP            ' /
      DATA GAS_CHEM_SPC( 114 ) / 'OLNN            ' /
      DATA GAS_CHEM_SPC( 115 ) / 'OLND            ' /
      DATA GAS_CHEM_SPC( 116 ) / 'TOLNN           ' /
      DATA GAS_CHEM_SPC( 117 ) / 'TOLND           ' /
      DATA GAS_CHEM_SPC( 118 ) / 'ADCN            ' /
      DATA GAS_CHEM_SPC( 119 ) / 'TOLP            ' /
      DATA GAS_CHEM_SPC( 120 ) / 'PER1            ' /
      DATA GAS_CHEM_SPC( 121 ) / 'XYLP            ' /
      DATA GAS_CHEM_SPC( 122 ) / 'PER2            ' /
      DATA GAS_CHEM_SPC( 123 ) / 'XYOP            ' /
      DATA GAS_CHEM_SPC( 124 ) / 'MONIT           ' /
      DATA GAS_CHEM_SPC( 125 ) / 'DONIT           ' /
      DATA GAS_CHEM_SPC( 126 ) / 'AONIT           ' /
      DATA GAS_CHEM_SPC( 127 ) / 'ISOPND          ' /
      DATA GAS_CHEM_SPC( 128 ) / 'ISOPNB          ' /
      DATA GAS_CHEM_SPC( 129 ) / 'UHC             ' /
      DATA GAS_CHEM_SPC( 130 ) / 'DIBOO           ' /
      DATA GAS_CHEM_SPC( 131 ) / 'TONIT           ' /
      DATA GAS_CHEM_SPC( 132 ) / 'UTONIT          ' /
      DATA GAS_CHEM_SPC( 133 ) / 'HAC             ' /
      DATA GAS_CHEM_SPC( 134 ) / 'MACRN           ' /
      DATA GAS_CHEM_SPC( 135 ) / 'GLYC            ' /
      DATA GAS_CHEM_SPC( 136 ) / 'MVKN            ' /
      DATA GAS_CHEM_SPC( 137 ) / 'BAL1            ' /
      DATA GAS_CHEM_SPC( 138 ) / 'BAL2            ' /
      DATA GAS_CHEM_SPC( 139 ) / 'ISHPB           ' /
      DATA GAS_CHEM_SPC( 140 ) / 'ISHPD           ' /
      DATA GAS_CHEM_SPC( 141 ) / 'ISOPRXN         ' /
      DATA GAS_CHEM_SPC( 142 ) / 'VRP             ' /
      DATA GAS_CHEM_SPC( 143 ) / 'IEPOXD          ' /
      DATA GAS_CHEM_SPC( 144 ) / 'TONIN           ' /
      DATA GAS_CHEM_SPC( 145 ) / 'TONIH           ' /
      DATA GAS_CHEM_SPC( 146 ) / 'UTONIN          ' /
      DATA GAS_CHEM_SPC( 147 ) / 'HONIT           ' /
      DATA GAS_CHEM_SPC( 148 ) / 'IAP             ' /
      DATA GAS_CHEM_SPC( 149 ) / 'DHMOB           ' /
      DATA GAS_CHEM_SPC( 150 ) / 'UHCAP           ' /
      DATA GAS_CHEM_SPC( 151 ) / 'UHCP            ' /
      DATA GAS_CHEM_SPC( 152 ) / 'MOBA            ' /
      DATA GAS_CHEM_SPC( 153 ) / 'MOBAOO          ' /
      DATA GAS_CHEM_SPC( 154 ) / 'HPALD           ' /
      DATA GAS_CHEM_SPC( 155 ) / 'ISOPNBO2        ' /
      DATA GAS_CHEM_SPC( 156 ) / 'ISOPNDO2        ' /
      DATA GAS_CHEM_SPC( 157 ) / 'DHDN            ' /
      DATA GAS_CHEM_SPC( 158 ) / 'PROPNN          ' /
      DATA GAS_CHEM_SPC( 159 ) / 'ETHLN           ' /
      DATA GAS_CHEM_SPC( 160 ) / 'IMONIT          ' /
      DATA GAS_CHEM_SPC( 161 ) / 'ISNP            ' /
      DATA GAS_CHEM_SPC( 162 ) / 'IEPOXOO         ' /
      DATA GAS_CHEM_SPC( 163 ) / 'INO             ' /
      DATA GAS_CHEM_SPC( 164 ) / 'IHND            ' /
      DATA GAS_CHEM_SPC( 165 ) / 'IHNB            ' /
      DATA GAS_CHEM_SPC( 166 ) / 'INO2IN          ' /
      DATA GAS_CHEM_SPC( 167 ) / 'INHED           ' /
      DATA GAS_CHEM_SPC( 168 ) / 'NC4CO3          ' /
      DATA GAS_CHEM_SPC( 169 ) / 'R4NO            ' /
      DATA GAS_CHEM_SPC( 170 ) / 'R4N             ' /
      DATA GAS_CHEM_SPC( 171 ) / 'IDHNO2B         ' /
      DATA GAS_CHEM_SPC( 172 ) / 'IDHPN           ' /
      DATA GAS_CHEM_SPC( 173 ) / 'IDHNO2D         ' /
      DATA GAS_CHEM_SPC( 174 ) / 'INPD            ' /
      DATA GAS_CHEM_SPC( 175 ) / 'INPB            ' /
      DATA GAS_CHEM_SPC( 176 ) / 'INPHO2D         ' /
      DATA GAS_CHEM_SPC( 177 ) / 'INHEB           ' /
      DATA GAS_CHEM_SPC( 178 ) / 'INPHO2B         ' /
      DATA GAS_CHEM_SPC( 179 ) / 'IHDPN           ' /
      DATA GAS_CHEM_SPC( 180 ) / 'IHNO2           ' /
      DATA GAS_CHEM_SPC( 181 ) / 'TOLNRXN         ' /
      DATA GAS_CHEM_SPC( 182 ) / 'TOLHRXN         ' /
      DATA GAS_CHEM_SPC( 183 ) / 'XYLNRXN         ' /
      DATA GAS_CHEM_SPC( 184 ) / 'XYLHRXN         ' /
      DATA GAS_CHEM_SPC( 185 ) / 'BNZNRXN         ' /
      DATA GAS_CHEM_SPC( 186 ) / 'BNZHRXN         ' /
      DATA GAS_CHEM_SPC( 187 ) / 'SESQ            ' /
      DATA GAS_CHEM_SPC( 188 ) / 'SESQRXN         ' /
      DATA GAS_CHEM_SPC( 189 ) / 'NAPH            ' /
      DATA GAS_CHEM_SPC( 190 ) / 'PAHRO2          ' /
      DATA GAS_CHEM_SPC( 191 ) / 'PAHNRXN         ' /
      DATA GAS_CHEM_SPC( 192 ) / 'PAHHRXN         ' /
      DATA GAS_CHEM_SPC( 193 ) / 'SOAALK          ' /
      DATA GAS_CHEM_SPC( 194 ) / 'ALKRXN          ' /
      DATA GAS_CHEM_SPC( 195 ) / 'PCVOC           ' /
      DATA GAS_CHEM_SPC( 196 ) / 'PCSOARXN        ' /
      DATA GAS_CHEM_SPC( 197 ) / 'VLVPO1          ' /
      DATA GAS_CHEM_SPC( 198 ) / 'VSVPO1          ' /
      DATA GAS_CHEM_SPC( 199 ) / 'VSVPO2          ' /
      DATA GAS_CHEM_SPC( 200 ) / 'VSVPO3          ' /
      DATA GAS_CHEM_SPC( 201 ) / 'VIVPO1          ' /
      DATA GAS_CHEM_SPC( 202 ) / 'VLVOO1          ' /
      DATA GAS_CHEM_SPC( 203 ) / 'VLVOO2          ' /
      DATA GAS_CHEM_SPC( 204 ) / 'VSVOO2          ' /
      DATA GAS_CHEM_SPC( 205 ) / 'VSVOO3          ' /
      DATA GAS_CHEM_SPC( 206 ) / 'VSVOO1          ' /




      LOGICAL   :: HALOGEN_PARAMETER = .TRUE. 

      DATA CHEMISTRY_SPC(   1 ), SPECIES_MOLWT(   1 ) / 'O3              ',   48.00 /
      DATA CHEMISTRY_SPC(   2 ), SPECIES_MOLWT(   2 ) / 'O3P             ',   16.00 /
      DATA CHEMISTRY_SPC(   3 ), SPECIES_MOLWT(   3 ) / 'O1D             ',   16.00 /
      DATA CHEMISTRY_SPC(   4 ), SPECIES_MOLWT(   4 ) / 'H2O2            ',   34.00 /
      DATA CHEMISTRY_SPC(   5 ), SPECIES_MOLWT(   5 ) / 'HO              ',   17.00 /
      DATA CHEMISTRY_SPC(   6 ), SPECIES_MOLWT(   6 ) / 'NO2             ',   46.00 /
      DATA CHEMISTRY_SPC(   7 ), SPECIES_MOLWT(   7 ) / 'NO              ',   30.00 /
      DATA CHEMISTRY_SPC(   8 ), SPECIES_MOLWT(   8 ) / 'NO3             ',   62.00 /
      DATA CHEMISTRY_SPC(   9 ), SPECIES_MOLWT(   9 ) / 'HONO            ',   47.00 /
      DATA CHEMISTRY_SPC(  10 ), SPECIES_MOLWT(  10 ) / 'HNO3            ',   63.00 /
      DATA CHEMISTRY_SPC(  11 ), SPECIES_MOLWT(  11 ) / 'HNO4            ',   79.00 /
      DATA CHEMISTRY_SPC(  12 ), SPECIES_MOLWT(  12 ) / 'HO2             ',   33.00 /
      DATA CHEMISTRY_SPC(  13 ), SPECIES_MOLWT(  13 ) / 'HCHO            ',   30.00 /
      DATA CHEMISTRY_SPC(  14 ), SPECIES_MOLWT(  14 ) / 'CO              ',   28.00 /
      DATA CHEMISTRY_SPC(  15 ), SPECIES_MOLWT(  15 ) / 'ACD             ',   44.00 /
      DATA CHEMISTRY_SPC(  16 ), SPECIES_MOLWT(  16 ) / 'MO2             ',   47.00 /
      DATA CHEMISTRY_SPC(  17 ), SPECIES_MOLWT(  17 ) / 'ALD             ',   58.00 /
      DATA CHEMISTRY_SPC(  18 ), SPECIES_MOLWT(  18 ) / 'ETHP            ',   61.00 /
      DATA CHEMISTRY_SPC(  19 ), SPECIES_MOLWT(  19 ) / 'ACT             ',   58.00 /
      DATA CHEMISTRY_SPC(  20 ), SPECIES_MOLWT(  20 ) / 'ACO3            ',   75.00 /
      DATA CHEMISTRY_SPC(  21 ), SPECIES_MOLWT(  21 ) / 'UALD            ',   84.00 /
      DATA CHEMISTRY_SPC(  22 ), SPECIES_MOLWT(  22 ) / 'KET             ',   86.00 /
      DATA CHEMISTRY_SPC(  23 ), SPECIES_MOLWT(  23 ) / 'MEK             ',   72.00 /
      DATA CHEMISTRY_SPC(  24 ), SPECIES_MOLWT(  24 ) / 'HKET            ',   74.00 /
      DATA CHEMISTRY_SPC(  25 ), SPECIES_MOLWT(  25 ) / 'MACR            ',   70.00 /
      DATA CHEMISTRY_SPC(  26 ), SPECIES_MOLWT(  26 ) / 'MACP            ',  101.00 /
      DATA CHEMISTRY_SPC(  27 ), SPECIES_MOLWT(  27 ) / 'MVK             ',   70.00 /
      DATA CHEMISTRY_SPC(  28 ), SPECIES_MOLWT(  28 ) / 'OLT             ',   42.00 /
      DATA CHEMISTRY_SPC(  29 ), SPECIES_MOLWT(  29 ) / 'GLY             ',   58.00 /
      DATA CHEMISTRY_SPC(  30 ), SPECIES_MOLWT(  30 ) / 'MGLY            ',   72.00 /
      DATA CHEMISTRY_SPC(  31 ), SPECIES_MOLWT(  31 ) / 'DCB1            ',   91.00 /
      DATA CHEMISTRY_SPC(  32 ), SPECIES_MOLWT(  32 ) / 'XO2             ',    1.00 /
      DATA CHEMISTRY_SPC(  33 ), SPECIES_MOLWT(  33 ) / 'DCB2            ',  110.00 /
      DATA CHEMISTRY_SPC(  34 ), SPECIES_MOLWT(  34 ) / 'BALD            ',  106.00 /
      DATA CHEMISTRY_SPC(  35 ), SPECIES_MOLWT(  35 ) / 'CHO             ',  139.00 /
      DATA CHEMISTRY_SPC(  36 ), SPECIES_MOLWT(  36 ) / 'OP1             ',   48.00 /
      DATA CHEMISTRY_SPC(  37 ), SPECIES_MOLWT(  37 ) / 'OP2             ',   62.00 /
      DATA CHEMISTRY_SPC(  38 ), SPECIES_MOLWT(  38 ) / 'PAA             ',   76.00 /
      DATA CHEMISTRY_SPC(  39 ), SPECIES_MOLWT(  39 ) / 'ONIT            ',  161.20 /
      DATA CHEMISTRY_SPC(  40 ), SPECIES_MOLWT(  40 ) / 'PAN             ',  121.00 /
      DATA CHEMISTRY_SPC(  41 ), SPECIES_MOLWT(  41 ) / 'N2O5            ',  108.00 /
      DATA CHEMISTRY_SPC(  42 ), SPECIES_MOLWT(  42 ) / 'SO2             ',   64.00 /
      DATA CHEMISTRY_SPC(  43 ), SPECIES_MOLWT(  43 ) / 'SULF            ',   98.00 /
      DATA CHEMISTRY_SPC(  44 ), SPECIES_MOLWT(  44 ) / 'SULRXN          ',   98.00 /
      DATA CHEMISTRY_SPC(  45 ), SPECIES_MOLWT(  45 ) / 'ETH             ',   30.00 /
      DATA CHEMISTRY_SPC(  46 ), SPECIES_MOLWT(  46 ) / 'HC3             ',   44.00 /
      DATA CHEMISTRY_SPC(  47 ), SPECIES_MOLWT(  47 ) / 'HC3P            ',   75.00 /
      DATA CHEMISTRY_SPC(  48 ), SPECIES_MOLWT(  48 ) / 'HC5             ',   72.00 /
      DATA CHEMISTRY_SPC(  49 ), SPECIES_MOLWT(  49 ) / 'HC5P            ',  103.00 /
      DATA CHEMISTRY_SPC(  50 ), SPECIES_MOLWT(  50 ) / 'HC8             ',  114.00 /
      DATA CHEMISTRY_SPC(  51 ), SPECIES_MOLWT(  51 ) / 'HC8P            ',  145.00 /
      DATA CHEMISTRY_SPC(  52 ), SPECIES_MOLWT(  52 ) / 'ETE             ',   28.00 /
      DATA CHEMISTRY_SPC(  53 ), SPECIES_MOLWT(  53 ) / 'ETEP            ',   77.00 /
      DATA CHEMISTRY_SPC(  54 ), SPECIES_MOLWT(  54 ) / 'OLTP            ',   91.00 /
      DATA CHEMISTRY_SPC(  55 ), SPECIES_MOLWT(  55 ) / 'OLI             ',   68.00 /
      DATA CHEMISTRY_SPC(  56 ), SPECIES_MOLWT(  56 ) / 'OLIP            ',  117.00 /
      DATA CHEMISTRY_SPC(  57 ), SPECIES_MOLWT(  57 ) / 'DIEN            ',   54.00 /
      DATA CHEMISTRY_SPC(  58 ), SPECIES_MOLWT(  58 ) / 'ACE             ',   26.00 /
      DATA CHEMISTRY_SPC(  59 ), SPECIES_MOLWT(  59 ) / 'ORA1            ',   46.00 /
      DATA CHEMISTRY_SPC(  60 ), SPECIES_MOLWT(  60 ) / 'BENZENE         ',   78.00 /
      DATA CHEMISTRY_SPC(  61 ), SPECIES_MOLWT(  61 ) / 'BENP            ',  127.00 /
      DATA CHEMISTRY_SPC(  62 ), SPECIES_MOLWT(  62 ) / 'EPX             ',  122.50 /
      DATA CHEMISTRY_SPC(  63 ), SPECIES_MOLWT(  63 ) / 'PHEN            ',   94.00 /
      DATA CHEMISTRY_SPC(  64 ), SPECIES_MOLWT(  64 ) / 'BENZRO2         ',  127.00 /
      DATA CHEMISTRY_SPC(  65 ), SPECIES_MOLWT(  65 ) / 'TOL             ',   92.00 /
      DATA CHEMISTRY_SPC(  66 ), SPECIES_MOLWT(  66 ) / 'TR2             ',  109.00 /
      DATA CHEMISTRY_SPC(  67 ), SPECIES_MOLWT(  67 ) / 'TLP1            ',   91.00 /
      DATA CHEMISTRY_SPC(  68 ), SPECIES_MOLWT(  68 ) / 'CSL             ',  108.00 /
      DATA CHEMISTRY_SPC(  69 ), SPECIES_MOLWT(  69 ) / 'TOLRO2          ',  141.00 /
      DATA CHEMISTRY_SPC(  70 ), SPECIES_MOLWT(  70 ) / 'XYM             ',  106.00 /
      DATA CHEMISTRY_SPC(  71 ), SPECIES_MOLWT(  71 ) / 'XY2             ',  124.00 /
      DATA CHEMISTRY_SPC(  72 ), SPECIES_MOLWT(  72 ) / 'XYL1            ',  156.00 /
      DATA CHEMISTRY_SPC(  73 ), SPECIES_MOLWT(  73 ) / 'XYLRO2          ',  155.00 /
      DATA CHEMISTRY_SPC(  74 ), SPECIES_MOLWT(  74 ) / 'XYP             ',  106.00 /
      DATA CHEMISTRY_SPC(  75 ), SPECIES_MOLWT(  75 ) / 'XYO             ',  106.00 /
      DATA CHEMISTRY_SPC(  76 ), SPECIES_MOLWT(  76 ) / 'XYO2            ',  155.00 /
      DATA CHEMISTRY_SPC(  77 ), SPECIES_MOLWT(  77 ) / 'ISO             ',   68.00 /
      DATA CHEMISTRY_SPC(  78 ), SPECIES_MOLWT(  78 ) / 'ISOP            ',  117.00 /
      DATA CHEMISTRY_SPC(  79 ), SPECIES_MOLWT(  79 ) / 'API             ',  136.00 /
      DATA CHEMISTRY_SPC(  80 ), SPECIES_MOLWT(  80 ) / 'APIP            ',  185.00 /
      DATA CHEMISTRY_SPC(  81 ), SPECIES_MOLWT(  81 ) / 'LIM             ',  136.00 /
      DATA CHEMISTRY_SPC(  82 ), SPECIES_MOLWT(  82 ) / 'LIMP            ',  185.00 /
      DATA CHEMISTRY_SPC(  83 ), SPECIES_MOLWT(  83 ) / 'RCO3            ',   90.00 /
      DATA CHEMISTRY_SPC(  84 ), SPECIES_MOLWT(  84 ) / 'ACTP            ',   89.00 /
      DATA CHEMISTRY_SPC(  85 ), SPECIES_MOLWT(  85 ) / 'MEKP            ',  103.00 /
      DATA CHEMISTRY_SPC(  86 ), SPECIES_MOLWT(  86 ) / 'KETP            ',  117.00 /
      DATA CHEMISTRY_SPC(  87 ), SPECIES_MOLWT(  87 ) / 'MACRO2          ',  119.10 /
      DATA CHEMISTRY_SPC(  88 ), SPECIES_MOLWT(  88 ) / 'MVKP            ',  119.00 /
      DATA CHEMISTRY_SPC(  89 ), SPECIES_MOLWT(  89 ) / 'UALP            ',  133.00 /
      DATA CHEMISTRY_SPC(  90 ), SPECIES_MOLWT(  90 ) / 'DCB3            ',   84.00 /
      DATA CHEMISTRY_SPC(  91 ), SPECIES_MOLWT(  91 ) / 'BALP            ',  137.00 /
      DATA CHEMISTRY_SPC(  92 ), SPECIES_MOLWT(  92 ) / 'ADDC            ',  125.00 /
      DATA CHEMISTRY_SPC(  93 ), SPECIES_MOLWT(  93 ) / 'MCT             ',  124.00 /
      DATA CHEMISTRY_SPC(  94 ), SPECIES_MOLWT(  94 ) / 'MCTO            ',  123.00 /
      DATA CHEMISTRY_SPC(  95 ), SPECIES_MOLWT(  95 ) / 'MOH             ',   32.00 /
      DATA CHEMISTRY_SPC(  96 ), SPECIES_MOLWT(  96 ) / 'EOH             ',   46.00 /
      DATA CHEMISTRY_SPC(  97 ), SPECIES_MOLWT(  97 ) / 'ROH             ',   60.00 /
      DATA CHEMISTRY_SPC(  98 ), SPECIES_MOLWT(  98 ) / 'ETEG            ',   62.00 /
      DATA CHEMISTRY_SPC(  99 ), SPECIES_MOLWT(  99 ) / 'ISHPA           ',  118.00 /
      DATA CHEMISTRY_SPC( 100 ), SPECIES_MOLWT( 100 ) / 'IEPOXA          ',  118.10 /
      DATA CHEMISTRY_SPC( 101 ), SPECIES_MOLWT( 101 ) / 'IEPOXB          ',  118.13 /
      DATA CHEMISTRY_SPC( 102 ), SPECIES_MOLWT( 102 ) / 'MAHP            ',  102.00 /
      DATA CHEMISTRY_SPC( 103 ), SPECIES_MOLWT( 103 ) / 'ORA2            ',   60.00 /
      DATA CHEMISTRY_SPC( 104 ), SPECIES_MOLWT( 104 ) / 'ORAP            ',  109.00 /
      DATA CHEMISTRY_SPC( 105 ), SPECIES_MOLWT( 105 ) / 'PPN             ',  135.00 /
      DATA CHEMISTRY_SPC( 106 ), SPECIES_MOLWT( 106 ) / 'MPAN            ',  148.00 /
      DATA CHEMISTRY_SPC( 107 ), SPECIES_MOLWT( 107 ) / 'IHPN            ',  179.07 /
      DATA CHEMISTRY_SPC( 108 ), SPECIES_MOLWT( 108 ) / 'IHNO            ',  162.07 /
      DATA CHEMISTRY_SPC( 109 ), SPECIES_MOLWT( 109 ) / 'INO2            ',  146.08 /
      DATA CHEMISTRY_SPC( 110 ), SPECIES_MOLWT( 110 ) / 'ICN             ',  145.00 /
      DATA CHEMISTRY_SPC( 111 ), SPECIES_MOLWT( 111 ) / 'TRPRXN          ',  136.00 /
      DATA CHEMISTRY_SPC( 112 ), SPECIES_MOLWT( 112 ) / 'PYAC            ',   88.10 /
      DATA CHEMISTRY_SPC( 113 ), SPECIES_MOLWT( 113 ) / 'MCTP            ',  172.00 /
      DATA CHEMISTRY_SPC( 114 ), SPECIES_MOLWT( 114 ) / 'OLNN            ',  136.00 /
      DATA CHEMISTRY_SPC( 115 ), SPECIES_MOLWT( 115 ) / 'OLND            ',  136.00 /
      DATA CHEMISTRY_SPC( 116 ), SPECIES_MOLWT( 116 ) / 'TOLNN           ',  230.20 /
      DATA CHEMISTRY_SPC( 117 ), SPECIES_MOLWT( 117 ) / 'TOLND           ',  230.20 /
      DATA CHEMISTRY_SPC( 118 ), SPECIES_MOLWT( 118 ) / 'ADCN            ',  156.00 /
      DATA CHEMISTRY_SPC( 119 ), SPECIES_MOLWT( 119 ) / 'TOLP            ',  141.00 /
      DATA CHEMISTRY_SPC( 120 ), SPECIES_MOLWT( 120 ) / 'PER1            ',  141.00 /
      DATA CHEMISTRY_SPC( 121 ), SPECIES_MOLWT( 121 ) / 'XYLP            ',  155.00 /
      DATA CHEMISTRY_SPC( 122 ), SPECIES_MOLWT( 122 ) / 'PER2            ',  157.00 /
      DATA CHEMISTRY_SPC( 123 ), SPECIES_MOLWT( 123 ) / 'XYOP            ',  155.00 /
      DATA CHEMISTRY_SPC( 124 ), SPECIES_MOLWT( 124 ) / 'MONIT           ',  147.12 /
      DATA CHEMISTRY_SPC( 125 ), SPECIES_MOLWT( 125 ) / 'DONIT           ',  149.14 /
      DATA CHEMISTRY_SPC( 126 ), SPECIES_MOLWT( 126 ) / 'AONIT           ',  189.12 /
      DATA CHEMISTRY_SPC( 127 ), SPECIES_MOLWT( 127 ) / 'ISOPND          ',  147.13 /
      DATA CHEMISTRY_SPC( 128 ), SPECIES_MOLWT( 128 ) / 'ISOPNB          ',  147.13 /
      DATA CHEMISTRY_SPC( 129 ), SPECIES_MOLWT( 129 ) / 'UHC             ',   72.00 /
      DATA CHEMISTRY_SPC( 130 ), SPECIES_MOLWT( 130 ) / 'DIBOO           ',  133.10 /
      DATA CHEMISTRY_SPC( 131 ), SPECIES_MOLWT( 131 ) / 'TONIT           ',  215.24 /
      DATA CHEMISTRY_SPC( 132 ), SPECIES_MOLWT( 132 ) / 'UTONIT          ',  215.24 /
      DATA CHEMISTRY_SPC( 133 ), SPECIES_MOLWT( 133 ) / 'HAC             ',   74.10 /
      DATA CHEMISTRY_SPC( 134 ), SPECIES_MOLWT( 134 ) / 'MACRN           ',  149.10 /
      DATA CHEMISTRY_SPC( 135 ), SPECIES_MOLWT( 135 ) / 'GLYC            ',   60.05 /
      DATA CHEMISTRY_SPC( 136 ), SPECIES_MOLWT( 136 ) / 'MVKN            ',  149.10 /
      DATA CHEMISTRY_SPC( 137 ), SPECIES_MOLWT( 137 ) / 'BAL1            ',  121.00 /
      DATA CHEMISTRY_SPC( 138 ), SPECIES_MOLWT( 138 ) / 'BAL2            ',  105.00 /
      DATA CHEMISTRY_SPC( 139 ), SPECIES_MOLWT( 139 ) / 'ISHPB           ',  118.00 /
      DATA CHEMISTRY_SPC( 140 ), SPECIES_MOLWT( 140 ) / 'ISHPD           ',  118.00 /
      DATA CHEMISTRY_SPC( 141 ), SPECIES_MOLWT( 141 ) / 'ISOPRXN         ',   68.00 /
      DATA CHEMISTRY_SPC( 142 ), SPECIES_MOLWT( 142 ) / 'VRP             ',  132.08 /
      DATA CHEMISTRY_SPC( 143 ), SPECIES_MOLWT( 143 ) / 'IEPOXD          ',  118.13 /
      DATA CHEMISTRY_SPC( 144 ), SPECIES_MOLWT( 144 ) / 'TONIN           ',  215.24 /
      DATA CHEMISTRY_SPC( 145 ), SPECIES_MOLWT( 145 ) / 'TONIH           ',  231.24 /
      DATA CHEMISTRY_SPC( 146 ), SPECIES_MOLWT( 146 ) / 'UTONIN          ',  215.24 /
      DATA CHEMISTRY_SPC( 147 ), SPECIES_MOLWT( 147 ) / 'HONIT           ',  264.00 /
      DATA CHEMISTRY_SPC( 148 ), SPECIES_MOLWT( 148 ) / 'IAP             ',  150.08 /
      DATA CHEMISTRY_SPC( 149 ), SPECIES_MOLWT( 149 ) / 'DHMOB           ',  132.10 /
      DATA CHEMISTRY_SPC( 150 ), SPECIES_MOLWT( 150 ) / 'UHCAP           ',  131.07 /
      DATA CHEMISTRY_SPC( 151 ), SPECIES_MOLWT( 151 ) / 'UHCP            ',  131.07 /
      DATA CHEMISTRY_SPC( 152 ), SPECIES_MOLWT( 152 ) / 'MOBA            ',  114.50 /
      DATA CHEMISTRY_SPC( 153 ), SPECIES_MOLWT( 153 ) / 'MOBAOO          ',  145.50 /
      DATA CHEMISTRY_SPC( 154 ), SPECIES_MOLWT( 154 ) / 'HPALD           ',  116.10 /
      DATA CHEMISTRY_SPC( 155 ), SPECIES_MOLWT( 155 ) / 'ISOPNBO2        ',  196.10 /
      DATA CHEMISTRY_SPC( 156 ), SPECIES_MOLWT( 156 ) / 'ISOPNDO2        ',  196.10 /
      DATA CHEMISTRY_SPC( 157 ), SPECIES_MOLWT( 157 ) / 'DHDN            ',  226.70 /
      DATA CHEMISTRY_SPC( 158 ), SPECIES_MOLWT( 158 ) / 'PROPNN          ',  120.10 /
      DATA CHEMISTRY_SPC( 159 ), SPECIES_MOLWT( 159 ) / 'ETHLN           ',  105.05 /
      DATA CHEMISTRY_SPC( 160 ), SPECIES_MOLWT( 160 ) / 'IMONIT          ',  181.10 /
      DATA CHEMISTRY_SPC( 161 ), SPECIES_MOLWT( 161 ) / 'ISNP            ',  197.14 /
      DATA CHEMISTRY_SPC( 162 ), SPECIES_MOLWT( 162 ) / 'IEPOXOO         ',  149.10 /
      DATA CHEMISTRY_SPC( 163 ), SPECIES_MOLWT( 163 ) / 'INO             ',  162.07 /
      DATA CHEMISTRY_SPC( 164 ), SPECIES_MOLWT( 164 ) / 'IHND            ',  147.10 /
      DATA CHEMISTRY_SPC( 165 ), SPECIES_MOLWT( 165 ) / 'IHNB            ',  147.10 /
      DATA CHEMISTRY_SPC( 166 ), SPECIES_MOLWT( 166 ) / 'INO2IN          ',  292.20 /
      DATA CHEMISTRY_SPC( 167 ), SPECIES_MOLWT( 167 ) / 'INHED           ',  163.08 /
      DATA CHEMISTRY_SPC( 168 ), SPECIES_MOLWT( 168 ) / 'NC4CO3          ',  176.05 /
      DATA CHEMISTRY_SPC( 169 ), SPECIES_MOLWT( 169 ) / 'R4NO            ',  165.05 /
      DATA CHEMISTRY_SPC( 170 ), SPECIES_MOLWT( 170 ) / 'R4N             ',  149.05 /
      DATA CHEMISTRY_SPC( 171 ), SPECIES_MOLWT( 171 ) / 'IDHNO2B         ',  180.08 /
      DATA CHEMISTRY_SPC( 172 ), SPECIES_MOLWT( 172 ) / 'IDHPN           ',  197.08 /
      DATA CHEMISTRY_SPC( 173 ), SPECIES_MOLWT( 173 ) / 'IDHNO2D         ',  180.08 /
      DATA CHEMISTRY_SPC( 174 ), SPECIES_MOLWT( 174 ) / 'INPD            ',  163.10 /
      DATA CHEMISTRY_SPC( 175 ), SPECIES_MOLWT( 175 ) / 'INPB            ',  163.10 /
      DATA CHEMISTRY_SPC( 176 ), SPECIES_MOLWT( 176 ) / 'INPHO2D         ',  195.06 /
      DATA CHEMISTRY_SPC( 177 ), SPECIES_MOLWT( 177 ) / 'INHEB           ',  163.08 /
      DATA CHEMISTRY_SPC( 178 ), SPECIES_MOLWT( 178 ) / 'INPHO2B         ',  195.06 /
      DATA CHEMISTRY_SPC( 179 ), SPECIES_MOLWT( 179 ) / 'IHDPN           ',  213.07 /
      DATA CHEMISTRY_SPC( 180 ), SPECIES_MOLWT( 180 ) / 'IHNO2           ',  178.06 /
      DATA CHEMISTRY_SPC( 181 ), SPECIES_MOLWT( 181 ) / 'TOLNRXN         ',  141.00 /
      DATA CHEMISTRY_SPC( 182 ), SPECIES_MOLWT( 182 ) / 'TOLHRXN         ',  141.00 /
      DATA CHEMISTRY_SPC( 183 ), SPECIES_MOLWT( 183 ) / 'XYLNRXN         ',  155.00 /
      DATA CHEMISTRY_SPC( 184 ), SPECIES_MOLWT( 184 ) / 'XYLHRXN         ',  155.00 /
      DATA CHEMISTRY_SPC( 185 ), SPECIES_MOLWT( 185 ) / 'BNZNRXN         ',  127.00 /
      DATA CHEMISTRY_SPC( 186 ), SPECIES_MOLWT( 186 ) / 'BNZHRXN         ',  127.00 /
      DATA CHEMISTRY_SPC( 187 ), SPECIES_MOLWT( 187 ) / 'SESQ            ',  204.00 /
      DATA CHEMISTRY_SPC( 188 ), SPECIES_MOLWT( 188 ) / 'SESQRXN         ',  204.00 /
      DATA CHEMISTRY_SPC( 189 ), SPECIES_MOLWT( 189 ) / 'NAPH            ',  128.20 /
      DATA CHEMISTRY_SPC( 190 ), SPECIES_MOLWT( 190 ) / 'PAHRO2          ',  187.20 /
      DATA CHEMISTRY_SPC( 191 ), SPECIES_MOLWT( 191 ) / 'PAHNRXN         ',  187.20 /
      DATA CHEMISTRY_SPC( 192 ), SPECIES_MOLWT( 192 ) / 'PAHHRXN         ',  187.20 /
      DATA CHEMISTRY_SPC( 193 ), SPECIES_MOLWT( 193 ) / 'SOAALK          ',  112.00 /
      DATA CHEMISTRY_SPC( 194 ), SPECIES_MOLWT( 194 ) / 'ALKRXN          ',  112.00 /
      DATA CHEMISTRY_SPC( 195 ), SPECIES_MOLWT( 195 ) / 'AISO3J          ',  168.20 /
      DATA CHEMISTRY_SPC( 196 ), SPECIES_MOLWT( 196 ) / 'AGLYJ           ',   66.40 /
      DATA CHEMISTRY_SPC( 197 ), SPECIES_MOLWT( 197 ) / 'AHISOPNDJ       ',  147.00 /
      DATA CHEMISTRY_SPC( 198 ), SPECIES_MOLWT( 198 ) / 'AHISOPNBJ       ',  147.00 /
      DATA CHEMISTRY_SPC( 199 ), SPECIES_MOLWT( 199 ) / 'AHMVKNJ         ',  149.00 /
      DATA CHEMISTRY_SPC( 200 ), SPECIES_MOLWT( 200 ) / 'AHMACRNJ        ',  149.00 /
      DATA CHEMISTRY_SPC( 201 ), SPECIES_MOLWT( 201 ) / 'AHDHDNJ         ',  226.00 /
      DATA CHEMISTRY_SPC( 202 ), SPECIES_MOLWT( 202 ) / 'AHIMONITJ       ',  181.00 /
      DATA CHEMISTRY_SPC( 203 ), SPECIES_MOLWT( 203 ) / 'AHUTONITJ       ',  215.00 /
      DATA CHEMISTRY_SPC( 204 ), SPECIES_MOLWT( 204 ) / 'AHUTONINJ       ',  215.00 /
      DATA CHEMISTRY_SPC( 205 ), SPECIES_MOLWT( 205 ) / 'AHTONITJ        ',  215.00 /
      DATA CHEMISTRY_SPC( 206 ), SPECIES_MOLWT( 206 ) / 'AHTONINJ        ',  215.00 /
      DATA CHEMISTRY_SPC( 207 ), SPECIES_MOLWT( 207 ) / 'AHTONIHJ        ',  231.00 /
      DATA CHEMISTRY_SPC( 208 ), SPECIES_MOLWT( 208 ) / 'AHHONITJ        ',  264.00 /
      DATA CHEMISTRY_SPC( 209 ), SPECIES_MOLWT( 209 ) / 'AHINHEJ         ',  163.00 /
      DATA CHEMISTRY_SPC( 210 ), SPECIES_MOLWT( 210 ) / 'AHIHPNJ         ',  179.00 /
      DATA CHEMISTRY_SPC( 211 ), SPECIES_MOLWT( 211 ) / 'AHIHDPNJ        ',  213.00 /
      DATA CHEMISTRY_SPC( 212 ), SPECIES_MOLWT( 212 ) / 'AHIDHPNJ        ',  197.00 /
      DATA CHEMISTRY_SPC( 213 ), SPECIES_MOLWT( 213 ) / 'AHR4NJ          ',  149.00 /
      DATA CHEMISTRY_SPC( 214 ), SPECIES_MOLWT( 214 ) / 'AHR4NOJ         ',  165.00 /
      DATA CHEMISTRY_SPC( 215 ), SPECIES_MOLWT( 215 ) / 'AHINPDJ         ',  163.00 /
      DATA CHEMISTRY_SPC( 216 ), SPECIES_MOLWT( 216 ) / 'AHINPBJ         ',  163.00 /
      DATA CHEMISTRY_SPC( 217 ), SPECIES_MOLWT( 217 ) / 'AHIHNDJ         ',  147.00 /
      DATA CHEMISTRY_SPC( 218 ), SPECIES_MOLWT( 218 ) / 'AHIHNBJ         ',  147.00 /
      DATA CHEMISTRY_SPC( 219 ), SPECIES_MOLWT( 219 ) / 'AHISNPJ         ',  197.00 /
      DATA CHEMISTRY_SPC( 220 ), SPECIES_MOLWT( 220 ) / 'AHMTHYJ         ',  185.00 /
      DATA CHEMISTRY_SPC( 221 ), SPECIES_MOLWT( 221 ) / 'AISOPNDJ        ',  147.00 /
      DATA CHEMISTRY_SPC( 222 ), SPECIES_MOLWT( 222 ) / 'AMTHYDJ         ',  185.00 /
      DATA CHEMISTRY_SPC( 223 ), SPECIES_MOLWT( 223 ) / 'AIHNDJ          ',  147.00 /
      DATA CHEMISTRY_SPC( 224 ), SPECIES_MOLWT( 224 ) / 'AINPDJ          ',  163.00 /
      DATA CHEMISTRY_SPC( 225 ), SPECIES_MOLWT( 225 ) / 'AISOPNBJ        ',  147.00 /
      DATA CHEMISTRY_SPC( 226 ), SPECIES_MOLWT( 226 ) / 'AINPBJ          ',  163.00 /
      DATA CHEMISTRY_SPC( 227 ), SPECIES_MOLWT( 227 ) / 'AIHNBJ          ',  147.00 /
      DATA CHEMISTRY_SPC( 228 ), SPECIES_MOLWT( 228 ) / 'AMVKNJ          ',  149.00 /
      DATA CHEMISTRY_SPC( 229 ), SPECIES_MOLWT( 229 ) / 'AMACRNJ         ',  149.00 /
      DATA CHEMISTRY_SPC( 230 ), SPECIES_MOLWT( 230 ) / 'ADHDNJ          ',  226.00 /
      DATA CHEMISTRY_SPC( 231 ), SPECIES_MOLWT( 231 ) / 'AIMONITJ        ',  181.00 /
      DATA CHEMISTRY_SPC( 232 ), SPECIES_MOLWT( 232 ) / 'AUTONITJ        ',  215.00 /
      DATA CHEMISTRY_SPC( 233 ), SPECIES_MOLWT( 233 ) / 'AUTONINJ        ',  215.00 /
      DATA CHEMISTRY_SPC( 234 ), SPECIES_MOLWT( 234 ) / 'ATONITJ         ',  215.00 /
      DATA CHEMISTRY_SPC( 235 ), SPECIES_MOLWT( 235 ) / 'ATONINJ         ',  215.00 /
      DATA CHEMISTRY_SPC( 236 ), SPECIES_MOLWT( 236 ) / 'ATONIHJ         ',  231.00 /
      DATA CHEMISTRY_SPC( 237 ), SPECIES_MOLWT( 237 ) / 'AHONITJ         ',  264.00 /
      DATA CHEMISTRY_SPC( 238 ), SPECIES_MOLWT( 238 ) / 'AIHPNJ          ',  179.00 /
      DATA CHEMISTRY_SPC( 239 ), SPECIES_MOLWT( 239 ) / 'AIHDPNJ         ',  213.00 /
      DATA CHEMISTRY_SPC( 240 ), SPECIES_MOLWT( 240 ) / 'AIDHPNJ         ',  197.00 /
      DATA CHEMISTRY_SPC( 241 ), SPECIES_MOLWT( 241 ) / 'AR4NJ           ',  149.00 /
      DATA CHEMISTRY_SPC( 242 ), SPECIES_MOLWT( 242 ) / 'AR4NOJ          ',  165.00 /
      DATA CHEMISTRY_SPC( 243 ), SPECIES_MOLWT( 243 ) / 'AISNPJ          ',  197.00 /
      DATA CHEMISTRY_SPC( 244 ), SPECIES_MOLWT( 244 ) / 'AINHEJ          ',  163.00 /
      DATA CHEMISTRY_SPC( 245 ), SPECIES_MOLWT( 245 ) / 'AXYL1J          ',  174.00 /
      DATA CHEMISTRY_SPC( 246 ), SPECIES_MOLWT( 246 ) / 'AOLGAJ          ',  206.00 /
      DATA CHEMISTRY_SPC( 247 ), SPECIES_MOLWT( 247 ) / 'AXYL2J          ',  185.00 /
      DATA CHEMISTRY_SPC( 248 ), SPECIES_MOLWT( 248 ) / 'ATOL1J          ',  163.00 /
      DATA CHEMISTRY_SPC( 249 ), SPECIES_MOLWT( 249 ) / 'ATOL2J          ',  175.00 /
      DATA CHEMISTRY_SPC( 250 ), SPECIES_MOLWT( 250 ) / 'ABNZ1J          ',  161.00 /
      DATA CHEMISTRY_SPC( 251 ), SPECIES_MOLWT( 251 ) / 'ABNZ2J          ',  134.00 /
      DATA CHEMISTRY_SPC( 252 ), SPECIES_MOLWT( 252 ) / 'ATRP1J          ',  177.00 /
      DATA CHEMISTRY_SPC( 253 ), SPECIES_MOLWT( 253 ) / 'AOLGBJ          ',  248.00 /
      DATA CHEMISTRY_SPC( 254 ), SPECIES_MOLWT( 254 ) / 'ATRP2J          ',  198.00 /
      DATA CHEMISTRY_SPC( 255 ), SPECIES_MOLWT( 255 ) / 'AISO1J          ',  132.00 /
      DATA CHEMISTRY_SPC( 256 ), SPECIES_MOLWT( 256 ) / 'AISO2J          ',  133.00 /
      DATA CHEMISTRY_SPC( 257 ), SPECIES_MOLWT( 257 ) / 'ASQTJ           ',  273.00 /
      DATA CHEMISTRY_SPC( 258 ), SPECIES_MOLWT( 258 ) / 'APAH1J          ',  195.60 /
      DATA CHEMISTRY_SPC( 259 ), SPECIES_MOLWT( 259 ) / 'APAH2J          ',  178.70 /
      DATA CHEMISTRY_SPC( 260 ), SPECIES_MOLWT( 260 ) / 'AALK1J          ',  225.00 /
      DATA CHEMISTRY_SPC( 261 ), SPECIES_MOLWT( 261 ) / 'AALK2J          ',  205.10 /
      DATA CHEMISTRY_SPC( 262 ), SPECIES_MOLWT( 262 ) / 'PCVOC           ',  170.00 /
      DATA CHEMISTRY_SPC( 263 ), SPECIES_MOLWT( 263 ) / 'PCSOARXN        ',  170.00 /
      DATA CHEMISTRY_SPC( 264 ), SPECIES_MOLWT( 264 ) / 'VLVPO1          ',  218.00 /
      DATA CHEMISTRY_SPC( 265 ), SPECIES_MOLWT( 265 ) / 'VSVPO1          ',  230.00 /
      DATA CHEMISTRY_SPC( 266 ), SPECIES_MOLWT( 266 ) / 'VSVPO2          ',  241.00 /
      DATA CHEMISTRY_SPC( 267 ), SPECIES_MOLWT( 267 ) / 'VSVPO3          ',  253.00 /
      DATA CHEMISTRY_SPC( 268 ), SPECIES_MOLWT( 268 ) / 'VIVPO1          ',  266.00 /
      DATA CHEMISTRY_SPC( 269 ), SPECIES_MOLWT( 269 ) / 'VLVOO1          ',  136.00 /
      DATA CHEMISTRY_SPC( 270 ), SPECIES_MOLWT( 270 ) / 'VLVOO2          ',  136.00 /
      DATA CHEMISTRY_SPC( 271 ), SPECIES_MOLWT( 271 ) / 'VSVOO2          ',  135.00 /
      DATA CHEMISTRY_SPC( 272 ), SPECIES_MOLWT( 272 ) / 'VSVOO3          ',  134.00 /
      DATA CHEMISTRY_SPC( 273 ), SPECIES_MOLWT( 273 ) / 'VSVOO1          ',  135.00 /



      LOGICAL   :: MAPPED_TO_CGRID   = .FALSE. 


! MAPPED_TO_CGRID declares whether CMAQ namelists were used to determine 
! the below values of CGRID_INDEX, SPECIES_TYPE, SPECIES_MOLWT, and CONVERT_CONC
      LOGICAL, PARAMETER, PRIVATE :: F = .FALSE.
      LOGICAL, PARAMETER, PRIVATE :: T = .TRUE.

      DATA CGRID_INDEX(   1 ), SPECIES_TYPE(   1 ), CONVERT_CONC(   1 ) /    1, 'GC', F /  ! O3
      DATA CGRID_INDEX(   2 ), SPECIES_TYPE(   2 ), CONVERT_CONC(   2 ) /    2, 'GC', F /  ! O3P
      DATA CGRID_INDEX(   3 ), SPECIES_TYPE(   3 ), CONVERT_CONC(   3 ) /    3, 'GC', F /  ! O1D
      DATA CGRID_INDEX(   4 ), SPECIES_TYPE(   4 ), CONVERT_CONC(   4 ) /    4, 'GC', F /  ! H2O2
      DATA CGRID_INDEX(   5 ), SPECIES_TYPE(   5 ), CONVERT_CONC(   5 ) /    5, 'GC', F /  ! HO
      DATA CGRID_INDEX(   6 ), SPECIES_TYPE(   6 ), CONVERT_CONC(   6 ) /    6, 'GC', F /  ! NO2
      DATA CGRID_INDEX(   7 ), SPECIES_TYPE(   7 ), CONVERT_CONC(   7 ) /    7, 'GC', F /  ! NO
      DATA CGRID_INDEX(   8 ), SPECIES_TYPE(   8 ), CONVERT_CONC(   8 ) /    8, 'GC', F /  ! NO3
      DATA CGRID_INDEX(   9 ), SPECIES_TYPE(   9 ), CONVERT_CONC(   9 ) /    9, 'GC', F /  ! HONO
      DATA CGRID_INDEX(  10 ), SPECIES_TYPE(  10 ), CONVERT_CONC(  10 ) /   10, 'GC', F /  ! HNO3
      DATA CGRID_INDEX(  11 ), SPECIES_TYPE(  11 ), CONVERT_CONC(  11 ) /   11, 'GC', F /  ! HNO4
      DATA CGRID_INDEX(  12 ), SPECIES_TYPE(  12 ), CONVERT_CONC(  12 ) /   12, 'GC', F /  ! HO2
      DATA CGRID_INDEX(  13 ), SPECIES_TYPE(  13 ), CONVERT_CONC(  13 ) /   13, 'GC', F /  ! HCHO
      DATA CGRID_INDEX(  14 ), SPECIES_TYPE(  14 ), CONVERT_CONC(  14 ) /   14, 'GC', F /  ! CO
      DATA CGRID_INDEX(  15 ), SPECIES_TYPE(  15 ), CONVERT_CONC(  15 ) /   15, 'GC', F /  ! ACD
      DATA CGRID_INDEX(  16 ), SPECIES_TYPE(  16 ), CONVERT_CONC(  16 ) /   16, 'GC', F /  ! MO2
      DATA CGRID_INDEX(  17 ), SPECIES_TYPE(  17 ), CONVERT_CONC(  17 ) /   17, 'GC', F /  ! ALD
      DATA CGRID_INDEX(  18 ), SPECIES_TYPE(  18 ), CONVERT_CONC(  18 ) /   18, 'GC', F /  ! ETHP
      DATA CGRID_INDEX(  19 ), SPECIES_TYPE(  19 ), CONVERT_CONC(  19 ) /   19, 'GC', F /  ! ACT
      DATA CGRID_INDEX(  20 ), SPECIES_TYPE(  20 ), CONVERT_CONC(  20 ) /   20, 'GC', F /  ! ACO3
      DATA CGRID_INDEX(  21 ), SPECIES_TYPE(  21 ), CONVERT_CONC(  21 ) /   21, 'GC', F /  ! UALD
      DATA CGRID_INDEX(  22 ), SPECIES_TYPE(  22 ), CONVERT_CONC(  22 ) /   22, 'GC', F /  ! KET
      DATA CGRID_INDEX(  23 ), SPECIES_TYPE(  23 ), CONVERT_CONC(  23 ) /   23, 'GC', F /  ! MEK
      DATA CGRID_INDEX(  24 ), SPECIES_TYPE(  24 ), CONVERT_CONC(  24 ) /   24, 'GC', F /  ! HKET
      DATA CGRID_INDEX(  25 ), SPECIES_TYPE(  25 ), CONVERT_CONC(  25 ) /   25, 'GC', F /  ! MACR
      DATA CGRID_INDEX(  26 ), SPECIES_TYPE(  26 ), CONVERT_CONC(  26 ) /   26, 'GC', F /  ! MACP
      DATA CGRID_INDEX(  27 ), SPECIES_TYPE(  27 ), CONVERT_CONC(  27 ) /   28, 'GC', F /  ! MVK
      DATA CGRID_INDEX(  28 ), SPECIES_TYPE(  28 ), CONVERT_CONC(  28 ) /   52, 'GC', F /  ! OLT
      DATA CGRID_INDEX(  29 ), SPECIES_TYPE(  29 ), CONVERT_CONC(  29 ) /   29, 'GC', F /  ! GLY
      DATA CGRID_INDEX(  30 ), SPECIES_TYPE(  30 ), CONVERT_CONC(  30 ) /   30, 'GC', F /  ! MGLY
      DATA CGRID_INDEX(  31 ), SPECIES_TYPE(  31 ), CONVERT_CONC(  31 ) /   31, 'GC', F /  ! DCB1
      DATA CGRID_INDEX(  32 ), SPECIES_TYPE(  32 ), CONVERT_CONC(  32 ) /   27, 'GC', F /  ! XO2
      DATA CGRID_INDEX(  33 ), SPECIES_TYPE(  33 ), CONVERT_CONC(  33 ) /   32, 'GC', F /  ! DCB2
      DATA CGRID_INDEX(  34 ), SPECIES_TYPE(  34 ), CONVERT_CONC(  34 ) /   33, 'GC', F /  ! BALD
      DATA CGRID_INDEX(  35 ), SPECIES_TYPE(  35 ), CONVERT_CONC(  35 ) /   34, 'GC', F /  ! CHO
      DATA CGRID_INDEX(  36 ), SPECIES_TYPE(  36 ), CONVERT_CONC(  36 ) /   35, 'GC', F /  ! OP1
      DATA CGRID_INDEX(  37 ), SPECIES_TYPE(  37 ), CONVERT_CONC(  37 ) /   36, 'GC', F /  ! OP2
      DATA CGRID_INDEX(  38 ), SPECIES_TYPE(  38 ), CONVERT_CONC(  38 ) /   37, 'GC', F /  ! PAA
      DATA CGRID_INDEX(  39 ), SPECIES_TYPE(  39 ), CONVERT_CONC(  39 ) /  139, 'GC', F /  ! ONIT
      DATA CGRID_INDEX(  40 ), SPECIES_TYPE(  40 ), CONVERT_CONC(  40 ) /   38, 'GC', F /  ! PAN
      DATA CGRID_INDEX(  41 ), SPECIES_TYPE(  41 ), CONVERT_CONC(  41 ) /   39, 'GC', F /  ! N2O5
      DATA CGRID_INDEX(  42 ), SPECIES_TYPE(  42 ), CONVERT_CONC(  42 ) /   40, 'GC', F /  ! SO2
      DATA CGRID_INDEX(  43 ), SPECIES_TYPE(  43 ), CONVERT_CONC(  43 ) /   41, 'GC', F /  ! SULF
      DATA CGRID_INDEX(  44 ), SPECIES_TYPE(  44 ), CONVERT_CONC(  44 ) /   42, 'GC', F /  ! SULRXN
      DATA CGRID_INDEX(  45 ), SPECIES_TYPE(  45 ), CONVERT_CONC(  45 ) /   43, 'GC', F /  ! ETH
      DATA CGRID_INDEX(  46 ), SPECIES_TYPE(  46 ), CONVERT_CONC(  46 ) /   44, 'GC', F /  ! HC3
      DATA CGRID_INDEX(  47 ), SPECIES_TYPE(  47 ), CONVERT_CONC(  47 ) /   45, 'GC', F /  ! HC3P
      DATA CGRID_INDEX(  48 ), SPECIES_TYPE(  48 ), CONVERT_CONC(  48 ) /   46, 'GC', F /  ! HC5
      DATA CGRID_INDEX(  49 ), SPECIES_TYPE(  49 ), CONVERT_CONC(  49 ) /   47, 'GC', F /  ! HC5P
      DATA CGRID_INDEX(  50 ), SPECIES_TYPE(  50 ), CONVERT_CONC(  50 ) /   48, 'GC', F /  ! HC8
      DATA CGRID_INDEX(  51 ), SPECIES_TYPE(  51 ), CONVERT_CONC(  51 ) /   49, 'GC', F /  ! HC8P
      DATA CGRID_INDEX(  52 ), SPECIES_TYPE(  52 ), CONVERT_CONC(  52 ) /   50, 'GC', F /  ! ETE
      DATA CGRID_INDEX(  53 ), SPECIES_TYPE(  53 ), CONVERT_CONC(  53 ) /   51, 'GC', F /  ! ETEP
      DATA CGRID_INDEX(  54 ), SPECIES_TYPE(  54 ), CONVERT_CONC(  54 ) /   53, 'GC', F /  ! OLTP
      DATA CGRID_INDEX(  55 ), SPECIES_TYPE(  55 ), CONVERT_CONC(  55 ) /   54, 'GC', F /  ! OLI
      DATA CGRID_INDEX(  56 ), SPECIES_TYPE(  56 ), CONVERT_CONC(  56 ) /   55, 'GC', F /  ! OLIP
      DATA CGRID_INDEX(  57 ), SPECIES_TYPE(  57 ), CONVERT_CONC(  57 ) /   56, 'GC', F /  ! DIEN
      DATA CGRID_INDEX(  58 ), SPECIES_TYPE(  58 ), CONVERT_CONC(  58 ) /   57, 'GC', F /  ! ACE
      DATA CGRID_INDEX(  59 ), SPECIES_TYPE(  59 ), CONVERT_CONC(  59 ) /   58, 'GC', F /  ! ORA1
      DATA CGRID_INDEX(  60 ), SPECIES_TYPE(  60 ), CONVERT_CONC(  60 ) /   59, 'GC', F /  ! BENZENE
      DATA CGRID_INDEX(  61 ), SPECIES_TYPE(  61 ), CONVERT_CONC(  61 ) /   60, 'GC', F /  ! BENP
      DATA CGRID_INDEX(  62 ), SPECIES_TYPE(  62 ), CONVERT_CONC(  62 ) /   61, 'GC', F /  ! EPX
      DATA CGRID_INDEX(  63 ), SPECIES_TYPE(  63 ), CONVERT_CONC(  63 ) /   62, 'GC', F /  ! PHEN
      DATA CGRID_INDEX(  64 ), SPECIES_TYPE(  64 ), CONVERT_CONC(  64 ) /   63, 'GC', F /  ! BENZRO2
      DATA CGRID_INDEX(  65 ), SPECIES_TYPE(  65 ), CONVERT_CONC(  65 ) /   64, 'GC', F /  ! TOL
      DATA CGRID_INDEX(  66 ), SPECIES_TYPE(  66 ), CONVERT_CONC(  66 ) /   65, 'GC', F /  ! TR2
      DATA CGRID_INDEX(  67 ), SPECIES_TYPE(  67 ), CONVERT_CONC(  67 ) /   66, 'GC', F /  ! TLP1
      DATA CGRID_INDEX(  68 ), SPECIES_TYPE(  68 ), CONVERT_CONC(  68 ) /   67, 'GC', F /  ! CSL
      DATA CGRID_INDEX(  69 ), SPECIES_TYPE(  69 ), CONVERT_CONC(  69 ) /   68, 'GC', F /  ! TOLRO2
      DATA CGRID_INDEX(  70 ), SPECIES_TYPE(  70 ), CONVERT_CONC(  70 ) /   69, 'GC', F /  ! XYM
      DATA CGRID_INDEX(  71 ), SPECIES_TYPE(  71 ), CONVERT_CONC(  71 ) /   70, 'GC', F /  ! XY2
      DATA CGRID_INDEX(  72 ), SPECIES_TYPE(  72 ), CONVERT_CONC(  72 ) /   71, 'GC', F /  ! XYL1
      DATA CGRID_INDEX(  73 ), SPECIES_TYPE(  73 ), CONVERT_CONC(  73 ) /   72, 'GC', F /  ! XYLRO2
      DATA CGRID_INDEX(  74 ), SPECIES_TYPE(  74 ), CONVERT_CONC(  74 ) /   73, 'GC', F /  ! XYP
      DATA CGRID_INDEX(  75 ), SPECIES_TYPE(  75 ), CONVERT_CONC(  75 ) /   74, 'GC', F /  ! XYO
      DATA CGRID_INDEX(  76 ), SPECIES_TYPE(  76 ), CONVERT_CONC(  76 ) /   75, 'GC', F /  ! XYO2
      DATA CGRID_INDEX(  77 ), SPECIES_TYPE(  77 ), CONVERT_CONC(  77 ) /   76, 'GC', F /  ! ISO
      DATA CGRID_INDEX(  78 ), SPECIES_TYPE(  78 ), CONVERT_CONC(  78 ) /   77, 'GC', F /  ! ISOP
      DATA CGRID_INDEX(  79 ), SPECIES_TYPE(  79 ), CONVERT_CONC(  79 ) /   79, 'GC', F /  ! API
      DATA CGRID_INDEX(  80 ), SPECIES_TYPE(  80 ), CONVERT_CONC(  80 ) /   80, 'GC', F /  ! APIP
      DATA CGRID_INDEX(  81 ), SPECIES_TYPE(  81 ), CONVERT_CONC(  81 ) /   82, 'GC', F /  ! LIM
      DATA CGRID_INDEX(  82 ), SPECIES_TYPE(  82 ), CONVERT_CONC(  82 ) /   83, 'GC', F /  ! LIMP
      DATA CGRID_INDEX(  83 ), SPECIES_TYPE(  83 ), CONVERT_CONC(  83 ) /   84, 'GC', F /  ! RCO3
      DATA CGRID_INDEX(  84 ), SPECIES_TYPE(  84 ), CONVERT_CONC(  84 ) /   85, 'GC', F /  ! ACTP
      DATA CGRID_INDEX(  85 ), SPECIES_TYPE(  85 ), CONVERT_CONC(  85 ) /   86, 'GC', F /  ! MEKP
      DATA CGRID_INDEX(  86 ), SPECIES_TYPE(  86 ), CONVERT_CONC(  86 ) /   87, 'GC', F /  ! KETP
      DATA CGRID_INDEX(  87 ), SPECIES_TYPE(  87 ), CONVERT_CONC(  87 ) /   88, 'GC', F /  ! MACRO2
      DATA CGRID_INDEX(  88 ), SPECIES_TYPE(  88 ), CONVERT_CONC(  88 ) /   89, 'GC', F /  ! MVKP
      DATA CGRID_INDEX(  89 ), SPECIES_TYPE(  89 ), CONVERT_CONC(  89 ) /   90, 'GC', F /  ! UALP
      DATA CGRID_INDEX(  90 ), SPECIES_TYPE(  90 ), CONVERT_CONC(  90 ) /   91, 'GC', F /  ! DCB3
      DATA CGRID_INDEX(  91 ), SPECIES_TYPE(  91 ), CONVERT_CONC(  91 ) /   92, 'GC', F /  ! BALP
      DATA CGRID_INDEX(  92 ), SPECIES_TYPE(  92 ), CONVERT_CONC(  92 ) /   93, 'GC', F /  ! ADDC
      DATA CGRID_INDEX(  93 ), SPECIES_TYPE(  93 ), CONVERT_CONC(  93 ) /   94, 'GC', F /  ! MCT
      DATA CGRID_INDEX(  94 ), SPECIES_TYPE(  94 ), CONVERT_CONC(  94 ) /   95, 'GC', F /  ! MCTO
      DATA CGRID_INDEX(  95 ), SPECIES_TYPE(  95 ), CONVERT_CONC(  95 ) /   96, 'GC', F /  ! MOH
      DATA CGRID_INDEX(  96 ), SPECIES_TYPE(  96 ), CONVERT_CONC(  96 ) /   97, 'GC', F /  ! EOH
      DATA CGRID_INDEX(  97 ), SPECIES_TYPE(  97 ), CONVERT_CONC(  97 ) /   98, 'GC', F /  ! ROH
      DATA CGRID_INDEX(  98 ), SPECIES_TYPE(  98 ), CONVERT_CONC(  98 ) /   99, 'GC', F /  ! ETEG
      DATA CGRID_INDEX(  99 ), SPECIES_TYPE(  99 ), CONVERT_CONC(  99 ) /  100, 'GC', F /  ! ISHPA
      DATA CGRID_INDEX( 100 ), SPECIES_TYPE( 100 ), CONVERT_CONC( 100 ) /  101, 'GC', F /  ! IEPOXA
      DATA CGRID_INDEX( 101 ), SPECIES_TYPE( 101 ), CONVERT_CONC( 101 ) /  133, 'GC', F /  ! IEPOXB
      DATA CGRID_INDEX( 102 ), SPECIES_TYPE( 102 ), CONVERT_CONC( 102 ) /  102, 'GC', F /  ! MAHP
      DATA CGRID_INDEX( 103 ), SPECIES_TYPE( 103 ), CONVERT_CONC( 103 ) /  103, 'GC', F /  ! ORA2
      DATA CGRID_INDEX( 104 ), SPECIES_TYPE( 104 ), CONVERT_CONC( 104 ) /  104, 'GC', F /  ! ORAP
      DATA CGRID_INDEX( 105 ), SPECIES_TYPE( 105 ), CONVERT_CONC( 105 ) /  105, 'GC', F /  ! PPN
      DATA CGRID_INDEX( 106 ), SPECIES_TYPE( 106 ), CONVERT_CONC( 106 ) /  106, 'GC', F /  ! MPAN
      DATA CGRID_INDEX( 107 ), SPECIES_TYPE( 107 ), CONVERT_CONC( 107 ) /  185, 'GC', F /  ! IHPN
      DATA CGRID_INDEX( 108 ), SPECIES_TYPE( 108 ), CONVERT_CONC( 108 ) /  193, 'GC', F /  ! IHNO
      DATA CGRID_INDEX( 109 ), SPECIES_TYPE( 109 ), CONVERT_CONC( 109 ) /  190, 'GC', F /  ! INO2
      DATA CGRID_INDEX( 110 ), SPECIES_TYPE( 110 ), CONVERT_CONC( 110 ) /  173, 'GC', F /  ! ICN
      DATA CGRID_INDEX( 111 ), SPECIES_TYPE( 111 ), CONVERT_CONC( 111 ) /   81, 'GC', F /  ! TRPRXN
      DATA CGRID_INDEX( 112 ), SPECIES_TYPE( 112 ), CONVERT_CONC( 112 ) /  195, 'GC', F /  ! PYAC
      DATA CGRID_INDEX( 113 ), SPECIES_TYPE( 113 ), CONVERT_CONC( 113 ) /  107, 'GC', F /  ! MCTP
      DATA CGRID_INDEX( 114 ), SPECIES_TYPE( 114 ), CONVERT_CONC( 114 ) /  108, 'GC', F /  ! OLNN
      DATA CGRID_INDEX( 115 ), SPECIES_TYPE( 115 ), CONVERT_CONC( 115 ) /  109, 'GC', F /  ! OLND
      DATA CGRID_INDEX( 116 ), SPECIES_TYPE( 116 ), CONVERT_CONC( 116 ) /  171, 'GC', F /  ! TOLNN
      DATA CGRID_INDEX( 117 ), SPECIES_TYPE( 117 ), CONVERT_CONC( 117 ) /  172, 'GC', F /  ! TOLND
      DATA CGRID_INDEX( 118 ), SPECIES_TYPE( 118 ), CONVERT_CONC( 118 ) /  110, 'GC', F /  ! ADCN
      DATA CGRID_INDEX( 119 ), SPECIES_TYPE( 119 ), CONVERT_CONC( 119 ) /  111, 'GC', F /  ! TOLP
      DATA CGRID_INDEX( 120 ), SPECIES_TYPE( 120 ), CONVERT_CONC( 120 ) /  112, 'GC', F /  ! PER1
      DATA CGRID_INDEX( 121 ), SPECIES_TYPE( 121 ), CONVERT_CONC( 121 ) /  113, 'GC', F /  ! XYLP
      DATA CGRID_INDEX( 122 ), SPECIES_TYPE( 122 ), CONVERT_CONC( 122 ) /  114, 'GC', F /  ! PER2
      DATA CGRID_INDEX( 123 ), SPECIES_TYPE( 123 ), CONVERT_CONC( 123 ) /  115, 'GC', F /  ! XYOP
      DATA CGRID_INDEX( 124 ), SPECIES_TYPE( 124 ), CONVERT_CONC( 124 ) /  136, 'GC', F /  ! MONIT
      DATA CGRID_INDEX( 125 ), SPECIES_TYPE( 125 ), CONVERT_CONC( 125 ) /  137, 'GC', F /  ! DONIT
      DATA CGRID_INDEX( 126 ), SPECIES_TYPE( 126 ), CONVERT_CONC( 126 ) /  138, 'GC', F /  ! AONIT
      DATA CGRID_INDEX( 127 ), SPECIES_TYPE( 127 ), CONVERT_CONC( 127 ) /  146, 'GC', F /  ! ISOPND
      DATA CGRID_INDEX( 128 ), SPECIES_TYPE( 128 ), CONVERT_CONC( 128 ) /  147, 'GC', F /  ! ISOPNB
      DATA CGRID_INDEX( 129 ), SPECIES_TYPE( 129 ), CONVERT_CONC( 129 ) /  159, 'GC', F /  ! UHC
      DATA CGRID_INDEX( 130 ), SPECIES_TYPE( 130 ), CONVERT_CONC( 130 ) /  163, 'GC', F /  ! DIBOO
      DATA CGRID_INDEX( 131 ), SPECIES_TYPE( 131 ), CONVERT_CONC( 131 ) /  142, 'GC', F /  ! TONIT
      DATA CGRID_INDEX( 132 ), SPECIES_TYPE( 132 ), CONVERT_CONC( 132 ) /  143, 'GC', F /  ! UTONIT
      DATA CGRID_INDEX( 133 ), SPECIES_TYPE( 133 ), CONVERT_CONC( 133 ) /  162, 'GC', F /  ! HAC
      DATA CGRID_INDEX( 134 ), SPECIES_TYPE( 134 ), CONVERT_CONC( 134 ) /  151, 'GC', F /  ! MACRN
      DATA CGRID_INDEX( 135 ), SPECIES_TYPE( 135 ), CONVERT_CONC( 135 ) /  165, 'GC', F /  ! GLYC
      DATA CGRID_INDEX( 136 ), SPECIES_TYPE( 136 ), CONVERT_CONC( 136 ) /  152, 'GC', F /  ! MVKN
      DATA CGRID_INDEX( 137 ), SPECIES_TYPE( 137 ), CONVERT_CONC( 137 ) /  116, 'GC', F /  ! BAL1
      DATA CGRID_INDEX( 138 ), SPECIES_TYPE( 138 ), CONVERT_CONC( 138 ) /  117, 'GC', F /  ! BAL2
      DATA CGRID_INDEX( 139 ), SPECIES_TYPE( 139 ), CONVERT_CONC( 139 ) /  132, 'GC', F /  ! ISHPB
      DATA CGRID_INDEX( 140 ), SPECIES_TYPE( 140 ), CONVERT_CONC( 140 ) /  134, 'GC', F /  ! ISHPD
      DATA CGRID_INDEX( 141 ), SPECIES_TYPE( 141 ), CONVERT_CONC( 141 ) /   78, 'GC', F /  ! ISOPRXN
      DATA CGRID_INDEX( 142 ), SPECIES_TYPE( 142 ), CONVERT_CONC( 142 ) /  157, 'GC', F /  ! VRP
      DATA CGRID_INDEX( 143 ), SPECIES_TYPE( 143 ), CONVERT_CONC( 143 ) /  135, 'GC', F /  ! IEPOXD
      DATA CGRID_INDEX( 144 ), SPECIES_TYPE( 144 ), CONVERT_CONC( 144 ) /  140, 'GC', F /  ! TONIN
      DATA CGRID_INDEX( 145 ), SPECIES_TYPE( 145 ), CONVERT_CONC( 145 ) /  144, 'GC', F /  ! TONIH
      DATA CGRID_INDEX( 146 ), SPECIES_TYPE( 146 ), CONVERT_CONC( 146 ) /  141, 'GC', F /  ! UTONIN
      DATA CGRID_INDEX( 147 ), SPECIES_TYPE( 147 ), CONVERT_CONC( 147 ) /  145, 'GC', F /  ! HONIT
      DATA CGRID_INDEX( 148 ), SPECIES_TYPE( 148 ), CONVERT_CONC( 148 ) /  156, 'GC', F /  ! IAP
      DATA CGRID_INDEX( 149 ), SPECIES_TYPE( 149 ), CONVERT_CONC( 149 ) /  155, 'GC', F /  ! DHMOB
      DATA CGRID_INDEX( 150 ), SPECIES_TYPE( 150 ), CONVERT_CONC( 150 ) /  161, 'GC', F /  ! UHCAP
      DATA CGRID_INDEX( 151 ), SPECIES_TYPE( 151 ), CONVERT_CONC( 151 ) /  160, 'GC', F /  ! UHCP
      DATA CGRID_INDEX( 152 ), SPECIES_TYPE( 152 ), CONVERT_CONC( 152 ) /  158, 'GC', F /  ! MOBA
      DATA CGRID_INDEX( 153 ), SPECIES_TYPE( 153 ), CONVERT_CONC( 153 ) /  166, 'GC', F /  ! MOBAOO
      DATA CGRID_INDEX( 154 ), SPECIES_TYPE( 154 ), CONVERT_CONC( 154 ) /  170, 'GC', F /  ! HPALD
      DATA CGRID_INDEX( 155 ), SPECIES_TYPE( 155 ), CONVERT_CONC( 155 ) /  168, 'GC', F /  ! ISOPNBO2
      DATA CGRID_INDEX( 156 ), SPECIES_TYPE( 156 ), CONVERT_CONC( 156 ) /  167, 'GC', F /  ! ISOPNDO2
      DATA CGRID_INDEX( 157 ), SPECIES_TYPE( 157 ), CONVERT_CONC( 157 ) /  154, 'GC', F /  ! DHDN
      DATA CGRID_INDEX( 158 ), SPECIES_TYPE( 158 ), CONVERT_CONC( 158 ) /  149, 'GC', F /  ! PROPNN
      DATA CGRID_INDEX( 159 ), SPECIES_TYPE( 159 ), CONVERT_CONC( 159 ) /  150, 'GC', F /  ! ETHLN
      DATA CGRID_INDEX( 160 ), SPECIES_TYPE( 160 ), CONVERT_CONC( 160 ) /  153, 'GC', F /  ! IMONIT
      DATA CGRID_INDEX( 161 ), SPECIES_TYPE( 161 ), CONVERT_CONC( 161 ) /  148, 'GC', F /  ! ISNP
      DATA CGRID_INDEX( 162 ), SPECIES_TYPE( 162 ), CONVERT_CONC( 162 ) /  169, 'GC', F /  ! IEPOXOO
      DATA CGRID_INDEX( 163 ), SPECIES_TYPE( 163 ), CONVERT_CONC( 163 ) /  191, 'GC', F /  ! INO
      DATA CGRID_INDEX( 164 ), SPECIES_TYPE( 164 ), CONVERT_CONC( 164 ) /  174, 'GC', F /  ! IHND
      DATA CGRID_INDEX( 165 ), SPECIES_TYPE( 165 ), CONVERT_CONC( 165 ) /  175, 'GC', F /  ! IHNB
      DATA CGRID_INDEX( 166 ), SPECIES_TYPE( 166 ), CONVERT_CONC( 166 ) /  182, 'GC', F /  ! INO2IN
      DATA CGRID_INDEX( 167 ), SPECIES_TYPE( 167 ), CONVERT_CONC( 167 ) /  184, 'GC', F /  ! INHED
      DATA CGRID_INDEX( 168 ), SPECIES_TYPE( 168 ), CONVERT_CONC( 168 ) /  194, 'GC', F /  ! NC4CO3
      DATA CGRID_INDEX( 169 ), SPECIES_TYPE( 169 ), CONVERT_CONC( 169 ) /  179, 'GC', F /  ! R4NO
      DATA CGRID_INDEX( 170 ), SPECIES_TYPE( 170 ), CONVERT_CONC( 170 ) /  178, 'GC', F /  ! R4N
      DATA CGRID_INDEX( 171 ), SPECIES_TYPE( 171 ), CONVERT_CONC( 171 ) /  188, 'GC', F /  ! IDHNO2B
      DATA CGRID_INDEX( 172 ), SPECIES_TYPE( 172 ), CONVERT_CONC( 172 ) /  180, 'GC', F /  ! IDHPN
      DATA CGRID_INDEX( 173 ), SPECIES_TYPE( 173 ), CONVERT_CONC( 173 ) /  187, 'GC', F /  ! IDHNO2D
      DATA CGRID_INDEX( 174 ), SPECIES_TYPE( 174 ), CONVERT_CONC( 174 ) /  176, 'GC', F /  ! INPD
      DATA CGRID_INDEX( 175 ), SPECIES_TYPE( 175 ), CONVERT_CONC( 175 ) /  177, 'GC', F /  ! INPB
      DATA CGRID_INDEX( 176 ), SPECIES_TYPE( 176 ), CONVERT_CONC( 176 ) /  186, 'GC', F /  ! INPHO2D
      DATA CGRID_INDEX( 177 ), SPECIES_TYPE( 177 ), CONVERT_CONC( 177 ) /  183, 'GC', F /  ! INHEB
      DATA CGRID_INDEX( 178 ), SPECIES_TYPE( 178 ), CONVERT_CONC( 178 ) /  189, 'GC', F /  ! INPHO2B
      DATA CGRID_INDEX( 179 ), SPECIES_TYPE( 179 ), CONVERT_CONC( 179 ) /  181, 'GC', F /  ! IHDPN
      DATA CGRID_INDEX( 180 ), SPECIES_TYPE( 180 ), CONVERT_CONC( 180 ) /  192, 'GC', F /  ! IHNO2
      DATA CGRID_INDEX( 181 ), SPECIES_TYPE( 181 ), CONVERT_CONC( 181 ) /  118, 'GC', F /  ! TOLNRXN
      DATA CGRID_INDEX( 182 ), SPECIES_TYPE( 182 ), CONVERT_CONC( 182 ) /  119, 'GC', F /  ! TOLHRXN
      DATA CGRID_INDEX( 183 ), SPECIES_TYPE( 183 ), CONVERT_CONC( 183 ) /  120, 'GC', F /  ! XYLNRXN
      DATA CGRID_INDEX( 184 ), SPECIES_TYPE( 184 ), CONVERT_CONC( 184 ) /  121, 'GC', F /  ! XYLHRXN
      DATA CGRID_INDEX( 185 ), SPECIES_TYPE( 185 ), CONVERT_CONC( 185 ) /  122, 'GC', F /  ! BNZNRXN
      DATA CGRID_INDEX( 186 ), SPECIES_TYPE( 186 ), CONVERT_CONC( 186 ) /  123, 'GC', F /  ! BNZHRXN
      DATA CGRID_INDEX( 187 ), SPECIES_TYPE( 187 ), CONVERT_CONC( 187 ) /  124, 'GC', F /  ! SESQ
      DATA CGRID_INDEX( 188 ), SPECIES_TYPE( 188 ), CONVERT_CONC( 188 ) /  125, 'GC', F /  ! SESQRXN
      DATA CGRID_INDEX( 189 ), SPECIES_TYPE( 189 ), CONVERT_CONC( 189 ) /  126, 'GC', F /  ! NAPH
      DATA CGRID_INDEX( 190 ), SPECIES_TYPE( 190 ), CONVERT_CONC( 190 ) /  127, 'GC', F /  ! PAHRO2
      DATA CGRID_INDEX( 191 ), SPECIES_TYPE( 191 ), CONVERT_CONC( 191 ) /  128, 'GC', F /  ! PAHNRXN
      DATA CGRID_INDEX( 192 ), SPECIES_TYPE( 192 ), CONVERT_CONC( 192 ) /  129, 'GC', F /  ! PAHHRXN
      DATA CGRID_INDEX( 193 ), SPECIES_TYPE( 193 ), CONVERT_CONC( 193 ) /  130, 'GC', F /  ! SOAALK
      DATA CGRID_INDEX( 194 ), SPECIES_TYPE( 194 ), CONVERT_CONC( 194 ) /  131, 'GC', F /  ! ALKRXN
      DATA CGRID_INDEX( 195 ), SPECIES_TYPE( 195 ), CONVERT_CONC( 195 ) /  270, 'AE', T /  ! AISO3J
      DATA CGRID_INDEX( 196 ), SPECIES_TYPE( 196 ), CONVERT_CONC( 196 ) /  291, 'AE', T /  ! AGLYJ
      DATA CGRID_INDEX( 197 ), SPECIES_TYPE( 197 ), CONVERT_CONC( 197 ) /  324, 'AE', T /  ! AHISOPNDJ
      DATA CGRID_INDEX( 198 ), SPECIES_TYPE( 198 ), CONVERT_CONC( 198 ) /  325, 'AE', T /  ! AHISOPNBJ
      DATA CGRID_INDEX( 199 ), SPECIES_TYPE( 199 ), CONVERT_CONC( 199 ) /  327, 'AE', T /  ! AHMVKNJ
      DATA CGRID_INDEX( 200 ), SPECIES_TYPE( 200 ), CONVERT_CONC( 200 ) /  328, 'AE', T /  ! AHMACRNJ
      DATA CGRID_INDEX( 201 ), SPECIES_TYPE( 201 ), CONVERT_CONC( 201 ) /  332, 'AE', T /  ! AHDHDNJ
      DATA CGRID_INDEX( 202 ), SPECIES_TYPE( 202 ), CONVERT_CONC( 202 ) /  334, 'AE', T /  ! AHIMONITJ
      DATA CGRID_INDEX( 203 ), SPECIES_TYPE( 203 ), CONVERT_CONC( 203 ) /  293, 'AE', T /  ! AHUTONITJ
      DATA CGRID_INDEX( 204 ), SPECIES_TYPE( 204 ), CONVERT_CONC( 204 ) /  295, 'AE', T /  ! AHUTONINJ
      DATA CGRID_INDEX( 205 ), SPECIES_TYPE( 205 ), CONVERT_CONC( 205 ) /  297, 'AE', T /  ! AHTONITJ
      DATA CGRID_INDEX( 206 ), SPECIES_TYPE( 206 ), CONVERT_CONC( 206 ) /  299, 'AE', T /  ! AHTONINJ
      DATA CGRID_INDEX( 207 ), SPECIES_TYPE( 207 ), CONVERT_CONC( 207 ) /  301, 'AE', T /  ! AHTONIHJ
      DATA CGRID_INDEX( 208 ), SPECIES_TYPE( 208 ), CONVERT_CONC( 208 ) /  303, 'AE', T /  ! AHHONITJ
      DATA CGRID_INDEX( 209 ), SPECIES_TYPE( 209 ), CONVERT_CONC( 209 ) /  305, 'AE', T /  ! AHINHEJ
      DATA CGRID_INDEX( 210 ), SPECIES_TYPE( 210 ), CONVERT_CONC( 210 ) /  316, 'AE', T /  ! AHIHPNJ
      DATA CGRID_INDEX( 211 ), SPECIES_TYPE( 211 ), CONVERT_CONC( 211 ) /  319, 'AE', T /  ! AHIHDPNJ
      DATA CGRID_INDEX( 212 ), SPECIES_TYPE( 212 ), CONVERT_CONC( 212 ) /  321, 'AE', T /  ! AHIDHPNJ
      DATA CGRID_INDEX( 213 ), SPECIES_TYPE( 213 ), CONVERT_CONC( 213 ) /  336, 'AE', T /  ! AHR4NJ
      DATA CGRID_INDEX( 214 ), SPECIES_TYPE( 214 ), CONVERT_CONC( 214 ) /  338, 'AE', T /  ! AHR4NOJ
      DATA CGRID_INDEX( 215 ), SPECIES_TYPE( 215 ), CONVERT_CONC( 215 ) /  307, 'AE', T /  ! AHINPDJ
      DATA CGRID_INDEX( 216 ), SPECIES_TYPE( 216 ), CONVERT_CONC( 216 ) /  309, 'AE', T /  ! AHINPBJ
      DATA CGRID_INDEX( 217 ), SPECIES_TYPE( 217 ), CONVERT_CONC( 217 ) /  312, 'AE', T /  ! AHIHNDJ
      DATA CGRID_INDEX( 218 ), SPECIES_TYPE( 218 ), CONVERT_CONC( 218 ) /  313, 'AE', T /  ! AHIHNBJ
      DATA CGRID_INDEX( 219 ), SPECIES_TYPE( 219 ), CONVERT_CONC( 219 ) /  340, 'AE', T /  ! AHISNPJ
      DATA CGRID_INDEX( 220 ), SPECIES_TYPE( 220 ), CONVERT_CONC( 220 ) /  342, 'AE', T /  ! AHMTHYJ
      DATA CGRID_INDEX( 221 ), SPECIES_TYPE( 221 ), CONVERT_CONC( 221 ) /  322, 'AE', T /  ! AISOPNDJ
      DATA CGRID_INDEX( 222 ), SPECIES_TYPE( 222 ), CONVERT_CONC( 222 ) /  341, 'AE', T /  ! AMTHYDJ
      DATA CGRID_INDEX( 223 ), SPECIES_TYPE( 223 ), CONVERT_CONC( 223 ) /  310, 'AE', T /  ! AIHNDJ
      DATA CGRID_INDEX( 224 ), SPECIES_TYPE( 224 ), CONVERT_CONC( 224 ) /  306, 'AE', T /  ! AINPDJ
      DATA CGRID_INDEX( 225 ), SPECIES_TYPE( 225 ), CONVERT_CONC( 225 ) /  323, 'AE', T /  ! AISOPNBJ
      DATA CGRID_INDEX( 226 ), SPECIES_TYPE( 226 ), CONVERT_CONC( 226 ) /  308, 'AE', T /  ! AINPBJ
      DATA CGRID_INDEX( 227 ), SPECIES_TYPE( 227 ), CONVERT_CONC( 227 ) /  311, 'AE', T /  ! AIHNBJ
      DATA CGRID_INDEX( 228 ), SPECIES_TYPE( 228 ), CONVERT_CONC( 228 ) /  326, 'AE', T /  ! AMVKNJ
      DATA CGRID_INDEX( 229 ), SPECIES_TYPE( 229 ), CONVERT_CONC( 229 ) /  329, 'AE', T /  ! AMACRNJ
      DATA CGRID_INDEX( 230 ), SPECIES_TYPE( 230 ), CONVERT_CONC( 230 ) /  331, 'AE', T /  ! ADHDNJ
      DATA CGRID_INDEX( 231 ), SPECIES_TYPE( 231 ), CONVERT_CONC( 231 ) /  333, 'AE', T /  ! AIMONITJ
      DATA CGRID_INDEX( 232 ), SPECIES_TYPE( 232 ), CONVERT_CONC( 232 ) /  292, 'AE', T /  ! AUTONITJ
      DATA CGRID_INDEX( 233 ), SPECIES_TYPE( 233 ), CONVERT_CONC( 233 ) /  294, 'AE', T /  ! AUTONINJ
      DATA CGRID_INDEX( 234 ), SPECIES_TYPE( 234 ), CONVERT_CONC( 234 ) /  296, 'AE', T /  ! ATONITJ
      DATA CGRID_INDEX( 235 ), SPECIES_TYPE( 235 ), CONVERT_CONC( 235 ) /  298, 'AE', T /  ! ATONINJ
      DATA CGRID_INDEX( 236 ), SPECIES_TYPE( 236 ), CONVERT_CONC( 236 ) /  300, 'AE', T /  ! ATONIHJ
      DATA CGRID_INDEX( 237 ), SPECIES_TYPE( 237 ), CONVERT_CONC( 237 ) /  302, 'AE', T /  ! AHONITJ
      DATA CGRID_INDEX( 238 ), SPECIES_TYPE( 238 ), CONVERT_CONC( 238 ) /  315, 'AE', T /  ! AIHPNJ
      DATA CGRID_INDEX( 239 ), SPECIES_TYPE( 239 ), CONVERT_CONC( 239 ) /  318, 'AE', T /  ! AIHDPNJ
      DATA CGRID_INDEX( 240 ), SPECIES_TYPE( 240 ), CONVERT_CONC( 240 ) /  320, 'AE', T /  ! AIDHPNJ
      DATA CGRID_INDEX( 241 ), SPECIES_TYPE( 241 ), CONVERT_CONC( 241 ) /  335, 'AE', T /  ! AR4NJ
      DATA CGRID_INDEX( 242 ), SPECIES_TYPE( 242 ), CONVERT_CONC( 242 ) /  337, 'AE', T /  ! AR4NOJ
      DATA CGRID_INDEX( 243 ), SPECIES_TYPE( 243 ), CONVERT_CONC( 243 ) /  339, 'AE', T /  ! AISNPJ
      DATA CGRID_INDEX( 244 ), SPECIES_TYPE( 244 ), CONVERT_CONC( 244 ) /  304, 'AE', T /  ! AINHEJ
      DATA CGRID_INDEX( 245 ), SPECIES_TYPE( 245 ), CONVERT_CONC( 245 ) /  217, 'AE', T /  ! AXYL1J
      DATA CGRID_INDEX( 246 ), SPECIES_TYPE( 246 ), CONVERT_CONC( 246 ) /  271, 'AE', T /  ! AOLGAJ
      DATA CGRID_INDEX( 247 ), SPECIES_TYPE( 247 ), CONVERT_CONC( 247 ) /  218, 'AE', T /  ! AXYL2J
      DATA CGRID_INDEX( 248 ), SPECIES_TYPE( 248 ), CONVERT_CONC( 248 ) /  220, 'AE', T /  ! ATOL1J
      DATA CGRID_INDEX( 249 ), SPECIES_TYPE( 249 ), CONVERT_CONC( 249 ) /  221, 'AE', T /  ! ATOL2J
      DATA CGRID_INDEX( 250 ), SPECIES_TYPE( 250 ), CONVERT_CONC( 250 ) /  223, 'AE', T /  ! ABNZ1J
      DATA CGRID_INDEX( 251 ), SPECIES_TYPE( 251 ), CONVERT_CONC( 251 ) /  224, 'AE', T /  ! ABNZ2J
      DATA CGRID_INDEX( 252 ), SPECIES_TYPE( 252 ), CONVERT_CONC( 252 ) /  229, 'AE', T /  ! ATRP1J
      DATA CGRID_INDEX( 253 ), SPECIES_TYPE( 253 ), CONVERT_CONC( 253 ) /  272, 'AE', T /  ! AOLGBJ
      DATA CGRID_INDEX( 254 ), SPECIES_TYPE( 254 ), CONVERT_CONC( 254 ) /  230, 'AE', T /  ! ATRP2J
      DATA CGRID_INDEX( 255 ), SPECIES_TYPE( 255 ), CONVERT_CONC( 255 ) /  231, 'AE', T /  ! AISO1J
      DATA CGRID_INDEX( 256 ), SPECIES_TYPE( 256 ), CONVERT_CONC( 256 ) /  232, 'AE', T /  ! AISO2J
      DATA CGRID_INDEX( 257 ), SPECIES_TYPE( 257 ), CONVERT_CONC( 257 ) /  233, 'AE', T /  ! ASQTJ
      DATA CGRID_INDEX( 258 ), SPECIES_TYPE( 258 ), CONVERT_CONC( 258 ) /  226, 'AE', T /  ! APAH1J
      DATA CGRID_INDEX( 259 ), SPECIES_TYPE( 259 ), CONVERT_CONC( 259 ) /  227, 'AE', T /  ! APAH2J
      DATA CGRID_INDEX( 260 ), SPECIES_TYPE( 260 ), CONVERT_CONC( 260 ) /  215, 'AE', T /  ! AALK1J
      DATA CGRID_INDEX( 261 ), SPECIES_TYPE( 261 ), CONVERT_CONC( 261 ) /  216, 'AE', T /  ! AALK2J
      DATA CGRID_INDEX( 262 ), SPECIES_TYPE( 262 ), CONVERT_CONC( 262 ) /  206, 'GC', F /  ! PCVOC
      DATA CGRID_INDEX( 263 ), SPECIES_TYPE( 263 ), CONVERT_CONC( 263 ) /  207, 'GC', F /  ! PCSOARXN
      DATA CGRID_INDEX( 264 ), SPECIES_TYPE( 264 ), CONVERT_CONC( 264 ) /  196, 'GC', F /  ! VLVPO1
      DATA CGRID_INDEX( 265 ), SPECIES_TYPE( 265 ), CONVERT_CONC( 265 ) /  197, 'GC', F /  ! VSVPO1
      DATA CGRID_INDEX( 266 ), SPECIES_TYPE( 266 ), CONVERT_CONC( 266 ) /  198, 'GC', F /  ! VSVPO2
      DATA CGRID_INDEX( 267 ), SPECIES_TYPE( 267 ), CONVERT_CONC( 267 ) /  199, 'GC', F /  ! VSVPO3
      DATA CGRID_INDEX( 268 ), SPECIES_TYPE( 268 ), CONVERT_CONC( 268 ) /  200, 'GC', F /  ! VIVPO1
      DATA CGRID_INDEX( 269 ), SPECIES_TYPE( 269 ), CONVERT_CONC( 269 ) /  201, 'GC', F /  ! VLVOO1
      DATA CGRID_INDEX( 270 ), SPECIES_TYPE( 270 ), CONVERT_CONC( 270 ) /  202, 'GC', F /  ! VLVOO2
      DATA CGRID_INDEX( 271 ), SPECIES_TYPE( 271 ), CONVERT_CONC( 271 ) /  204, 'GC', F /  ! VSVOO2
      DATA CGRID_INDEX( 272 ), SPECIES_TYPE( 272 ), CONVERT_CONC( 272 ) /  205, 'GC', F /  ! VSVOO3
      DATA CGRID_INDEX( 273 ), SPECIES_TYPE( 273 ), CONVERT_CONC( 273 ) /  203, 'GC', F /  ! VSVOO1

      INTEGER, PARAMETER :: N_ACT_SP = 273

      INTEGER, PARAMETER :: NRXNS = 638

      INTEGER            :: KUNITS

      DATA  KUNITS /   2 /

      INTEGER IRXXN

      REAL( 8 )          :: RTDAT( 3,NRXNS )

      INTEGER, PARAMETER :: NFALLOFF =  18
      REAL( 8 )          :: RFDAT( 5,NFALLOFF )

      INTEGER            :: KTYPE( NRXNS )

      DATA ( KTYPE( IRXXN ), IRXXN = 1, NRXNS ) /  & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    3,    3,    3,    3,    2,    3,    3, & ! 3   
     &      3,    1,    3,    3,    9,    9,    3,   10,   10,    3, & ! 4   
     &      9,    3,    3,    3,   10,   10,    8,    1,    1,    3, & ! 5   
     &      3,    3,   10,    5,    1,   10,    5,    3,   10,    9, & ! 6   
     &      3,    3,    3,    3,    3,   10,    3,    3,    3,   10, & ! 7   
     &      3,    3,    1,    1,    1,    3,    3,    3,    3,    3, & ! 8   
     &      3,    4,    3,    3,    1,    3,    3,    3,    1,    3, & ! 9   
     &      3,    3,    1,    3,    3,    3,    3,    3,    3,    3, & ! O   
     &      3,    1,    3,    3,    3,    1,    1,    3,    3,    1, & ! 1   
     &      1,    1,    3,    0,    1,    3,    3,    3,    3,    3, & ! 2   
     &      3,    3,    3,    3,    1,    1,    1,    1,    1,    1, & ! 3   
     &      4,    3,    3,    1,    3,    3,    1,    3,    3,    3, & ! 4   
     &      1,    3,    3,    3,    1,    1,    3,    1,    3,    1, & ! 5   
     &      1,    1,    1,    1,    1,   10,    5,   10,    5,    3, & ! 6   
     &      3,    3,    3,    1,    1,    1,    1,    1,    1,    3, & ! 7   
     &      1,    3,    3,    1,    3,    3,    3,    3,    1,    1, & ! 8   
     &      3,    3,    3,    1,    1,    3,    3,    3,    3,    1, & ! 9   
     &      1,    3,    3,    1,    1,    1,    3,    1,    1,    1, & ! O   
     &      1,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 1   
     &      3,    3,    3,    3,    3,    3,    3,    3,    1,    1, & ! 2   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 3   
     &      1,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 4   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 5   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 6   
     &      3,    3,    1,    1,    3,    3,    3,    3,    3,    3, & ! 7   
     &      3,    3,    1,    3,    3,    3,    3,    3,    3,    3, & ! 8   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 9   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! O   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 1   
     &      3,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    3,    3,    3, & ! 5   
     &      1,    3,    3,    3,    3,    3,    3,    3,    3,    1, & ! 6   
     &      1,    1,    1,    1,    3,    3,    3,    3,    3,    3, & ! 7   
     &      1,    1,    3,    3,    3,    1,    1,    1,    1,    1, & ! 8   
     &      1,    3,    3,    3,    3,    3,    1,    1,    8,    3, & ! 9   
     &      3,    3,    1,    3,    1,    1,    3,    1,    3,    1, & ! O   
     &      3,    3,    1,    3,    3,    3,    3,    1,    1,    1, & ! 1   
     &      1,    3,    1,    3,    1,    1,    3,    1,    3,    3, & ! 2   
     &      1,    1,    3,    3,    3,    3,    1,    3,    1,    1, & ! 3   
     &      3,    1,    3,    3,    1,    1,    3,    1,    1,    1, & ! 4   
     &      1,    1,    3,    3,    3,    3,    3,    3,    3,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    1, & ! 8   
     &      3,    1,    1,    3,    3,    1,    1,    3,    3,    1, & ! 9   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    1,    1,    3,    3, & ! 1   
     &      1,    3,    3,    3,    3,    3,    3,    1,    1,    1, & ! 2   
     &      1,    3,    3,    3,   -1,   -1,   12,   -1,   -1,   -1, & ! 3   
     &     -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1, & ! 4   
     &     -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1, & ! 5   
     &     -1,   -1,   -1,   -1,   -1,   -1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 8   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 9   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    1/     !  3   

      INTEGER            :: IRXBITS( NRXNS )

      DATA ( IRXBITS( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    0,    0,    0,    0,   20,    0,   16, & ! 3   
     &     32,    8,  128,    0,    0,    8,    0,    1,    1,    0, & ! 4   
     &      0,   16,    0,    0,    1,    1,    0,    0,    0,    0, & ! 5   
     &      0,    0,    1,    0,    8,    1,    0,    0,    1,    0, & ! 6   
     &     64,    0,    0,    0,    0,    1,    0,    0,    0,    1, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    2,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    1,    0,    1,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,  512,  512,  512,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,   16,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    0,    0,   16,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    1,    1,    0,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      INTEGER            :: IORDER( NRXNS )

      DATA ( IORDER( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    2,    2,    2,    2,    3,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    3,    2,    2,    2,    2, & ! 4   
     &      2,    3,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    1,    2,    2,    1,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    1,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 5   
     &      1,    1,    1,    1,    1,    2,    1,    2,    1,    2, & ! 6   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 8   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 9   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2/     !  3   

      INTEGER, PARAMETER :: KTN1 = 235
      INTEGER            :: KRX1( KTN1 )

      DATA ( KRX1( IRXXN ), IRXXN = 1, KTN1 ) / & 
     &     42,   58,   59,   65,   83,   84,   85,   95,   99,  103, & ! O   
     &    112,  116,  117,  120,  121,  122,  125,  135,  136,  137, & ! 1   
     &    138,  139,  140,  144,  147,  151,  155,  156,  158,  160, & ! 2   
     &    161,  162,  163,  164,  165,  174,  175,  176,  177,  178, & ! 3   
     &    179,  181,  184,  189,  190,  194,  195,  200,  201,  204, & ! 4   
     &    205,  206,  208,  209,  210,  211,  229,  230,  241,  273, & ! 5   
     &    274,  283,  322,  323,  324,  325,  326,  327,  328,  329, & ! 6   
     &    330,  331,  332,  333,  334,  335,  336,  337,  338,  339, & ! 7   
     &    340,  341,  342,  343,  344,  345,  346,  347,  348,  349, & ! 8   
     &    350,  351,  352,  353,  354,  355,  356,  357,  361,  370, & ! 9   
     &    371,  372,  373,  374,  381,  382,  386,  387,  388,  389, & ! O   
     &    390,  391,  397,  398,  403,  405,  406,  408,  410,  413, & ! 1   
     &    418,  419,  420,  421,  423,  425,  426,  428,  431,  432, & ! 2   
     &    437,  439,  440,  442,  445,  446,  448,  449,  450,  451, & ! 3   
     &    452,  490,  492,  493,  496,  497,  500,  501,  502,  503, & ! 4   
     &    504,  505,  506,  507,  508,  509,  517,  518,  521,  528, & ! 5   
     &    529,  530,  531,  567,  568,  569,  570,  571,  572,  573, & ! 6   
     &    574,  575,  576,  577,  578,  579,  580,  581,  582,  583, & ! 7   
     &    584,  585,  586,  587,  588,  589,  590,  591,  592,  593, & ! 8   
     &    594,  595,  596,  597,  598,  599,  600,  601,  602,  603, & ! 9   
     &    604,  605,  606,  607,  608,  609,  610,  611,  612,  613, & ! O   
     &    614,  615,  616,  617,  618,  619,  620,  621,  622,  623, & ! 1   
     &    624,  625,  626,  627,  628,  629,  630,  631,  632,  633, & ! 2   
     &    634,  635,  636,  637,  638/     !  3   

      INTEGER, PARAMETER :: KTN2 =   1
      INTEGER            :: KRX2( KTN2 )

      DATA ( KRX2( IRXXN ), IRXXN = 1, KTN2 ) / & 
     &     38/

      INTEGER, PARAMETER :: KTN3 = 276
      INTEGER            :: KRX3( KTN3 )

      DATA ( KRX3( IRXXN ), IRXXN = 1, KTN3 ) / & 
     &     34,   35,   36,   37,   39,   40,   41,   43,   44,   47, & ! O   
     &     50,   52,   53,   54,   60,   61,   62,   68,   71,   72, & ! 1   
     &     73,   74,   75,   77,   78,   79,   81,   82,   86,   87, & ! 2   
     &     88,   89,   90,   91,   93,   94,   96,   97,   98,  100, & ! 3   
     &    101,  102,  104,  105,  106,  107,  108,  109,  110,  111, & ! 4   
     &    113,  114,  115,  118,  119,  123,  126,  127,  128,  129, & ! 5   
     &    130,  131,  132,  133,  134,  142,  143,  145,  146,  148, & ! 6   
     &    149,  150,  152,  153,  154,  157,  159,  170,  171,  172, & ! 7   
     &    173,  180,  182,  183,  185,  186,  187,  188,  191,  192, & ! 8   
     &    193,  196,  197,  198,  199,  202,  203,  207,  212,  213, & ! 9   
     &    214,  215,  216,  217,  218,  219,  220,  221,  222,  223, & ! O   
     &    224,  225,  226,  227,  228,  231,  232,  233,  234,  235, & ! 1   
     &    236,  237,  238,  239,  240,  242,  243,  244,  245,  246, & ! 2   
     &    247,  248,  249,  250,  251,  252,  253,  254,  255,  256, & ! 3   
     &    257,  258,  259,  260,  261,  262,  263,  264,  265,  266, & ! 4   
     &    267,  268,  269,  270,  271,  272,  275,  276,  277,  278, & ! 5   
     &    279,  280,  281,  282,  284,  285,  286,  287,  288,  289, & ! 6   
     &    290,  291,  292,  293,  294,  295,  296,  297,  298,  299, & ! 7   
     &    300,  301,  302,  303,  304,  305,  306,  307,  308,  309, & ! 8   
     &    310,  311,  312,  313,  314,  315,  316,  317,  318,  319, & ! 9   
     &    320,  321,  358,  359,  360,  362,  363,  364,  365,  366, & ! O   
     &    367,  368,  369,  375,  376,  377,  378,  379,  380,  383, & ! 1   
     &    384,  385,  392,  393,  394,  395,  396,  400,  401,  402, & ! 2   
     &    404,  407,  409,  411,  412,  414,  415,  416,  417,  422, & ! 3   
     &    424,  427,  429,  430,  433,  434,  435,  436,  438,  441, & ! 4   
     &    443,  444,  447,  453,  454,  455,  456,  457,  458,  459, & ! 5   
     &    491,  494,  495,  498,  499,  519,  520,  522,  523,  524, & ! 6   
     &    525,  526,  527,  532,  533,  534/     !  7   

      INTEGER, PARAMETER :: KTN4 =   2
      INTEGER            :: KRX4( KTN4 )

      DATA ( KRX4( IRXXN ), IRXXN = 1, KTN4 ) / & 
     &     92,  141/

      INTEGER, PARAMETER :: KTN5 =   4
      INTEGER            :: KRX5( KTN5 )

      DATA ( KRX5( IRXXN ), IRXXN = 1, KTN5 ) / & 
     &     64,   67,  167,  169/

      INTEGER, PARAMETER :: KTN6 =   0
      INTEGER            :: KRX6( 1 )

      DATA   KRX6( 1 ) / 0 /

      INTEGER, PARAMETER :: KTN7 =   0
      INTEGER            :: KRX7( 1 )

      DATA   KRX7( 1 ) / 0 /

      INTEGER, PARAMETER :: NWM =   1
      INTEGER            :: NRXWM( NWM )

      DATA ( NRXWM( IRXXN ), IRXXN = 1, NWM ) /  & 
     &     38/
      REAL,    PARAMETER :: ATM_AIR = 1.00000E+06

      INTEGER, PARAMETER :: NWW =   3
      INTEGER            :: NRXWW( NWW )

      DATA ( NRXWW( IRXXN ), IRXXN = 1, NWW ) / & 
     &     42,   46,   65/

      INTEGER, PARAMETER :: NWO2 =   5
      INTEGER            :: NRXWO2( NWO2 )

      DATA ( NRXWO2( IRXXN ), IRXXN = 1, NWO2 ) / & 
     &     38,   40,   52,  498,  519/
      REAL,    PARAMETER :: ATM_O2 = 2.09500E+05

      INTEGER, PARAMETER :: NWN2 =   1
      INTEGER            :: NRXWN2( NWN2 )

      DATA ( NRXWN2( IRXXN ), IRXXN = 1, NWN2 ) / & 
     &     41/
      REAL,    PARAMETER :: ATM_N2 = 7.80800E+05

      INTEGER, PARAMETER :: NWCH4 =   1
      INTEGER            :: NRXWCH4( NWCH4 )

      DATA ( NRXWCH4( IRXXN ), IRXXN = 1, NWCH4 ) / & 
     &     71/
      REAL,    PARAMETER :: ATM_CH4 = 1.85000E+00

      INTEGER, PARAMETER :: NWH2 =   1
      INTEGER            :: NRXWH2( NWH2 )

      DATA ( NRXWH2( IRXXN ), IRXXN = 1, NWH2 ) / & 
     &     43/
      REAL,    PARAMETER :: ATM_H2 = 5.60000E-01

      INTEGER, PARAMETER :: MXPRD =  21
      INTEGER            :: IRR( NRXNS,MXPRD+3 )

      DATA ( IRR( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    4,    6,    8,    8,    9,   10,   11,   13, & ! O   
     &     13,   15,   17,   19,   21,   23,   22,   24,   25,   27, & ! 1   
     &     29,   29,   29,   30,   31,   33,   34,   36,   37,   38, & ! 2   
     &     39,   40,   40,    1,    1,    1,    1,    2,    2,    3, & ! 3   
     &      3,    3,    5,    5,   12,   12,    4,    7,    7,    7, & ! 4   
     &      7,    7,    9,    6,    6,    6,   10,    8,    8,    8, & ! 5   
     &      8,    8,    8,   41,   41,    6,   11,   11,   42,   14, & ! 6   
     &      5,   45,   46,   48,   50,   52,   28,   55,   57,   58, & ! 7   
     &     60,   65,   70,   74,   75,   77,   79,   81,   13,   15, & ! 8   
     &     17,   19,   23,   22,   24,   25,   27,   21,   29,   30, & ! 9   
     &     31,   33,   90,   34,   63,   68,   62,   93,   95,   96, & ! O   
     &     97,   98,   36,   37,   99,  102,   59,  103,   38,   40, & ! 1   
     &    105,  106,   39,  107,  109,   52,   28,   55,   57,   77, & ! 2   
     &     79,   81,   25,   27,   21,   31,   33,   90,   62,   94, & ! 3   
     &     52,   28,   55,   57,   77,   79,   81,   13,   15,   17, & ! 4   
     &     25,   21,   29,   30,   63,   68,   62,   93,  106,   66, & ! 5   
     &    119,   71,  121,   76,  123,   20,   40,   83,  105,   26, & ! 6   
     &    106,   16,   18,   47,   49,   51,   53,   54,   56,   61, & ! 7   
     &     67,  119,  120,   72,  121,  122,  123,   78,   80,   82, & ! 8   
     &     20,   83,   84,   85,   86,   26,   87,   88,   89,   91, & ! 9   
     &    137,   92,  113,  104,  114,  115,  118,   32,  138,   35, & ! O   
     &     94,   16,   18,   47,   49,   51,   53,   54,   56,   61, & ! 1   
     &     67,  119,  120,   72,  121,  122,  123,   78,   80,   82, & ! 2   
     &     20,   83,   84,   85,   86,   26,   87,   88,   89,   92, & ! 3   
     &     35,  113,  104,  114,  115,  118,   32,   16,   18,   47, & ! 4   
     &     49,   51,   53,   54,   56,   61,   67,  119,  120,   72, & ! 5   
     &    121,  122,  123,   78,   80,   82,   20,   83,   84,   85, & ! 6   
     &     86,   26,   87,   88,   89,   91,  137,   92,  113,  104, & ! 7   
     &    114,  115,  118,   32,   18,   47,   49,   51,   53,   54, & ! 8   
     &     56,   61,   67,  119,  120,   72,  121,  122,  123,   78, & ! 9   
     &     80,   82,   20,   83,   84,   85,   86,   26,   87,   88, & ! O   
     &     89,   91,  137,   92,  113,  104,  114,  115,  118,   32, & ! 1   
     &     83,   16,   18,   47,   49,   51,   53,   54,   56,   61, & ! 2   
     &     67,  119,  120,   72,  121,  122,  123,   78,   80,   82, & ! 3   
     &     20,   83,   84,   85,   86,   26,   87,   88,   89,   91, & ! 4   
     &    137,   92,  113,  104,  114,  115,  118,  114,  114,  115, & ! 5   
     &     32,   32,   32,   99,  139,  140,  139,  140,   87,  124, & ! 6   
     &    126,  125,  116,  117,  116,  117,  116,  117,  116,  117, & ! 7   
     &    116,  117,  116,  116,  117,  144,  146,  146,  131,  132, & ! 8   
     &    132,  144,  146,  131,  132,  145,  145,  145,  147,  133, & ! 9   
     &    130,  130,  130,  130,  130,  129,  129,  129,  151,  151, & ! O   
     &    150,  151,  151,  150,  151,  150,  150,  150,  112,  152, & ! 1   
     &    153,  153,  153,  153,  153,  149,  148,  135,  135,   78, & ! 2   
     &    154,  152,  128,  127,  155,  156,  155,  155,  155,  156, & ! 3   
     &    156,  156,  156,  155,  127,  128,  161,  158,  159,  134, & ! 4   
     &    136,  160,  142,  100,  101,  143,  162,  162,  162,  154, & ! 5   
     &    133,  135,  102,  112,   99,  139,  140,  129,  152,  142, & ! 6   
     &    161,  148,  149,  128,  127,  160,  125,  131,  126,  132, & ! 7   
     &    147,  144,  146,  145,  124,  158,  159,  134,  136,  109, & ! 8   
     &    110,  168,  110,  171,  173,  164,  165,  163,  109,  174, & ! 9   
     &    175,  174,  175,  178,  176,  167,  177,  170,  169,  174, & ! O   
     &    175,  110,  170,  169,  164,  164,  163,  180,  108,  180, & ! 1   
     &    180,   69,   69,   73,   73,   64,   64,  187,  187,  187, & ! 2   
     &    189,  190,  190,  193,   41,    6,    1,  100,  101,  143, & ! 3   
     &     29,   30,  127,  128,  136,  134,  157,  160,  132,  146, & ! 4   
     &    131,  144,  145,  147,  167,  177,  107,  179,  172,  170, & ! 5   
     &    169,  174,  175,  164,  165,  161,  197,  217,  215,  198, & ! 6   
     &    216,  218,  199,  200,  201,  202,  203,  204,  205,  206, & ! 7   
     &    207,  208,  210,  211,  212,  213,  214,  219,  209,  221, & ! 8   
     &    223,  224,  225,  226,  227,  228,  229,  230,  231,  232, & ! 9   
     &    233,  234,  235,  236,  237,  238,  239,  240,  241,  242, & ! O   
     &    243,  244,  245,  247,  248,  249,  250,  251,  252,  254, & ! 1   
     &    255,  256,  257,  258,  259,  260,  261,  262,  264,  265, & ! 2   
     &    266,  267,  268,  269,  270,  273,  271,  272/     !  3   

      DATA ( IRR( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    5,   12,    7,    6,    0,    1,    0, & ! 3   
     &      0,    0,    0,   12,   12,   12,    5,    2,    5,   12, & ! 4   
     &     12,    7,    5,    2,    2,    5,    5,    5,   12,    7, & ! 5   
     &      6,    8,    6,    0,    0,   12,    0,    5,    5,    5, & ! 6   
     &      0,    5,    5,    5,    5,    5,    5,    5,    5,    5, & ! 7   
     &      5,    5,    5,    5,    5,    5,    5,    5,    5,    5, & ! 8   
     &      5,    5,    5,    5,    5,    5,    5,    5,    5,    5, & ! 9   
     &      5,    5,    5,    5,    5,    5,    5,    5,    5,    5, & ! O   
     &      5,    5,    5,    5,    5,    5,    5,    5,    5,    5, & ! 1   
     &      5,    5,    5,    0,    8,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      8,    8,    8,    8,    8,    8,    8,    8,    8,    8, & ! 4   
     &      8,    8,    8,    8,    8,    8,    8,    8,    8,    0, & ! 5   
     &      0,    0,    0,    0,    0,    6,    0,    6,    0,    6, & ! 6   
     &      0,    7,    7,    7,    7,    7,    7,    7,    7,    7, & ! 7   
     &      7,    7,    7,    7,    7,    7,    7,    7,    7,    7, & ! 8   
     &      7,    7,    7,    7,    7,    7,    7,    7,    7,    7, & ! 9   
     &      7,    7,    7,    7,    7,    7,    7,    7,    6,    6, & ! O   
     &      6,   12,   12,   12,   12,   12,   12,   12,   12,   12, & ! 1   
     &     12,   12,   12,   12,   12,   12,   12,   12,   12,   12, & ! 2   
     &     12,   12,   12,   12,   12,   12,   12,   12,   12,   12, & ! 3   
     &     12,   12,   12,   12,   12,   12,   12,   16,   16,   16, & ! 4   
     &     16,   16,   16,   16,   16,   16,   16,   16,   16,   16, & ! 5   
     &     16,   16,   16,   16,   16,   16,   16,   16,   16,   16, & ! 6   
     &     16,   16,   16,   16,   16,   16,   16,   16,   16,   16, & ! 7   
     &     16,   16,   16,   16,   20,   20,   20,   20,   20,   20, & ! 8   
     &     20,   20,   20,   20,   20,   20,   20,   20,   20,   20, & ! 9   
     &     20,   20,   20,   20,   20,   20,   20,   20,   20,   20, & ! O   
     &     20,   20,   20,   20,   20,   20,   20,   20,   20,   20, & ! 1   
     &     83,    8,    8,    8,    8,    8,    8,    8,    8,    8, & ! 2   
     &      8,    8,    8,    8,    8,    8,    8,    8,    8,    8, & ! 3   
     &      8,    8,    8,    8,    8,    8,    8,    8,    8,    8, & ! 4   
     &      8,    8,    8,    8,    8,    8,    8,  114,  115,  115, & ! 5   
     &      8,   83,   32,    5,    5,    5,    5,    5,    0,    5, & ! 6   
     &      5,    5,    7,    7,   12,   12,   16,   16,   20,   20, & ! 7   
     &      8,    8,  116,  117,  117,    5,    5,    1,    5,    5, & ! 8   
     &      1,    8,    8,    8,    8,    8,    5,    1,    5,    5, & ! 9   
     &      7,   12,   16,   20,    8,    1,    8,    5,    7,   16, & ! O   
     &     16,   20,    8,    7,   12,   12,   20,    8,    5,    5, & ! 1   
     &      7,   12,   16,   20,    8,    5,    5,    5,    8,    0, & ! 2   
     &      5,    1,    5,    5,    7,    7,   16,   20,    8,   16, & ! 3   
     &     20,    8,   12,   12,    1,    1,    5,    5,    5,    5, & ! 4   
     &      5,    5,    5,    5,    5,    5,   12,    7,    8,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  109, & ! 8   
     &      8,    8,    5,   12,   12,    5,    5,    0,   12,    5, & ! 9   
     &      5,    5,    5,   12,   12,    5,    5,    5,    5,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    8,    0,   12, & ! 1   
     &    180,    7,   12,    7,   12,    7,   12,    1,    5,    8, & ! 2   
     &      5,    7,   12,    5,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    5,    5,    5, & ! 2   
     &      5,    5,    5,    5,    5,    5,    5,    5/     !  3   

      DATA ( IRR( IRXXN,  3 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &      2,    3,    5,    2,    7,    2,    5,    5,    5,   14, & ! O   
     &     12,   12,   12,   16,   12,   16,   18,   12,   26,   28, & ! 1   
     &     14,   13,   12,   12,   12,   12,   35,    5,    5,    5, & ! 2   
     &     12,   20,   16,   12,    5,    6,    8,    1,    0,    2, & ! 3   
     &      2,    5,   12,    0,    4,    4,   12,    6,    9,    6, & ! 4   
     &     10,    6,    6,    7,    8,   10,    8,   12,    5,    6, & ! 5   
     &      7,    6,   41,    6,   10,   11,   12,    6,   12,   12, & ! 6   
     &     16,   18,   47,   49,   12,   53,   54,   56,   56,    5, & ! 7   
     &     12,   12,   12,   12,   12,   78,   80,   82,   12,   20, & ! 8   
     &     83,   84,   85,   86,   12,   26,   88,   20,   12,   20, & ! 9   
     &     12,   12,   12,   91,   12,   12,   12,   94,   12,   12, & ! O   
     &     12,   12,    5,    5,  100,   26,   12,   16,    5,   32, & ! 1   
     &     32,    6,   47,  108,  110,    5,    5,    5,    2,    5, & ! 2   
     &      5,    5,   30,   20,    5,    5,    5,    5,    5,  113, & ! 3   
     &    114,  114,  114,  114,  109,  116,  116,   12,   20,   83, & ! 4   
     &     26,   26,   12,   20,   35,   35,    5,   94,   26,    5, & ! 5   
     &      5,    5,    5,    5,    5,   40,   20,  105,   83,  106, & ! 6   
     &     26,   12,   12,   12,   12,   12,   12,   12,   12,   12, & ! 7   
     &      6,   12,   12,    6,   12,   12,   12,   27,   12,  132, & ! 8   
     &     16,   18,   20,   12,   12,    6,    6,  135,   12,  137, & ! 9   
     &    138,   12,   94,    6,    6,    6,    6,    6,  126,  126, & ! O   
     &    126,   36,   37,   37,   37,   37,   37,   37,   37,   37, & ! 1   
     &     37,   37,   37,   37,   37,   37,   37,   99,   37,   37, & ! 2   
     &      5,    5,    5,   37,   37,  102,   37,  142,   37,   37, & ! 3   
     &     68,   37,   37,  124,  124,  126,   37,   12,   12,   12, & ! 4   
     &     12,   12,   12,   12,   12,   12,   12,   12,   12,   12, & ! 5   
     &     12,   12,   12,   13,   12,   12,   12,   12,   12,   12, & ! 6   
     &     12,   12,   24,   97,   12,   12,   12,   12,   12,   13, & ! 7   
     &     12,   12,   12,   12,   12,   12,   12,   12,   12,   12, & ! 8   
     &     12,   12,   16,   12,   12,   16,   12,   12,   12,   12, & ! 9   
     &     12,   12,   16,   16,   16,   12,   12,   16,  133,   24, & ! O   
     &     12,   16,   16,   12,   12,   16,   12,   16,   12,   16, & ! 1   
     &     18,   12,   12,   12,   12,   12,   12,   17,   12,   12, & ! 2   
     &      6,   12,   12,    6,   12,   12,   12,   25,   12,   12, & ! 3   
     &     16,   18,   20,   12,   12,    6,  133,    6,   12,  137, & ! 4   
     &    138,   12,    6,    6,   12,    6,    6,   12,   12,    6, & ! 5   
     &      6,   18,    0,   78,   78,   78,  100,  143,  133,   24, & ! 6   
     &     33,   21,  144,   13,  145,  145,   13,   13,  144,   13, & ! 7   
     &    144,   13,  144,   13,   13,  147,  147,  147,  147,  147, & ! 8   
     &    147,  147,  147,  147,  147,  147,  147,  147,    8,   30, & ! 9   
     &      6,  148,  149,  135,    6,   30,   10,  151,  149,  149, & ! O   
     &    103,   16,    6,    6,  148,  148,   16,    6,   20,  153, & ! 1   
     &     17,   37,   17,   17,    6,   14,  150,   29,   83,  154, & ! 2   
     &      5,   30,  155,  156,    6,    6,  134,  160,  134,   13, & ! 3   
     &    136,  136,  161,  161,  158,   13,  156,    6,   13,  103, & ! 4   
     &     59,   24,   88,  162,  162,  162,  133,  133,  133,    5, & ! 5   
     &     20,   12,   20,   20,    5,    5,    5,  150,  112,    5, & ! 6   
     &      5,    5,   12,   78,   78,   24,   21,   22,   22,   21, & ! 7   
     &     24,   22,   21,   22,   22,    6,   12,    6,  135,  163, & ! 8   
     &    167,  158,  169,  172,  172,  173,  171,  110,   27,   12, & ! 9   
     &     12,  167,  177,  179,  179,  133,  177,  158,  158,  163, & ! O   
     &    163,  158,    6,    5,   12,   12,  180,  108,  107,  107, & ! 1   
     &    108,    7,   12,    7,   12,    7,   12,    1,    5,    8, & ! 2   
     &      5,    7,   12,    5,   10,    9,    0,  195,  195,  195, & ! 3   
     &    196,  196,  197,  198,  199,  200,  201,  202,  203,  204, & ! 4   
     &    205,  206,  207,  208,  209,  209,  210,  211,  212,  213, & ! 5   
     &    214,  215,  216,  217,  218,  219,   10,   10,   10,   10, & ! 6   
     &     10,   10,   10,   10,   10,   10,   10,   10,   10,   10, & ! 7   
     &     10,   10,   10,   10,   10,   10,   10,   10,   10,   10, & ! 8   
     &     10,   10,   10,   10,   10,   10,   10,   10,   10,   10, & ! 9   
     &     10,   10,   10,   10,   10,   10,   10,   10,   10,   10, & ! O   
     &     10,   10,  246,  246,  246,  246,  246,  246,  253,  253, & ! 1   
     &    253,  253,  253,  246,  246,  246,  246,    5,    5,    5, & ! 2   
     &      5,    5,    5,    5,    5,    5,    5,    5/     !  3   

      DATA ( IRR( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    7,    0,    6,    7,    6,   12,    0, & ! O   
     &     14,   16,   18,   20,   20,   18,   20,   20,   20,   14, & ! 1   
     &      0,   14,   14,   20,   20,   20,   12,   12,   12,   16, & ! 2   
     &      6,    6,    8,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    5, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    6,    6,    0, & ! 5   
     &      6,    0,    0,    8,    0,    0,    6,    0,   43,    0, & ! 6   
     &      0,    0,    0,    0,   51,    0,    0,    0,    0,   12, & ! 7   
     &     61,   66,   71,   71,   76,    0,    0,    0,   14,    0, & ! 8   
     &      0,    0,    0,    0,   30,   87,    0,   89,   14,   14, & ! 9   
     &     14,   14,   26,    0,   92,   92,   32,    0,   13,   15, & ! O   
     &     17,   17,   16,   47,  101,    0,    0,  104,   20,    8, & ! 1   
     &      8,   24,    6,    5,   27,   12,   12,   12,    5,   12, & ! 2   
     &     12,   12,   13,   14,   12,   12,   12,   12,   12,    0, & ! 3   
     &    115,  115,  115,  115,    0,  117,  117,   14,   10,   10, & ! 4   
     &     10,   10,   14,   14,   92,   92,   12,   10,    6,   12, & ! 5   
     &     12,   12,   12,   12,   12,    0,    6,    0,    6,    0, & ! 6   
     &      6,    6,    6,   16,   16,   18,    6,    6,    6,    6, & ! 7   
     &     34,    6,    6,   34,    6,    6,    6,   25,    6,   12, & ! 8   
     &      6,    6,    6,    6,   20,   14,   12,   20,    6,    6, & ! 9   
     &      6,    6,    6,   29,   12,   13,   29,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,  139,  111,  111, & ! 2   
     &     16,   18,   20,    0,    0,    1,    0,  135,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,   13,   13,   16, & ! 4   
     &     16,   18,   13,   13,   13,   90,   13,   13,   13,   13, & ! 5   
     &     13,   13,   13,   12,   13,   13,   16,   16,   20,   13, & ! 6   
     &     13,   16,   13,   13,   14,  137,  138,   13,   94,   12, & ! 7   
     &     13,    6,    6,   13,   16,   16,   16,   16,   16,   16, & ! 8   
     &     16,   16,   34,   16,   16,   34,   16,   16,   16,   16, & ! 9   
     &     16,   16,    0,   18,   20,   16,   16,   20,   14,  103, & ! O   
     &     16,  137,  138,   16,   16,   29,   16,    6,   16,    0, & ! 1   
     &      0,   13,    6,   16,   16,   18,    6,   13,   17,    6, & ! 2   
     &     34,    6,    6,   34,    6,    6,    6,   27,    6,    6, & ! 3   
     &      6,    6,    6,    6,    6,   13,   14,   30,    6,    6, & ! 4   
     &      6,    6,   94,   29,    6,   13,   29,  124,    6,   13, & ! 5   
     &      0,    0,    0,   48,   48,   48,  101,   49,   14,    6, & ! 6   
     &      6,    6,    6,   17,    0,    0,   12,   12,  146,   17, & ! 7   
     &    146,   17,  146,   17,   17,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   12, & ! 9   
     &     12,    5,   13,   30,   12,    5,  150,  150,  135,   13, & ! O   
     &     13,  149,  149,  152,  149,  103,   12,   12,    0,    0, & ! 1   
     &     12,    0,   12,   12,   12,   12,  151,   12,   10,   12, & ! 2   
     &      0,    5,  100,    0,   12,   12,   95,  134,  136,   12, & ! 3   
     &    133,  133,    0,    0,  103,   14,  155,   30,    6,   13, & ! 4   
     &     30,    6,   22,    0,    0,    0,  135,  135,  135,   12, & ! 5   
     &     13,   13,    5,   12,  129,  129,  129,   12,   29,   20, & ! 6   
     &     12,  152,   14,    6,    6,    6,    6,    6,    6,    6, & ! 7   
     &      6,    6,    6,    6,    6,   20,   14,   14,    6,  110, & ! 8   
     &      6,   14,   14,    5,    5,  143,    0,   12,   25,  109, & ! 9   
     &    109,  143,    5,    5,  159,   14,  158,   12,    5,    5, & ! O   
     &      5,   14,   20,   30,    6,    6,    0,    6,   12,    0, & ! 1   
     &    107,  181,  182,  183,  184,  185,  186,  188,  188,  188, & ! 2   
     &    190,  191,  192,  194,    0,   10,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,  220,  220,  220,  220, & ! 6   
     &    220,  220,  220,  220,  220,  220,  220,  220,  220,  220, & ! 7   
     &    220,  220,  220,  220,  220,  220,  220,  220,  220,  222, & ! 8   
     &    222,  222,  222,  222,  222,  222,  222,  222,  222,  222, & ! 9   
     &    222,  222,  222,  222,  222,  222,  222,  222,  222,  222, & ! O   
     &    222,  222,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,  263,  264,  264, & ! 2   
     &    264,  264,  264,  269,  269,  269,  269,  269/     !  3   

      DATA ( IRR( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    6,    0, & ! O   
     &      0,   14,   14,    0,   14,   20,    0,   13,   13,   20, & ! 1   
     &      0,    0,    0,   14,   32,   32,   14,   13,   17,    0, & ! 2   
     &     17,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,   10,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,   44,    0, & ! 6   
     &      0,    0,    0,    0,   17,    0,    0,    0,    0,   14, & ! 7   
     &     62,   67,   72,   72,   72,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &     17,   23,   14,    0,   35,   35,   14,    0,    0,    0, & ! O   
     &     15,    0,   13,   32,    5,    0,    0,    0,   32,   13, & ! 1   
     &     13,    0,    0,    0,   25,   14,   16,   16,   12,   16, & ! 2   
     &     18,   18,    5,   59,   16,   83,   83,   14,   14,    0, & ! 3   
     &      0,    0,    0,   25,    0,    0,    0,   10,    0,    0, & ! 4   
     &      0,    0,   10,   10,  118,  118,   14,    0,    0,  119, & ! 5   
     &    120,   83,  122,   83,  122,    0,    0,    0,    0,    0, & ! 6   
     &      0,   13,   15,   18,   18,   32,   13,   13,   15,   33, & ! 7   
     &      0,   33,   34,    0,   90,   30,   29,    6,   13,   21, & ! 8   
     &      0,    0,   13,   13,   32,   13,  133,   30,   14,    0, & ! 9   
     &      0,   24,    0,   12,  124,   17,   37,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,  140,    0,    0, & ! 2   
     &    103,  103,   13,    0,    0,  103,    0,   20,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,   95,   15,   18, & ! 4   
     &     18,   32,   17,   17,   17,   13,   34,   29,   30,   34, & ! 5   
     &     33,   30,   29,   27,   17,   55,   13,   13,   13,   31, & ! 6   
     &     31,   14,   12,   24,   13,   13,   13,   24,   13,   29, & ! 7   
     &    124,   13,   13,    0,   15,   18,   18,   18,   13,   13, & ! 8   
     &     17,   33,    0,   33,   30,    0,   33,   30,   29,   13, & ! 9   
     &     17,   55,    0,    0,   13,   13,   31,   13,   30,   30, & ! O   
     &     14,    0,    0,   24,   94,    0,  124,   13,    6,    0, & ! 1   
     &      0,    6,   15,   32,   18,   32,   13,   12,   22,   33, & ! 2   
     &      0,   33,   30,    0,   90,   30,   29,   12,   17,   55, & ! 3   
     &      0,    0,   13,   13,   31,   16,   12,   12,   14,    0, & ! 4   
     &      0,   24,    0,   12,  124,   17,   37,    0,   13,   17, & ! 5   
     &      0,    0,    0,    5,    5,    5,    5,    5,    5,    0, & ! 6   
     &      0,    0,   12,   22,    0,    0,  144,   17,   16,   22, & ! 7   
     &      6,   22,   12,   22,   22,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &    135,  135,  135,   12,  135,   14,    0,    0,   30,   12, & ! O   
     &     12,  135,  135,   17,   30,    1,  152,  152,    0,    0, & ! 1   
     &      6,    0,   13,   16,   17,  133,  130,   83,    0,  141, & ! 2   
     &      0,   14,    6,    0,  157,  157,  136,  136,  133,  160, & ! 3   
     &    159,  159,    0,    0,  135,    5,  160,    0,    0,    8, & ! 4   
     &     13,    0,    5,    0,    0,    0,   29,   29,   29,  133, & ! 5   
     &     12,   14,    0,    0,  130,  130,  130,   20,   12,  135, & ! 6   
     &    158,   17,   86,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,   13,   13,  133,   20,   25, & ! 8   
     &    168,   12,   12,   12,   12,    6,    0,    6,    6,    0, & ! 9   
     &      0,    6,  178,   12,    5,    6,   29,   14,   14,    0, & ! O   
     &      0,   12,   18,   13,  129,  129,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,  265,  265, & ! 2   
     &    265,  265,  265,  270,  270,  270,  270,  270/     !  3   

      DATA ( IRR( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    8,    0, & ! O   
     &      0,    0,    0,    0,   13,    0,    0,    0,   16,   16, & ! 1   
     &      0,    0,    0,    0,   14,   14,    0,    0,    0,    0, & ! 2   
     &     22,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,   24,    0,    0,    0,    0,   29, & ! 7   
     &     63,   68,   68,   68,   68,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &     22,   29,   29,    0,   93,   93,   17,    0,    0,    0, & ! O   
     &      0,    0,    0,   17,   49,    0,    0,    0,   13,    0, & ! 1   
     &      0,    0,    0,    0,    6,   13,   18,   18,   16,   20, & ! 2   
     &     86,   86,   14,    4,   20,   32,   32,   29,   34,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,   10,   10,   29,    0,    0,  120, & ! 5   
     &     33,  121,   33,  122,   33,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   32,   32,    6,   17,   15,   17,   90, & ! 7   
     &      0,  126,   30,    0,  126,   31,   30,  127,   17,   13, & ! 8   
     &      0,    0,    0,   31,    6,   16,   14,   13,   13,    0, & ! 9   
     &      0,   29,    0,    0,    0,   22,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    5,    0,    0, & ! 2   
     &     38,   38,   37,    0,    0,    5,    0,    5,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,   95,   32, & ! 4   
     &     32,   13,   95,   22,   22,   33,    0,   33,   31,    0, & ! 5   
     &      0,   31,   30,   25,   22,   25,  103,  103,   95,   95, & ! 6   
     &     95,   20,  133,   95,   17,    0,    0,   29,    0,    0, & ! 7   
     &      0,   17,   29,    0,  103,   32,   32,   32,   17,   17, & ! 8   
     &     22,   90,    0,    0,   31,    0,    0,   31,   30,   25, & ! 9   
     &     22,   13,    0,    0,  103,   31,  103,   14,   13,   12, & ! O   
     &     13,    0,    0,   29,    0,    0,    0,   17,   29,    0, & ! 1   
     &      0,    0,    0,   18,   32,    6,   17,    6,    6,   90, & ! 2   
     &      0,    0,   31,    0,    0,   31,   30,    6,   22,   13, & ! 3   
     &      0,    0,    0,   31,    0,   14,   30,   13,   13,    0, & ! 4   
     &      0,   29,    0,    0,    0,   22,    0,    0,   17,   22, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    6,    0,    0,  146,   22,   12,   16, & ! 7   
     &     12,    6,    0,   12,  144,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &     30,   30,   30,   16,   30,   12,    0,    0,  133,  135, & ! O   
     &    152,   30,   30,   14,  135,    5,   17,   17,    0,    0, & ! 1   
     &      0,    0,   97,    0,    0,   17,    0,    0,    0,    0, & ! 2   
     &      0,   12,    0,    0,  136,  136,   12,   13,  135,  136, & ! 3   
     &     59,   59,    0,    0,    4,    6,    5,    0,    0,   59, & ! 4   
     &    112,    0,    0,    0,    0,    0,   30,   30,   30,   30, & ! 5   
     &      0,    0,    0,    0,   27,   27,   27,   14,   14,   30, & ! 6   
     &    135,   14,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    6,   12,    0,  164, & ! 8   
     &     10,    6,  170,  133,  133,    0,    0,   13,    5,    0, & ! 9   
     &      0,    5,    0,   13,   12,   13,  135,    0,    0,    0, & ! O   
     &      0,    0,   16,    6,  130,  130,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,  266,  266, & ! 2   
     &    266,  266,  266,  273,  273,  273,  273,  273/     !  3   

      DATA ( IRR( IRXXN,  8 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,   17,    0,    0,    0,   14,    0, & ! 1   
     &      0,    0,    0,    0,   29,   29,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   59, & ! 7   
     &     64,   69,   73,   73,   73,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &     29,   30,   30,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,   22,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,   12,   59,   47,   47,   20,   26, & ! 2   
     &      4,    4,   20,   12,   32,   14,   14,   31,   29,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    6,    0,    0,   33, & ! 5   
     &     68,  122,   68,  123,   68,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    6,    6,   17,    0,   17,   19,   29, & ! 7   
     &      0,    0,   31,    0,    0,   90,   31,  128,   19,  133, & ! 8   
     &      0,    0,    0,    0,   17,    0,   13,   12,   17,    0, & ! 9   
     &      0,   37,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,   12,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   16,    0,   22,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,   96,   13, & ! 4   
     &     13,   17,   98,   95,   95,   29,    0,    0,    0,    0, & ! 5   
     &      0,   90,   31,   95,   95,   95,    0,    0,   97,   97, & ! 6   
     &     97,  103,   14,   30,   22,    0,    0,   37,    0,    0, & ! 7   
     &      0,   22,   37,    0,    0,   13,   13,   17,  103,   22, & ! 8   
     &    103,   29,    0,    0,    0,    0,    0,   90,   31,   27, & ! 9   
     &    103,   25,    0,    0,    0,  103,    0,    0,   24,   13, & ! O   
     &     17,    0,    0,   37,    0,    0,    0,   22,   37,    0, & ! 1   
     &      0,    0,    0,    6,    6,   17,    0,   23,   19,   29, & ! 2   
     &      0,    0,   34,    0,    0,   90,   31,   13,  111,   25, & ! 3   
     &      0,    0,    0,    0,    0,   20,   13,  135,   17,    0, & ! 4   
     &      0,   37,    0,    0,    0,    0,    0,    0,   22,  124, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    6,    0,  103, & ! 7   
     &      0,    0,    0,    6,  146,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &     29,   29,   12,   29,   29,   13,    0,    0,   29,   30, & ! O   
     &     17,  133,   29,   12,    5,  152,   14,   14,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    4,    0,    0,  134,  134,   13,  133,   13,  133, & ! 3   
     &     24,   24,    0,    0,    5,  133,    0,    0,    0,   30, & ! 4   
     &      8,    0,    0,    0,    0,    0,    5,    5,    5,  135, & ! 5   
     &      0,    0,    0,    0,   25,   25,   25,  135,   16,   13, & ! 6   
     &    133,  149,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  165, & ! 8   
     &      0,    0,    5,   13,  159,    0,    0,   25,   13,    0, & ! 9   
     &      0,  176,    0,  169,  158,  158,   30,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,  267,  267, & ! 2   
     &    267,  267,  269,  271,  271,  271,  271,  271/     !  3   

      DATA ( IRR( IRXXN,  9 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,   22,    0,    0,    0,   12,    0, & ! 1   
     &      0,    0,    0,    0,   30,   30,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &     30,   37,   37,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,   13,    0,   49,   20,   86,    4, & ! 2   
     &     14,   14,   59,   15,   14,   13,   13,   59,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,   10,    0,    0,   68, & ! 5   
     &      0,   33,    0,   33,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   15,   13,   22,    0,   19,   22,  126, & ! 7   
     &      0,    0,  126,    0,    0,  126,   33,   13,   22,    6, & ! 8   
     &      0,    0,    0,    0,   30,    0,   30,  136,   29,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,   13,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   13,    0,   12,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   17, & ! 4   
     &     17,   22,    0,   97,   97,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,   33,   97,   97,   97,    0,    0,  103,    0, & ! 6   
     &      0,   13,   95,  135,   29,    0,    0,    0,    0,    0, & ! 7   
     &      0,   95,  126,    0,    0,   17,   17,   22,    0,  103, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,   33,  103, & ! 9   
     &    111,  103,    0,    0,    0,    0,    0,    0,  103,  135, & ! O   
     &     22,    0,    0,    0,    0,    0,    0,  103,  126,    0, & ! 1   
     &      0,    0,    0,   15,   13,   22,    0,   15,   15,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,   33,  141,    0,  111, & ! 3   
     &      0,    0,    0,    0,    0,    0,    6,   20,   22,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,  124,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,   95,    0,    6, & ! 7   
     &      0,    0,    0,  144,    6,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &    133,  133,   95,  133,  133,   95,    0,    0,   12,  133, & ! O   
     &     14,   29,  133,    0,   12,   17,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,   29,    0,    0,  133,  158,  133,  135,   12,  159, & ! 3   
     &    158,  158,    0,    0,   12,    4,    0,    0,    0,  133, & ! 4   
     &      0,    0,    0,    0,    0,    0,   13,   12,   12,   29, & ! 5   
     &      0,    0,    0,    0,   13,   13,   13,  133,    0,   12, & ! 6   
     &    159,  135,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  166, & ! 8   
     &      0,    0,    0,  159,  158,    0,    0,    0,  174,    0, & ! 9   
     &      0,    0,    0,    0,  135,   29,  133,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,  268,  269, & ! 2   
     &    269,  269,  270,  272,  272,  272,  272,  272/     !  3   

      DATA ( IRR( IRXXN, 10 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    4,   14,   32,   14, & ! 2   
     &     17,   28,   12,   13,   13,   29,   29,  103,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   62, & ! 5   
     &      0,   68,    0,   68,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   17,   15,   39,    0,   23,   24,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,   90,  129,   59,   55, & ! 8   
     &      0,    0,    0,    0,    0,    0,  134,    6,   22,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,   25,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   20,    0,   30,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   22, & ! 4   
     &     22,   95,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,   90,  141,  111,  111,    0,    0,    0,    0, & ! 6   
     &      0,    0,   30,   20,   30,    0,    0,    0,    0,    0, & ! 7   
     &      0,   97,    0,    0,    0,   22,   22,  103,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,   90,  141, & ! 9   
     &      0,  111,    0,    0,    0,    0,    0,    0,   16,   20, & ! O   
     &     29,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,   17,   17,    0,    0,   19,   24,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,   90,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,   29,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,   97,    0,    0, & ! 7   
     &      0,    0,    0,  146,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,   12,   29,  103,    0,  135,    0,    0,    6,   29, & ! O   
     &      0,   12,   12,    0,    0,   14,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,   86,    0,    0,  135,  159,  135,    6,    6,   59, & ! 3   
     &    135,  135,    0,    0,   14,  134,    0,    0,    0,    6, & ! 4   
     &      0,    0,    0,    0,    0,    0,   59,   13,   13,   13, & ! 5   
     &      0,    0,    0,    0,   12,   12,   12,    0,    0,    0, & ! 6   
     &      6,   30,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,  170,  135,    0,    0,    0,  175,    0, & ! 9   
     &      0,    0,    0,    0,  133,  170,    6,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,  269,  270, & ! 2   
     &    270,  270,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 11 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   14,    4,    4,   13, & ! 2   
     &     22,   13,    4,    5,   15,   30,   30,   38,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,   62,    0,   62,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   19,   17,    0,    0,  124,  124,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,   39,  130,  131,   22, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,   30,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,   27,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   14,    0,   13,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   29, & ! 4   
     &     95,   97,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   12,   95,    0,    0,    0,    0,    0, & ! 7   
     &      0,  124,    0,    0,    0,   29,  103,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,   12,   16, & ! O   
     &     30,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,   23,   22,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,   30,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,  144,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,  133,  149,    0,    4,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,   20,    0,    0,   13,  133,    6,   16,    0,   24, & ! 3   
     &     13,   13,    0,    0,   13,  160,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,   14,   59,   59,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &    136,  133,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,  170,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,  167,   13,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,  270,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 12 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   45,   45,   14,   25, & ! 2   
     &    111,   25,    0,   30,   22,   37,   31,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   23,   23,    0,    0,    0,  125,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,   12,  132,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,  141,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   95, & ! 4   
     &     97,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,   97,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,  103,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &    103,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,   19,   23,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,  146,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,  133,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,  135,  160,   12,    0,  158, & ! 3   
     &     12,   12,    0,    0,    6,   12,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,   14,   14,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &     13,   29,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,   13,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,  159,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,  271,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 13 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   46,   46,   28,   27, & ! 2   
     &      0,   59,    0,  112,   29,    0,   37,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,  124,   19,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   97, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,   19,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,   29,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   59,    0,  103,    0,  135, & ! 3   
     &      8,    8,    0,    0,  159,   59,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    6,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,   12,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,   22,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,  272,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 14 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   48,   13,   13,   59, & ! 2   
     &      0,  103,    0,    0,   30,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   39,   22,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,   15,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,   20,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   13,    0,    0,    0,    8, & ! 3   
     &     16,    6,    0,    0,  160,  136,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,  170,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 15 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   60,   15,   25,   28, & ! 2   
     &      0,  111,    0,    0,   59,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,   24,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,   24,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    6, & ! 3   
     &      6,    0,    0,    0,   30,   30,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 16 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   13,   17,   59,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,  124,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,  133,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 17 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   15,   19,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,   39,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,   29,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 18 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   17,   22,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 19 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   19,   24,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 20 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   34,  103,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 21 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   23,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 22 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   24,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 23 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   59,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 24 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,  103,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( RTDAT( 1,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.7000D-12, 1.0000D-14, & ! 3   
     &     1.4000D-12, 1.4000D-13, 5.7400D-34, 8.0000D-12, 3.2000D-11, & ! +   
     &     2.0000D-11, 2.1400D-10, 7.7000D-12, 4.8000D-11, 2.2000D-13, & ! 4   
     &     3.0800D-34, 2.9000D-12, 9.0000D-32, 7.0000D-31, 3.4500D-12, & ! +   
     &     6.0950D-14, 3.3000D-39, 2.5000D-12, 5.5000D-12, 2.5000D-31, & ! 5   
     &     1.5100D-30, 2.4000D-14, 2.0000D-11, 4.0000D-12, 1.8000D-11, & ! +   
     &     4.5000D-14, 8.5000D-13, 2.0000D-30, 2.7000D-27, 1.0000D-22, & ! 6   
     &     2.0000D-31, 2.1000D-27, 1.3000D-12, 3.3000D-31, 1.4400D-13, & ! +   
     &     1.8500D-12, 6.9000D-12, 7.6800D-12, 1.0100D-11, 2.8200D-11, & ! 7   
     &     1.0000D-28, 5.7200D-12, 1.3300D-11, 1.4800D-11, 5.5000D-30, & ! +   
     &     2.3300D-12, 1.8100D-12, 2.3100D-11, 1.4300D-11, 1.3600D-11, & ! 8   
     &     2.7000D-11, 1.2100D-11, 4.2000D-11, 5.5000D-12, 4.7000D-12, & ! +   
     &     4.9000D-12, 4.5600D-14, 1.5000D-12, 2.8000D-12, 3.0000D-12, & ! 9   
     &     8.0000D-12, 2.6000D-12, 5.7700D-12, 1.1000D-11, 9.2600D-13, & ! +   
     &     2.8000D-11, 2.8000D-11, 1.0000D-11, 5.3200D-12, 6.7500D-12, & ! O   
     &     4.6500D-11, 2.8000D-11, 2.0500D-10, 2.8500D-12, 3.0000D-12, & ! +   
     &     2.6000D-12, 1.4700D-11, 2.9000D-12, 3.4000D-12, 1.7000D-11, & ! 1   
     &     3.0000D-11, 4.5000D-13, 4.0000D-14, 2.9300D-12, 4.0000D-14, & ! +   
     &     4.0000D-14, 3.2000D-11, 5.3100D-12, 1.0000D+00, 2.0000D-12, & ! 2   
     &     9.1400D-15, 4.3300D-15, 4.4000D-15, 1.3400D-14, 7.8600D-15, & ! +   
     &     5.0000D-16, 2.9500D-15, 1.4000D-15, 8.5000D-16, 1.6600D-18, & ! 3   
     &     2.0000D-16, 2.0000D-16, 9.0000D-17, 5.0000D-16, 2.8600D-13, & ! +   
     &     4.3920D-13, 1.7900D-13, 8.6400D-13, 1.0000D-13, 3.0300D-12, & ! 4   
     &     4.0000D-12, 1.2200D-11, 2.0000D-12, 1.4000D-12, 3.7600D-12, & ! +   
     &     3.4000D-15, 5.0200D-13, 2.9000D-12, 3.7600D-12, 3.7800D-12, & ! 5   
     &     1.0600D-12, 2.8700D-13, 2.0100D-10, 2.2000D-14, 1.0000D+03, & ! +   
     &     1.0000D+03, 1.0000D+03, 1.0000D+03, 1.0000D+03, 1.0000D+03, & ! 6   
     &     9.7000D-29, 9.0000D-29, 9.7000D-29, 9.0000D-29, 2.8000D-12, & ! +   
     &     1.6000D+16, 2.8000D-12, 2.6000D-12, 4.0000D-12, 4.0000D-12, & ! 7   
     &     4.0000D-12, 9.0000D-12, 4.0000D-12, 4.0000D-12, 2.5400D-12, & ! +   
     &     4.0000D-12, 2.7000D-12, 2.7000D-12, 4.0000D-12, 2.7000D-12, & ! 8   
     &     2.7000D-12, 2.7000D-12, 2.4300D-12, 4.0000D-12, 4.0000D-12, & ! +   
     &     8.1000D-12, 8.1000D-12, 2.9000D-12, 4.0000D-12, 4.0000D-12, & ! 9   
     &     7.5000D-12, 2.7000D-12, 2.7000D-12, 2.5400D-12, 4.0000D-12, & ! +   
     &     4.0000D-12, 2.7000D-12, 2.7000D-12, 4.0000D-12, 4.0000D-12, & ! O   
     &     4.0000D-12, 2.7000D-12, 4.0000D-12, 2.0000D-11, 2.0000D-11, & ! +   
     &     2.0800D-12, 4.1000D-13, 7.5000D-13, 1.6600D-13, 1.6600D-13, & ! 1   
     &     1.6600D-13, 1.9000D-13, 1.6600D-13, 1.6600D-13, 2.9100D-13, & ! +   
     &     3.7500D-13, 3.7500D-13, 3.7500D-13, 3.7500D-13, 3.7500D-13, & ! 2   
     &     3.7500D-13, 3.7500D-13, 2.0600D-13, 1.5000D-11, 1.5000D-11, & ! +   
     &     4.3000D-13, 4.3000D-13, 1.1500D-13, 1.1500D-13, 1.1500D-13, & ! 3   
     &     5.2000D-13, 1.8000D-13, 1.8200D-13, 2.9100D-13, 3.7500D-13, & ! +   
     &     1.0000D-11, 3.7500D-13, 1.1500D-13, 1.6600D-13, 1.6600D-13, & ! 4   
     &     3.7500D-13, 1.6600D-13, 9.5000D-14, 1.1800D-13, 9.4600D-14, & ! +   
     &     1.0000D-13, 4.3400D-14, 1.7100D-13, 1.4600D-13, 9.1800D-14, & ! 5   
     &     3.5600D-14, 3.5600D-14, 3.5600D-14, 3.5600D-14, 3.5600D-14, & ! +   
     &     3.5600D-14, 3.5600D-14, 3.5600D-14, 3.4000D-14, 3.5600D-14, & ! 6   
     &     3.5600D-14, 2.0000D-11, 2.0000D-11, 7.5000D-13, 6.9100D-13, & ! +   
     &     6.9100D-13, 2.0000D-12, 2.0400D-13, 1.3800D-12, 3.4000D-14, & ! 7   
     &     3.5600D-14, 3.5600D-14, 3.5600D-14, 3.5600D-14, 7.5000D-13, & ! +   
     &     1.6000D-13, 9.6800D-14, 3.5600D-14, 5.9900D-15, 1.0300D-12, & ! 8   
     &     6.9000D-13, 5.5900D-13, 2.4700D-13, 9.4800D-13, 8.1100D-13, & ! +   
     &     5.0900D-13, 7.4000D-13, 7.4000D-13, 7.4000D-13, 7.4000D-13, & ! 9   
     &     7.4000D-13, 7.4000D-13, 7.4000D-13, 7.4000D-13, 8.4000D-14, & ! +   
     &     7.4000D-13, 7.4000D-13, 2.5000D-12, 2.5000D-12, 7.5100D-13, & ! O   
     &     7.5100D-13, 7.5100D-13, 2.9000D-12, 2.5800D-13, 1.7000D-12, & ! +   
     &     1.6800D-12, 7.4000D-13, 7.4000D-13, 7.4000D-13, 7.4000D-13, & ! 1   
     &     7.5100D-13, 8.8500D-13, 5.3700D-13, 7.4000D-13, 3.4000D-14, & ! +   
     &     2.5000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, & ! 2   
     &     1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, & ! +   
     &     1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, & ! 3   
     &     1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, & ! +   
     &     4.0000D-12, 4.0000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, & ! 4   
     &     4.0000D-12, 2.3000D-12, 2.3000D-12, 2.5000D-12, 2.5000D-12, & ! +   
     &     2.5000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, & ! 5   
     &     1.2000D-12, 1.2000D-12, 7.0000D-14, 4.2500D-14, 2.9600D-14, & ! +   
     &     1.2000D-12, 2.5000D-12, 7.1300D-17, 6.1300D-12, 4.1400D-12, & ! 6   
     &     5.1100D-12, 2.9700D-11, 2.9200D-11, 2.9000D+07, 1.7900D-12, & ! +   
     &     7.3000D-11, 4.1000D-11, 4.0000D-12, 4.0000D-12, 1.6600D-13, & ! 7   
     &     1.6600D-13, 1.6000D-13, 9.6800D-14, 8.8500D-13, 5.3700D-13, & ! +   
     &     1.2000D-12, 1.2000D-12, 7.0000D-14, 4.2500D-14, 2.9600D-14, & ! 8   
     &     4.8000D-12, 7.2900D-11, 1.6700D-16, 4.8000D-12, 7.2900D-11, & ! +   
     &     1.6700D-16, 3.1500D-13, 3.1500D-13, 3.1500D-13, 3.1500D-13, & ! 9   
     &     3.1500D-13, 9.3000D-12, 1.6000D-16, 2.4000D-14, 1.6000D-12, & ! +   
     &     2.7000D-12, 2.0600D-13, 4.6000D-13, 5.8000D-13, 2.3000D-12, & ! O   
     &     4.0000D-17, 5.9500D-12, 1.1000D-11, 2.7000D-12, 8.2400D-14, & ! +   
     &     8.8300D-13, 1.0400D-13, 2.3000D-12, 7.5000D-12, 2.0600D-13, & ! 1   
     &     5.2000D-13, 2.9000D-12, 4.0000D-12, 8.0000D-13, 3.0000D-12, & ! +   
     &     8.0000D-12, 2.0600D-13, 3.5500D-12, 4.2300D-12, 2.3000D-12, & ! 2   
     &     1.0000D-11, 2.7000D-11, 1.0000D-11, 1.4000D-12, 4.1200D+08, & ! +   
     &     4.6000D-11, 2.0000D-17, 2.4000D-12, 1.2000D-11, 2.4000D-12, & ! 3   
     &     2.4000D-12, 6.7000D-13, 8.1600D-13, 2.3000D-12, 7.5000D-14, & ! +   
     &     9.5300D-14, 2.3000D-12, 2.0600D-13, 2.0600D-13, 2.9000D-17, & ! 4   
     &     3.7000D-19, 1.6700D-11, 1.0000D-12, 1.0000D-11, 5.0000D-11, & ! +   
     &     5.6000D-12, 7.2000D-12, 3.8000D-12, 3.7300D-11, 5.7900D-11, & ! 5   
     &     3.2000D-11, 7.4000D-13, 2.7000D-12, 2.3000D-12, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 3.3300D-01, 1.0000D+00, & ! 7   
     &     3.3300D-01, 3.3300D-01, 3.3300D-01, 3.3300D-01, 3.3300D-01, & ! +   
     &     3.3300D-01, 3.3300D-01, 3.3300D-01, 3.3300D-01, 3.3300D-01, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 5.2000D-12, & ! +   
     &     6.3000D-12, 4.0000D-12, 4.1000D-11, 2.0400D-13, 2.0400D-13, & ! 9   
     &     1.1000D-10, 4.2000D-11, 2.5000D-14, 2.0600D-13, 6.9000D-12, & ! +   
     &     6.9000D-12, 1.1000D-10, 4.2000D-11, 2.3000D-12, 2.3000D-12, & ! O   
     &     8.4000D-12, 1.2500D-11, 1.7000D-11, 1.7000D-11, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     3.3300D-01, 2.0000D+05, 2.3000D-12, 2.5000D-12, 2.0400D-13, & ! +   
     &     1.0000D-11, 2.7000D-12, 1.9000D-13, 2.7000D-12, 1.9000D-13, & ! 2   
     &     2.7000D-12, 1.9000D-13, 1.1600D-14, 1.9700D-10, 1.9000D-11, & ! +   
     &     2.3100D-11, 2.7000D-12, 1.9000D-13, 2.7000D-12, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D-40, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 4.2000D-03, 4.2000D-03, 4.2000D-03, 4.1000D-03, & ! +   
     &     4.1000D-03, 4.1000D-03, 1.1000D-07, 7.5000D-03, 3.4000D-04, & ! 7   
     &     2.2000D-03, 5.3000D-03, 6.7000D-03, 7.1000D-03, 1.3000D-03, & ! +   
     &     1.7000D-03, 4.1000D-04, 1.6000D-05, 1.1000D-07, 1.1000D-07, & ! 8   
     &     1.1000D-07, 1.1000D-07, 1.4000D-04, 1.1000D-07, 4.2000D-03, & ! +   
     &     4.2000D-03, 4.2000D-03, 4.1000D-03, 4.1000D-03, 4.1000D-03, & ! 9   
     &     1.1000D-07, 7.5000D-03, 3.4000D-04, 2.2000D-03, 5.3000D-03, & ! +   
     &     6.7000D-03, 7.1000D-03, 1.3000D-03, 1.7000D-03, 4.1000D-04, & ! O   
     &     1.6000D-05, 1.1000D-07, 1.1000D-07, 1.1000D-07, 1.1000D-07, & ! +   
     &     1.4000D-04, 1.1000D-07, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! 1   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! +   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! 2   
     &     9.4882D-06, 9.4882D-06, 1.2500D-11, 4.0000D-11, 4.0000D-11, & ! +   
     &     4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, & ! 3   
     &     4.0000D-11, 4.0000D-11, 4.0000D-11/           !        +   

      DATA ( RTDAT( 2,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00,-2.6000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 6.0000D+02, & ! 4   
     &     2.8000D+03, 0.0000D+00,-1.5000D+00,-2.6000D+00, 0.0000D+00, & ! +   
     &     2.7000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.8000D+00, & ! 5   
     &    -3.0000D+00, 4.6000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00,-4.4000D+00, 1.1000D+04, 0.0000D+00, & ! 6   
     &    -3.4000D+00, 1.0900D+04, 0.0000D+00,-4.3000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &    -4.5000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.6500D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &    -5.6000D+00, 1.4000D+04,-5.6000D+00, 1.4000D+04, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 4.6000D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 4.0582D-09, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00/           !        +   

      DATA ( RTDAT( 3,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-9.4000D+02,-4.9000D+02, & ! 3   
     &    -1.3100D+03,-2.4700D+03, 0.0000D+00,-2.0600D+03,-6.7000D+01, & ! +   
     &     1.3000D+02, 0.0000D+00,-2.1000D+03, 2.5000D+02, 1.9000D-33, & ! 4   
     &     2.5900D-54,-1.6000D+02, 0.0000D+00, 0.0000D+00, 2.7000D+02, & ! +   
     &     6.8570D-34, 5.3000D+02, 2.6000D+02, 1.8800D+02, 0.0000D+00, & ! 5   
     &     0.0000D+00, 2.7000D-17, 0.0000D+00, 0.0000D+00, 1.1000D+02, & ! +   
     &    -1.2600D+03,-2.4500D+03, 0.0000D+00, 6.3000D+01, 0.0000D+00, & ! 6   
     &     0.0000D+00, 6.6000D+01, 3.8000D+02, 0.0000D+00, 2.8800D-33, & ! +   
     &    -1.6900D+03,-1.0000D+03,-3.7000D+02,-2.4500D+02,-2.7300D+02, & ! 7   
     &     0.0000D+00, 5.0000D+02, 5.0000D+02, 4.4800D+02, 0.0000D+00, & ! +   
     &    -1.9300D+02, 3.5400D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     3.9000D+02, 4.4000D+02, 4.0100D+02, 1.2500D+02, 3.4500D+02, & ! +   
     &     4.0500D+02,-4.2700D+02,-9.0000D+01, 1.0000D+01, 0.0000D+00, & ! 9   
     &     3.8000D+02, 6.1000D+02, 5.3300D+02, 0.0000D+00, 8.3000D+02, & ! +   
     &     1.7500D+02, 1.7500D+02, 0.0000D+00, 2.4300D+02, 4.0500D+02, & ! O   
     &     0.0000D+00, 1.7500D+02, 0.0000D+00,-3.4500D+02, 2.0000D+01, & ! +   
     &     2.0000D+02, 0.0000D+00, 1.9000D+02, 1.9000D+02, 3.9000D+02, & ! 1   
     &     0.0000D+00, 0.0000D+00, 8.5000D+02, 1.9000D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00,-2.6000D+02, 0.0000D+00, 0.0000D+00, & ! 2   
     &    -2.5800D+03,-1.8000D+03,-8.4500D+02,-2.2830D+03,-1.9130D+03, & ! +   
     &    -5.3000D+02,-7.8300D+02,-2.1000D+03,-1.5200D+03, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -2.2820D+03,-4.5000D+02, 4.5000D+02, 0.0000D+00,-4.4600D+02, & ! 4   
     &     4.9000D+02, 0.0000D+00,-2.4400D+03,-1.9000D+03,-1.9000D+03, & ! +   
     &     0.0000D+00,-1.0760D+03,-1.9000D+03,-1.9000D+03, 0.0000D+00, & ! 5   
     &     0.0000D+00,-1.0000D+03, 0.0000D+00,-5.0000D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 1.6600D+02, 0.0000D+00, 1.6800D+02, 1.8100D+02, & ! +   
     &    -1.3486D+04, 3.0000D+02, 3.6500D+02, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.6000D+02, & ! +   
     &     0.0000D+00, 3.6000D+02, 3.6000D+02, 0.0000D+00, 3.6000D+02, & ! 8   
     &     3.6000D+02, 3.6000D+02, 3.6000D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.7000D+02, 2.7000D+02, 3.0000D+02, 0.0000D+00, 0.0000D+00, & ! 9   
     &     2.9000D+02, 3.6000D+02, 3.5000D+02, 3.6000D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.6000D+02, 3.6000D+02, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 3.6000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 7.5000D+02, 7.0000D+02, 1.3000D+03, 1.3000D+03, & ! 1   
     &     1.3000D+03, 1.3000D+03, 1.3000D+03, 1.3000D+03, 1.3000D+03, & ! +   
     &     9.8000D+02, 9.8000D+02, 9.8000D+02, 9.8000D+02, 9.8000D+02, & ! 2   
     &     9.8000D+02, 9.8000D+02, 1.3000D+03, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0400D+03, 1.0400D+03, 1.3000D+03, 1.3000D+03, 1.3000D+03, & ! 3   
     &     9.8000D+02, 1.3000D+03, 1.3000D+03, 1.3000D+03, 9.8000D+02, & ! +   
     &     0.0000D+00, 9.8000D+02, 1.3000D+03, 1.3000D+03, 1.3000D+03, & ! 4   
     &     9.8000D+02, 1.3000D+03, 3.9000D+02, 1.5800D+02, 4.3100D+02, & ! +   
     &     4.6700D+02, 6.3300D+02, 7.0800D+02, 7.0800D+02, 7.0800D+02, & ! 5   
     &     7.0800D+02, 7.0800D+02, 7.0800D+02, 7.0800D+02, 7.0800D+02, & ! +   
     &     7.0800D+02, 7.0800D+02, 7.0800D+02, 2.2100D+02, 7.0800D+02, & ! 6   
     &     7.0800D+02, 5.0000D+02, 5.0000D+02, 5.0000D+02, 5.0800D+02, & ! +   
     &     5.0800D+02, 5.0000D+02, 0.0000D+00, 0.0000D+00, 2.2100D+02, & ! 7   
     &     7.0800D+02, 7.0800D+02, 7.0800D+02, 7.0800D+02, 5.0000D+02, & ! +   
     &     7.0800D+02, 7.0800D+02, 0.0000D+00, 1.5100D+03, 2.1100D+02, & ! 8   
     &     4.6000D+02, 5.2200D+02, 6.8300D+02, 7.6500D+02, 7.6500D+02, & ! +   
     &     7.6500D+02, 7.6500D+02, 7.6500D+02, 7.6500D+02, 7.6500D+02, & ! 9   
     &     7.6500D+02, 7.6500D+02, 7.6500D+02, 7.6500D+02, 2.2100D+02, & ! +   
     &     7.6500D+02, 7.6500D+02, 5.0000D+02, 5.0000D+02, 5.6500D+02, & ! O   
     &     5.6500D+02, 5.6500D+02, 5.0000D+02, 5.0000D+02, 5.0000D+02, & ! +   
     &     5.0000D+02, 7.6500D+02, 7.6500D+02, 7.0800D+02, 7.0800D+02, & ! 1   
     &     5.6500D+02, 7.6500D+02, 7.6500D+02, 7.0800D+02, 1.5600D+03, & ! +   
     &     5.0000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 1.0000D+03, 1.0000D+03, 1.0000D+03, & ! +   
     &     0.0000D+00, 5.0000D+02, 2.9500D+03, 2.0000D+02, 2.0000D+02, & ! 6   
     &     2.0000D+02, 3.9000D+02, 3.9000D+02,-5.2970D+03, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.3000D+03, & ! 7   
     &     1.3000D+03, 7.0800D+02, 7.0800D+02, 7.6500D+02, 7.6500D+02, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+03, 1.0000D+03, 1.0000D+03, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-4.4800D+02,-4.4800D+02,-4.4800D+02,-4.4800D+02, & ! 9   
     &    -4.4800D+02, 0.0000D+00, 0.0000D+00, 2.7000D-17,-3.0500D+02, & ! +   
     &     3.6000D+02, 1.3000D+03, 0.0000D+00, 5.0000D+02, 0.0000D+00, & ! O   
     &     0.0000D+00,-1.8600D+03, 0.0000D+00, 3.6000D+02, 0.0000D+00, & ! +   
     &     5.0000D+02, 5.0000D+02, 0.0000D+00, 2.9000D+02, 1.3000D+03, & ! 1   
     &     9.8000D+02, 5.0000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.3000D+03, 0.0000D+00, 5.0000D+02, 0.0000D+00, & ! 2   
     &     0.0000D+00, 2.0000D+02, 0.0000D+00,-1.8600D+03,-7.7000D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 7.4500D+02, 6.5200D+02, 3.6000D+02, & ! 3   
     &     3.6000D+02, 0.0000D+00, 5.0000D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     5.0000D+02, 0.0000D+00, 1.3000D+03, 1.3000D+03, 0.0000D+00, & ! 4   
     &     0.0000D+00, 2.0000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.0000D+02,-4.0000D+02,-4.0000D+02, & ! 5   
     &    -4.0000D+02, 7.0000D+02, 3.6000D+02, 7.0000D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -1.8600D+03, 0.0000D+00, 0.0000D+00, 1.3000D+03, 1.3000D+03, & ! 9   
     &     0.0000D+00, 0.0000D+00,-3.0000D+02, 1.3000D+03, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-3.0000D+02, 1.3000D+03, & ! +   
     &     0.0000D+00, 3.6000D+02, 1.3000D+03, 3.6000D+02, 1.3000D+03, & ! 2   
     &     3.6000D+02, 1.3000D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.6000D+02, 1.3000D+03, 3.7400D+02, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00/           !        +   
      INTEGER            :: IRRFALL( NFALLOFF )

      DATA ( IRRFALL( IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     45,   46,   48,   49,   51,   55,   56,   57,   63,   66, & 
     &     69,   70,   76,   80,  166,  168,  399,  537/

      DATA ( RFDAT( 1,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     9.8000D+02, 3.1800D+03, 3.0000D-11, 3.6000D-11, 2.7000D+02, & 
     &     2.2000D-11, 2.5800D-11, 2.1990D+03, 1.4000D-12, 2.9000D-12, & 
     &     1.6000D-12, 0.0000D+00, 8.8000D-12, 8.3000D-13, 9.3000D-12, & 
     &     9.3000D-12, 2.1990D+03, 7.8426D+01/

      DATA ( RFDAT( 2,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.0000D-01,-1.0000D+00, & 
     &    -7.0000D-01, 0.0000D+00, 6.5000D-34,-7.0000D-01,-1.1000D+00, & 
     &     0.0000D+00, 0.0000D+00,-8.5000D-01, 2.0000D+00,-1.5000D+00, & 
     &    -1.5000D+00, 6.5000D-34, 5.8212D+00/

      DATA ( RFDAT( 3,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 1.3350D+03, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 1.3350D+03, 0.0000D+00/

      DATA ( RFDAT( 4,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 0.0000D+00, 6.0000D-01, 6.0000D-01,-5.9680D-14, & 
     &     6.0000D-01, 6.0000D-01, 0.0000D+00, 6.0000D-01, 6.0000D-01, & 
     &     6.0000D-01, 0.0000D+00, 6.0000D-01, 6.0000D-01, 6.0000D-01, & 
     &     6.0000D-01, 0.0000D+00, 0.0000D+00/

      DATA ( RFDAT( 5,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 2.7000D+02, & 
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     1.0000D+00, 0.0000D+00, 0.0000D+00/

      REAL               :: SC( NRXNS,MXPRD )

      DATA ( SC( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &        1.00000,    1.00000,    2.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    0.20000,    1.00000, & ! +   
     &        2.00000,    1.00000,    1.00000,    1.00000,    1.22000, & ! 1   
     &        0.50000,    1.00000,    1.00000,    0.50000,    0.60000, & ! +   
     &        2.00000,    1.00000,    2.00000,    1.00000,    1.50000, & ! 2   
     &        1.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        1.00000,    2.00000,    1.00000,    0.00000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    2.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    0.70000,    2.00000, & ! +   
     &        1.00000,    2.00000,    1.00000,    1.00000,    2.00000, & ! 6   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.04900, & ! 7   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.65000, & ! +   
     &        0.64800,    0.17700,    0.17700,    0.17700,    0.17700, & ! 8   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        0.53000,    1.00000,    0.31300,    1.00000,    1.00000, & ! +   
     &        0.52000,    0.52000,    0.56000,    1.00000,    0.73000, & ! O   
     &        0.73000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.35000,    0.01000,    0.57800, & ! 1   
     &        1.00000,    1.00000,    0.64000,    0.35000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.54000, & ! 2   
     &        0.08000,    0.22000,    0.46000,    0.09000,    0.25000, & ! +   
     &        0.85000,    0.85000,    0.90200,    0.28000,    0.10000, & ! 3   
     &        0.05000,    0.05000,    0.05000,    0.05000,    1.00000, & ! +   
     &        0.80000,    0.43000,    0.11000,    0.90000,    1.00000, & ! 4   
     &        0.10000,    0.71000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.40000, & ! 5   
     &        0.40000,    0.50000,    1.00000,    1.00000,    0.28000, & ! +   
     &        0.49000,    0.15800,    0.39000,    0.15800,    0.39000, & ! 6   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.66000,    0.20000, & ! 7   
     &        0.60600,    1.00000,    0.78000,    0.83000,    0.91800, & ! +   
     &        1.00000,    0.95000,    0.50000,    1.00000,    0.95000, & ! 8   
     &        0.95000,    0.95000,    0.40000,    0.82000,    0.22000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.67000,    0.77000, & ! 9   
     &        1.00000,    0.85000,    0.71600,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        2.00000,    2.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    0.62800,    1.00000,    1.00000, & ! +   
     &        0.44000,    0.44000,    0.15000,    1.00000,    1.00000, & ! 3   
     &        0.40000,    1.00000,    0.38000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    0.74000,    1.00000,    0.89400, & ! +   
     &        0.84200,    0.91000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.60000,    1.00000,    2.00000,    2.00000,    1.00000, & ! +   
     &        2.00000,    2.00000,    2.00000,    1.54000,    1.00000, & ! 6   
     &        1.00000,    0.90000,    0.90000,    0.50000,    0.83400, & ! +   
     &        1.00000,    0.90000,    0.32800,    0.25000,    1.00000, & ! 7   
     &        1.00000,    1.00000,    2.00000,    1.00000,    1.00000, & ! +   
     &        2.00000,    0.50000,    1.00000,    1.00000,    0.50000, & ! 8   
     &        0.39400,    0.34200,    0.30300,    0.50000,    0.50000, & ! +   
     &        0.50000,    0.60000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.50000, & ! +   
     &        0.50000,    0.50000,    2.00000,    1.00000,    0.50000, & ! O   
     &        0.33000,    0.50000,    1.65000,    0.85500,    0.10000, & ! +   
     &        0.50000,    1.00000,    1.00000,    2.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    0.50000,    1.00000,    1.00000, & ! +   
     &        2.00000,    1.00000,    1.00000,    0.25400,    0.48800, & ! 2   
     &        0.82000,    1.00000,    0.47000,    0.86000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.50000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    0.43800,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.67000,    1.00000, & ! 4   
     &        1.00000,    0.85500,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        2.00000,    2.00000,    1.00000,    0.50000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.00000,    0.75000,    0.48000, & ! 6   
     &        0.25000,    0.68000,    0.50000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.28700,    1.00000, & ! 7   
     &        1.00000,    1.00000,    0.96500,    0.70000,    0.28700, & ! +   
     &        0.70000,    0.28700,    1.40000,    0.20200,    0.50400, & ! 8   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.33000,    0.39600,    0.52000,    1.00000, & ! O   
     &        0.50300,    1.00000,    0.74000,    0.39000,    0.57300, & ! +   
     &        0.10000,    1.00000,    1.00000,    1.00000,    0.65200, & ! 1   
     &        0.40000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.70000,    1.00000,    1.00000, & ! 2   
     &        1.50000,    0.16500,    0.20000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.55500,    0.88000,    1.00000,    0.88000, & ! 3   
     &        0.84000,    0.12800,    0.06400,    0.25600,    1.50600, & ! +   
     &        0.07800,    0.07800,    1.00000,    1.00000,    0.26600, & ! 4   
     &        0.54100,    0.46000,    1.00000,    1.00000,    0.08000, & ! +   
     &        0.65000,    1.00000,    0.70000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    0.72500,    0.72500,    0.72500,    1.00000, & ! +   
     &        1.00000,    2.00000,    1.00000,    1.00000,    1.00000, & ! 6   
     &        1.00000,    1.00000,    0.50000,    0.53000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 7   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.39000, & ! +   
     &        0.10000,    1.00000,    0.52000,    0.27000,    0.27000, & ! 9   
     &        0.92000,    1.00000,    0.88000,    0.22000,    1.00000, & ! +   
     &        1.00000,    0.37000,    0.78000,    0.27000,    0.27000, & ! O   
     &        0.27000,    0.08000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.46000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    2.00000, & ! 3   
     &        0.50000,    0.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 6   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    2.00000, & ! 7   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    2.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.14280,    1.14280,    1.00000, & ! 1   
     &        1.00000,    0.85714,    0.85714,    1.00000,    1.00000, & ! +   
     &        0.50000,    0.50000,    1.50000,    1.42860,    1.42860, & ! 2   
     &        1.71430,    1.71430,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    1.00000/           !         &  

      DATA ( SC( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    0.80000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.78400, & ! 1   
     &        0.50000,    1.00000,    1.00000,    0.17500,    0.60000, & ! +   
     &        0.00000,    1.00000,    2.00000,    1.00000,    0.25000, & ! 2   
     &        0.25000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    1.00000,    0.70000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 6   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.95100, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.35000, & ! +   
     &        0.35200,    0.76300,    0.76300,    0.76300,    0.76300, & ! 8   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 9   
     &        0.47000,    0.00000,    0.68700,    2.00000,    1.00000, & ! +   
     &        0.33000,    0.33000,    0.21000,    0.00000,    0.20000, & ! O   
     &        0.20000,    1.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.71900,    1.00000,    0.65000,    0.44000,    0.27200, & ! 1   
     &        0.00000,    0.00000,    0.36000,    0.65000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.42000, & ! 2   
     &        0.15000,    0.32000,    0.07000,    0.28000,    0.25000, & ! +   
     &        0.10000,    0.10000,    0.24200,    0.56000,    0.07200, & ! 3   
     &        1.00000,    1.00000,    1.00000,    1.50000,    0.00000, & ! +   
     &        0.20000,    0.57000,    0.89000,    0.10000,    0.00000, & ! 4   
     &        0.90000,    0.29000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    2.00000,    1.00000,    0.10000, & ! 5   
     &        0.10000,    1.50000,    1.00000,    1.00000,    0.29000, & ! +   
     &        0.01000,    0.30800,    0.01000,    0.30800,    0.01000, & ! 6   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.13100,    0.05100, & ! 7   
     &        0.13300,    1.00000,    0.97000,    0.95000,    0.91800, & ! +   
     &        1.00000,    0.95000,    0.95000,    1.00000,    0.95000, & ! 8   
     &        0.95000,    0.95000,    0.26000,    0.82000,    0.68600, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.23000, & ! 9   
     &        1.00000,    0.85000,    0.71600,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        0.28700,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.27200,    1.00000,    1.00000, & ! +   
     &        0.44000,    0.44000,    0.15000,    0.00000,    0.00000, & ! 3   
     &        0.20000,    0.00000,    0.37000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    1.37000,    0.75000,    0.08000, & ! +   
     &        0.01800,    0.09000,    1.95000,    1.50000,    0.75000, & ! 5   
     &        0.45900,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.75000, & ! 6   
     &        1.04000,    0.90000,    0.90000,    0.50000,    1.00000, & ! +   
     &        0.75000,    0.58500,    1.03700,    0.90000,    0.30500, & ! 7   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.50000,    0.70000,    1.00000,    0.50000, & ! 8   
     &        0.58000,    0.51800,    0.50000,    0.50000,    0.50000, & ! +   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.50000, & ! +   
     &        0.50000,    0.50000,    0.00000,    1.00000,    0.50000, & ! O   
     &        0.50000,    0.50000,    0.35000,    0.85500,    0.10000, & ! +   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.14000,    0.05500, & ! 2   
     &        0.18000,    1.00000,    0.79000,    0.72000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    0.61000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    0.85500,    0.30000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        0.28700,    1.00000,    2.00000,    0.50000,    0.50400, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.25000,    0.52000, & ! 6   
     &        0.75000,    0.32000,    0.50000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.24000,    0.00000, & ! 7   
     &        0.00000,    2.00000,    0.50000,    0.30000,    1.24000, & ! +   
     &        0.30000,    1.24000,    0.60000,    0.64000,    1.21000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        1.00000,    0.67000,    0.88000,    0.52000,    1.00000, & ! O   
     &        0.89000,    1.00000,    0.26000,    0.32000,    1.00000, & ! +   
     &        1.00000,    0.39000,    0.39000,    0.65000,    0.13600, & ! 1   
     &        0.20000,    1.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.40000,    1.00000,    1.00000, & ! 2   
     &        0.50000,    0.49500,    0.20000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.89000,    0.12000,    0.00000,    0.69000, & ! 3   
     &        0.65000,    0.16000,    0.23000,    0.14400,    0.94900, & ! +   
     &        0.44200,    0.44200,    0.00000,    0.00000,    0.01700, & ! 4   
     &        0.50600,    0.09700,    1.00000,    1.00000,    0.08000, & ! +   
     &        0.65000,    1.00000,    0.30000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.27500,    0.27500,    0.27500,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.20200, & ! 6   
     &        0.20200,    0.20200,    1.24000,    0.48000,    0.70000, & ! +   
     &        0.68600,    0.10700,    1.00000,    1.00000,    1.00000, & ! 7   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.67000, & ! +   
     &        0.10000,    1.00000,    1.00000,    0.73000,    0.73000, & ! 9   
     &        0.08000,    0.00000,    0.88000,    0.01500,    1.00000, & ! +   
     &        1.00000,    0.08000,    0.78000,    0.73000,    0.06000, & ! O   
     &        0.73000,    0.22000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    2.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        1.54000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.47000,    0.00000, & ! 3   
     &        0.50000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.50000,    0.50000,    0.50000,    0.50000, & ! +   
     &        0.50000,    0.50000,    0.40000,    0.40000,    0.50000, & ! 7   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.50000,    0.50000,    0.50000, & ! 8   
     &        0.50000,    0.50000,    0.50000,    0.50000,    0.50000, & ! +   
     &        0.50000,    0.50000,    0.50000,    0.50000,    0.50000, & ! 9   
     &        0.40000,    0.40000,    0.50000,    0.50000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        0.50000,    0.50000,    0.50000,    0.50000,    0.50000, & ! +   
     &        0.50000,    0.50000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    1.00000,    0.48570,    0.30030, & ! +   
     &        0.38560,    0.21810,    0.24120,    0.66640,    0.28580, & ! 3   
     &        0.33030,    0.34440,    0.38860/           !         &  

      DATA ( SC( IRXXN,  3 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.80000,    0.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.00000,    1.22000, & ! 1   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.40000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.20000, & ! 2   
     &        0.20000,    1.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        0.20000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.30000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.02500, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.35000, & ! +   
     &        0.11800,    0.06000,    0.06000,    0.06000,    0.06000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.40000,    0.13000,    0.11000,    0.00000,    0.07000, & ! O   
     &        0.07000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.18400,    0.00000,    0.35000,    0.07000,    0.85000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.35000,    1.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.04000, & ! 2   
     &        0.43000,    0.08000,    0.32000,    0.30000,    0.08000, & ! +   
     &        0.20000,    0.16000,    0.23800,    0.07500,    0.00800, & ! 3   
     &        0.60000,    0.60000,    1.50000,    1.50000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.90000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    1.00000,    0.50000, & ! 5   
     &        0.50000,    1.50000,    0.00000,    0.00000,    0.28000, & ! +   
     &        0.50000,    0.25000,    0.30000,    0.25000,    0.50000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.04800,    0.23100, & ! 7   
     &        0.41600,    1.60000,    0.78000,    0.81000,    0.45900, & ! +   
     &        0.00000,    0.95000,    0.50000,    0.00000,    0.95000, & ! 8   
     &        0.95000,    0.35000,    0.88300,    0.23000,    0.49100, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.33000,    0.16000, & ! 9   
     &        1.00000,    0.42500,    0.24900,    0.61000,    0.00000, & ! +   
     &        0.00000,    0.32000,    0.00000,    1.00000,    1.00000, & ! O   
     &        1.24000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.03700,    0.00000,    0.00000, & ! +   
     &        0.15000,    0.15000,    0.15000,    0.00000,    0.00000, & ! 3   
     &        0.20000,    0.00000,    0.37000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.63000,    0.75000,    0.02600, & ! +   
     &        0.14000,    0.28100,    0.15000,    0.70500,    1.28000, & ! 5   
     &        1.00000,    1.00000,    0.27100,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.36800,    0.30500,    0.75000, & ! 6   
     &        0.19200,    1.00000,    1.00000,    1.50000,    0.33400, & ! +   
     &        0.50000,    0.58500,    1.34400,    0.25000,    0.77300, & ! 7   
     &        1.00000,    1.00000,    0.32000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.96500,    1.00000,    0.00000,    1.00000, & ! 8   
     &        0.02600,    0.14000,    0.06700,    1.60000,    1.00000, & ! +   
     &        1.71000,    0.45900,    0.00000,    1.00000,    1.00000, & ! 9   
     &        0.00000,    1.00000,    1.00000,    0.36800,    1.04800, & ! +   
     &        1.00000,    0.19200,    0.00000,    0.00000,    1.00000, & ! O   
     &        0.33000,    0.50000,    1.00000,    0.13100,    0.27000, & ! +   
     &        0.50000,    0.00000,    0.00000,    0.32000,    1.00000, & ! 1   
     &        0.00000,    1.00000,    0.28700,    0.70000,    0.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.09200,    0.28000, & ! 2   
     &        0.56300,    1.60000,    0.79000,    0.11000,    0.50000, & ! +   
     &        0.00000,    1.00000,    0.50000,    0.00000,    1.00000, & ! 3   
     &        1.00000,    0.36800,    1.00000,    1.00000,    0.38500, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.33000,    1.00000, & ! 4   
     &        0.65000,    1.00000,    0.30000,    0.61000,    0.00000, & ! +   
     &        0.00000,    0.32000,    0.00000,    1.00000,    1.00000, & ! 5   
     &        1.24000,    1.00000,    0.00000,    0.20200,    1.21000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.12500,    0.26000, & ! 6   
     &        0.37500,    1.00000,    0.50000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.46400,    0.00000, & ! 7   
     &        0.00000,    0.70000,    0.93000,    1.00000,    0.46400, & ! +   
     &        1.00000,    0.46400,    1.00000,    0.14900,    0.28500, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.52000,    0.34800,    0.36400,    0.95200,    0.52000, & ! O   
     &        0.89000,    0.00000,    0.00000,    0.32000,    1.40000, & ! +   
     &        1.80000,    0.32000,    0.31700,    0.35000,    0.21200, & ! 1   
     &        0.20000,    0.65000,    0.65000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        0.50000,    0.34000,    0.80000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.89000,    0.12000,    0.00000,    0.21000, & ! 3   
     &        0.23000,    0.10100,    0.14400,    0.60000,    0.30000, & ! +   
     &        0.13300,    0.13300,    0.00000,    0.00000,    0.24900, & ! 4   
     &        0.52600,    0.44300,    0.00000,    0.00000,    0.15000, & ! +   
     &        0.35000,    0.00000,    0.30000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.27500,    0.27500,    0.27500,    0.50000, & ! +   
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.10800, & ! 6   
     &        0.10800,    0.10800,    0.26000,    1.52000,    0.70000, & ! +   
     &        0.29800,    0.05800,    1.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.10000, & ! +   
     &        0.90000,    1.00000,    0.52000,    0.73000,    0.73000, & ! 9   
     &        0.08000,    0.00000,    0.12000,    0.23500,    0.00000, & ! +   
     &        0.00000,    0.08000,    0.22000,    0.73000,    0.73000, & ! O   
     &        0.27000,    0.22000,    1.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    2.00000,    0.50000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00620,    0.28620, & ! +   
     &        0.09500,    0.30630,    0.20890,    0.01430,    0.39310, & ! 3   
     &        0.22720,    0.27490,    0.24210/           !         &  

      DATA ( SC( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.20000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.35000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.32500,    0.40000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 2   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.80000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.02400, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.65000, & ! +   
     &        0.53000,    0.17700,    0.17700,    0.17700,    0.17700, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.78000,    0.10000,    0.27000,    0.00000,    0.73000, & ! O   
     &        0.73000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.08000,    0.15000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.35000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.46000, & ! 2   
     &        1.00000,    0.06000,    0.07000,    0.03000,    0.10000, & ! +   
     &        0.42000,    0.42000,    0.65200,    0.09000,    0.00200, & ! 3   
     &        0.60000,    0.60000,    0.48000,    0.85000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.50000, & ! 5   
     &        0.50000,    1.00000,    0.00000,    0.00000,    0.15000, & ! +   
     &        0.49000,    0.30800,    0.49000,    0.15000,    0.49000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.08900,    0.23500, & ! 7   
     &        0.73900,    0.20000,    0.01200,    0.68000,    0.45900, & ! +   
     &        0.00000,    0.05000,    0.50000,    0.00000,    0.05000, & ! 8   
     &        0.95000,    0.60000,    0.01170,    0.43000,    0.23100, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.67000,    1.00000, & ! 9   
     &        1.00000,    0.42500,    0.24900,    0.03000,    0.00000, & ! +   
     &        0.00000,    0.68000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.46400,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.06300,    0.00000,    0.00000, & ! +   
     &        0.41000,    0.41000,    0.85000,    0.00000,    0.00000, & ! 3   
     &        0.40000,    0.00000,    0.62000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.25000,    0.02600, & ! +   
     &        0.19100,    0.75000,    0.25000,    0.04500,    0.21800, & ! 5   
     &        0.45900,    0.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.63200,    0.21900,    0.75000, & ! 6   
     &        0.30800,    0.10000,    0.10000,    0.25000,    0.25000, & ! +   
     &        0.25000,    0.31500,    0.59900,    0.25000,    0.20300, & ! 7   
     &        0.00000,    0.00000,    0.68000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.93000,    0.70000,    0.00000,    0.50000, & ! 8   
     &        0.02600,    0.19100,    0.20800,    0.20000,    0.94000, & ! +   
     &        0.29000,    0.45800,    0.00000,    0.00000,    1.00000, & ! 9   
     &        0.00000,    0.00000,    1.00000,    0.63200,    0.21900, & ! +   
     &        1.00000,    0.38500,    0.00000,    0.00000,    0.75000, & ! O   
     &        0.33400,    0.50000,    0.65000,    0.13100,    0.27000, & ! +   
     &        0.03000,    0.00000,    0.00000,    0.68000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    1.24000,    0.70000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.50300,    0.48500, & ! 2   
     &        1.00000,    0.20000,    1.00000,    1.00000,    0.50000, & ! +   
     &        0.00000,    0.00000,    0.50000,    0.00000,    0.00000, & ! 3   
     &        1.00000,    0.63200,    1.00000,    1.00000,    0.38500, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.67000,    0.00000, & ! 4   
     &        0.65000,    0.14500,    0.30000,    0.03000,    0.00000, & ! +   
     &        0.00000,    0.68000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.46400,    0.00000,    0.00000,    0.64000,    0.28500, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.12500,    0.26000, & ! 6   
     &        0.37500,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    2.00000,    0.00000, & ! 7   
     &        0.00000,    0.30000,    0.34800,    1.00000,    0.50000, & ! +   
     &        1.00000,    2.00000,    0.00000,    0.50000,    0.70000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.52000,    0.34800,    0.36400,    0.95200,    0.52000, & ! O   
     &        0.65900,    0.00000,    0.00000,    0.29000,    0.22200, & ! +   
     &        0.58500,    0.32000,    0.31700,    0.35000,    0.21200, & ! 1   
     &        0.40000,    0.35000,    0.35000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.30000,    0.00000,    0.00000, & ! 2   
     &        0.50000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.44500,    0.00000,    0.00000,    0.26000, & ! 3   
     &        0.08000,    0.80100,    0.37400,    0.60000,    0.05500, & ! +   
     &        0.30900,    0.30900,    0.00000,    0.00000,    0.07500, & ! 4   
     &        0.32700,    0.44300,    0.00000,    0.00000,    0.07000, & ! +   
     &        0.35000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.27500,    0.27500,    0.27500,    0.50000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.44000, & ! 6   
     &        0.44000,    0.44000,    0.74000,    2.00000,    0.30000, & ! +   
     &        0.61200,    0.05800,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.61600, & ! +   
     &        0.90000,    1.00000,    0.48000,    0.56000,    0.09000, & ! 9   
     &        0.00000,    0.00000,    0.12000,    0.23500,    0.00000, & ! +   
     &        0.00000,    0.37000,    0.00000,    0.73000,    0.73000, & ! O   
     &        0.27000,    0.31000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.50000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00250,    0.00410, & ! +   
     &        0.13730,    0.01530,    0.30000,    0.01230,    0.01390, & ! 3   
     &        0.26070,    0.04910,    0.06400/           !         &  

      DATA ( SC( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.43400, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.82500,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.50000, & ! 2   
     &        0.50000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.35000, & ! +   
     &        1.00000,    1.00000,    0.98000,    1.00000,    1.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.10000,    0.01000,    0.01000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.41000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.54000, & ! 2   
     &        0.37000,    0.04000,    0.04000,    0.15000,    0.10000, & ! +   
     &        0.02000,    0.02000,    0.09800,    0.28000,    0.10000, & ! 3   
     &        1.50000,    1.50000,    0.70000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.50000,    0.00000,    0.00000,    0.28000, & ! +   
     &        0.01000,    0.15000,    0.01000,    0.30800,    0.01000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.93500,    0.86400, & ! 7   
     &        0.15000,    0.00000,    0.44000,    0.20000,    0.91800, & ! +   
     &        0.00000,    0.00000,    0.50000,    0.00000,    0.00000, & ! 8   
     &        1.05000,    0.70000,    0.10530,    0.11000,    0.05800, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.46000, & ! 9   
     &        0.00000,    0.42500,    0.24900,    0.27000,    0.00000, & ! +   
     &        0.00000,    0.68000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.06300,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.26000,    0.00000,    0.13000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.25000,    0.82700, & ! +   
     &        0.77700,    0.19700,    0.25000,    0.25000,    0.25000, & ! 5   
     &        0.60000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.05000,    0.73700,    0.25000,    0.25000, & ! 6   
     &        0.25000,    0.00000,    0.00000,    0.25000,    0.25000, & ! +   
     &        0.25000,    0.10000,    0.59900,    0.15000,    0.52500, & ! 7   
     &        0.00000,    0.00000,    0.68000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.34800,    0.70000,    0.00000,    0.00000, & ! 8   
     &        0.13000,    0.04200,    0.21700,    0.50000,    0.06000, & ! +   
     &        0.50000,    0.60000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    1.05000,    0.73700,    0.30500, & ! +   
     &        1.00000,    0.30800,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.50000,    0.00000,    0.00000,    0.01400,    0.27000, & ! +   
     &        0.27000,    0.00000,    0.00000,    0.68000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.46400,    0.70000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! 2   
     &        0.20300,    0.00000,    0.18000,    0.20000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.50000,    0.00000,    0.00000, & ! 3   
     &        1.05000,    0.73700,    1.05000,    1.00000,    0.61500, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.35000,    0.14500,    0.70000,    0.27000,    0.00000, & ! +   
     &        0.00000,    0.68000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.14900,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.50000,    0.00000,    0.50000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.50000,    0.30000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.48000,    0.32200,    1.20800,    0.43200,    0.48000, & ! O   
     &        0.23100,    0.00000,    0.00000,    0.29000,    0.22200, & ! +   
     &        0.31500,    0.29000,    0.29300,    1.00000,    0.34800, & ! 1   
     &        0.26000,    0.35000,    0.35000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.07600,    0.00000,    0.00000,    0.44000, & ! 3   
     &        0.02000,    1.06900,    0.56200,    0.40000,    0.30900, & ! +   
     &        0.33600,    0.33600,    0.00000,    0.00000,    0.89000, & ! 4   
     &        0.17900,    0.00000,    0.00000,    0.00000,    0.07000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    1.95000,    1.12500,    1.12500,    0.25000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.25000, & ! 6   
     &        0.25000,    0.25000,    0.26000,    0.48000,    0.30000, & ! +   
     &        0.63200,    0.19300,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.15400, & ! +   
     &        0.00000,    0.00000,    0.48000,    0.17000,    0.09000, & ! 9   
     &        0.00000,    0.00000,    0.12000,    0.23500,    0.00000, & ! +   
     &        0.00000,    0.63000,    0.00000,    0.73000,    0.67000, & ! O   
     &        0.17000,    0.31000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00260,    0.00350, & ! +   
     &        0.00050,    0.10430,    0.20280,    0.12390,    0.10270, & ! 3   
     &        0.07020,    0.25770,    0.03850/           !         &  

      DATA ( SC( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.21600, & ! 1   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.50000, & ! 2   
     &        0.50000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.01000,    0.78000,    0.79000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.46000, & ! 2   
     &        0.00000,    0.02000,    0.09000,    0.02000,    0.09000, & ! +   
     &        0.14000,    0.14000,    0.20400,    0.10000,    0.24300, & ! 3   
     &        0.05000,    0.05000,    0.25000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.50000,    0.00000,    0.00000,    0.01000, & ! +   
     &        0.00000,    0.22400,    0.00000,    0.22400,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.50400,    0.01800, & ! 7   
     &        0.64200,    0.00000,    0.06000,    0.09000,    0.08200, & ! +   
     &        0.00000,    0.00000,    0.05000,    0.00000,    0.00000, & ! 8   
     &        0.05000,    0.07300,    0.66000,    0.44000,    0.78000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.54000, & ! 9   
     &        0.00000,    0.42500,    0.03500,    0.18000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.06300,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.40000,    0.00000,    0.25000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.19800, & ! +   
     &        0.25100,    0.65200,    0.00000,    0.25000,    0.25000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.07700,    0.25000,    0.25000, & ! 6   
     &        0.25000,    0.00000,    0.00000,    0.12500,    0.00000, & ! +   
     &        0.00000,    1.90000,    0.03600,    0.35000,    0.13500, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.25000,    0.30000,    0.00000,    0.00000, & ! 8   
     &        0.27300,    0.38100,    0.64200,    0.00000,    0.50000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.07700,    0.50000, & ! +   
     &        1.00000,    0.50000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.01400,    0.63000, & ! +   
     &        0.70000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.50000,    0.30000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.51900,    0.02400, & ! 2   
     &        0.86900,    0.00000,    0.02000,    0.85000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.07700,    1.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    1.00000,    0.70000,    0.70000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    1.50000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.25000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.05000,    1.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.48000,    0.32200,    0.12000,    0.43200,    0.48000, & ! O   
     &        0.01800,    0.00000,    0.00000,    1.00000,    0.20500, & ! +   
     &        0.31500,    0.29000,    0.29300,    0.00000,    0.34800, & ! 1   
     &        0.14000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.55500,    0.00000,    0.00000,    0.09000, & ! 3   
     &        0.32000,    0.34300,    0.56200,    0.40000,    0.09300, & ! +   
     &        0.14400,    0.14400,    0.00000,    0.00000,    0.44500, & ! 4   
     &        0.10200,    0.00000,    0.00000,    0.00000,    0.85000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.37500,    0.82500,    0.82500,    0.25000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.69000, & ! 6   
     &        0.69000,    0.69000,    0.24000,    0.00000,    0.30000, & ! +   
     &        0.31800,    0.33500,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.03500, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.56000,    0.58000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.54000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.67000, & ! O   
     &        0.17000,    0.10000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00230,    0.22390, & ! +   
     &        0.20510,    0.18930,    0.04710,    0.18310,    0.20450, & ! 3   
     &        0.11160,    0.07390,    0.26670/           !         &  

      DATA ( SC( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.06800,    0.37000,    0.13000,    0.14000, & ! +   
     &        0.65000,    0.46000,    0.14000,    0.54500,    0.08000, & ! 3   
     &        0.05000,    0.05000,    0.25000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.28000, & ! +   
     &        0.00000,    0.01000,    0.00000,    0.01000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.13200,    0.04500, & ! 7   
     &        0.19700,    0.00000,    0.13000,    0.02000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.17700,    0.14300,    0.07000,    0.28900, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.15000,    0.96500,    0.70000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.02500,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.14000,    0.00000,    0.12000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.49700, & ! +   
     &        0.61800,    0.25000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.18600,    1.00000,    1.00000, & ! 6   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.07300,    0.35000,    0.10500, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.25000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.66200,    0.82400,    0.49500,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.18600,    1.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.98600,    0.63000, & ! +   
     &        0.18000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.14700,    0.24100, & ! 2   
     &        0.00000,    0.00000,    0.09000,    0.04000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.18600,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.18000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.25000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.45000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.67000,    0.24000,    0.04800,    0.00000, & ! O   
     &        0.27100,    0.00000,    0.00000,    1.00000,    0.20500, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! 1   
     &        0.14000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.23100,    0.00000,    0.00000,    0.09000, & ! 3   
     &        0.22000,    0.34300,    0.56200,    1.60000,    0.21600, & ! +   
     &        0.14400,    0.14400,    0.00000,    0.00000,    0.21400, & ! 4   
     &        0.34900,    0.00000,    0.00000,    0.00000,    0.85000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.07400,    0.37500,    0.37500,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.89200, & ! 6   
     &        0.89200,    0.89200,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.31400,    0.33500,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.17000,    0.58000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.23000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.06000, & ! O   
     &        0.46000,    0.43000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.29440,    0.18200, & ! +   
     &        0.17640,    0.16680,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

      DATA ( SC( IRXXN,  8 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.43000,    0.02600,    0.00100,    0.58000, & ! +   
     &        0.53000,    0.04000,    0.14400,    0.36000,    0.42000, & ! 3   
     &        0.08000,    0.08000,    0.11000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.84000,    0.00000,    0.84000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.16500,    0.20300, & ! 7   
     &        0.00000,    0.00000,    0.03000,    0.03500,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.05000,    0.08000,    0.12000,    0.28900, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.21000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.03800,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.26000,    0.00000,    0.12000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.05000, & ! +   
     &        0.25000,    0.25000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.65000,    0.25000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.50000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.06700,    0.50000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.98600,    0.90000, & ! +   
     &        0.10500,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.07500,    0.06000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.21000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.35000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.24000,    0.04800,    0.00000, & ! O   
     &        0.07600,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.21400,    0.00000,    0.00000,    0.69000, & ! 3   
     &        0.29000,    0.34300,    0.93600,    0.00000,    0.23500, & ! +   
     &        0.72300,    0.72300,    0.00000,    0.00000,    0.21400, & ! 4   
     &        0.11200,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.25100,    0.07400,    0.07400,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.07000,    0.30700,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.06000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.10000,    0.39000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.20210,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

      DATA ( SC( IRXXN,  9 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.02000,    0.01000,    0.36000,    0.46100, & ! +   
     &        1.00000,    0.79000,    0.00000,    0.54500,    0.02800, & ! 3   
     &        0.65000,    0.70000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.04200,    0.03300, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.01500,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.80300,    0.06000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.25000, & ! +   
     &        0.25000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.25000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.50000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.50000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.09500,    0.06300, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.15000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.26600,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.32000,    0.42800,    0.37400,    0.00000,    0.10100, & ! +   
     &        0.35500,    0.35500,    0.00000,    0.00000,    0.44500, & ! 4   
     &        0.32700,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.25100,    0.25100,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.07000,    0.30700,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.06000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.01000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00190,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

      DATA ( SC( IRXXN, 10 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.01500,    0.01000,    0.35000,    0.18900, & ! +   
     &        0.00000,    0.01000,    0.00000,    0.07500,    0.49100, & ! 3   
     &        0.00000,    0.65000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00200,    0.21700, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.25000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.24700, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.26600,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.06000,    0.00000,    0.06400,    0.00000,    0.10100, & ! +   
     &        0.30900,    0.30900,    0.00000,    0.00000,    0.27100, & ! 4   
     &        0.06800,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.12000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00230,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

      DATA ( SC( IRXXN, 11 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00600,    0.09000,    0.90000,    0.28000, & ! +   
     &        0.00000,    0.07000,    0.00000,    0.00000,    0.00300, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.06300,    0.03300, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.04800, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.23100,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.16000,    0.00000,    0.00000,    0.00000,    0.21600, & ! +   
     &        1.00000,    1.33600,    0.00000,    0.00000,    0.01800, & ! 4   
     &        0.21200,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.26000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

      DATA ( SC( IRXXN, 12 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.03200,    0.45700,    0.39000,    0.15300, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.04400, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.27200, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.27500, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.23500, & ! +   
     &        0.33600,    0.00000,    0.00000,    0.00000,    0.44500, & ! 4   
     &        0.14800,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

      DATA ( SC( IRXXN, 13 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.56000,    0.73000,    0.15000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.02200, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.28900, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

      DATA ( SC( IRXXN, 14 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.01000,    0.11000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.11400, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.23100, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

      DATA ( SC( IRXXN, 15 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.44000,    0.01700,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

      DATA ( SC( IRXXN, 16 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.03000,    0.04400,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

      DATA ( SC( IRXXN, 17 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.02000,    0.01700,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

      DATA ( SC( IRXXN, 18 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.06000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

      DATA ( SC( IRXXN, 19 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.01000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

      DATA ( SC( IRXXN, 20 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.03000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

      DATA ( SC( IRXXN, 21 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.06000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

      INTEGER            :: NREACT( NRXNS )

      DATA ( NREACT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    2,    2,    2,    2,    1,    2,    1, & ! 3   
     &      1,    1,    1,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    1,    1,    2,    1,    2,    2,    2, & ! 6   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    1,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 5   
     &      1,    1,    1,    1,    1,    2,    1,    2,    1,    2, & ! 6   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    1,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    2,    1,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 8   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 9   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2/     !  3   
      INTEGER            :: NPRDCT( NRXNS )

      DATA ( NPRDCT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    1,    2,    1,    2,    2,    2,    4,    1, & ! O   
     &      2,    3,    3,    2,    6,    3,    2,    3,    6,    4, & ! 1   
     &      1,    2,    2,    3,    6,    6,    3,    3,    3,    2, & ! 2   
     &      4,    2,    2,    1,    1,    1,    1,    1,    0,    1, & ! 3   
     &      1,    1,    1,    0,    1,    1,    1,    1,    1,    2, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    2,    3,    1, & ! 5   
     &      2,    1,    1,    2,    1,    1,    2,    1,    3,    1, & ! 6   
     &      1,    1,    1,    1,    4,    1,    1,    1,    1,    5, & ! 7   
     &      5,    5,    5,    5,    5,    1,    1,    1,    2,    1, & ! 8   
     &      1,    1,    1,    1,    2,    2,    1,    2,    2,    2, & ! 9   
     &      6,    6,    6,    1,    4,    4,    4,    1,    2,    2, & ! O   
     &      3,    2,    3,    5,    4,    1,    1,    2,    4,    3, & ! 1   
     &      3,    2,    2,    2,    6,    5,   21,   17,   13,   12, & ! 2   
     &      9,   12,    8,   10,   12,    9,   10,    8,    5,    1, & ! 3   
     &      2,    2,    2,    3,    1,    2,    2,    3,    2,    2, & ! 4   
     &      2,    2,    3,    3,    4,    4,    6,    2,    2,    7, & ! 5   
     &      5,    8,    5,    8,    5,    1,    2,    1,    2,    1, & ! 6   
     &      2,    3,    3,   11,   14,    7,    4,    8,    9,    6, & ! 7   
     &      2,    4,    6,    2,    4,    6,    8,    9,    9,    8, & ! 8   
     &      2,    2,    3,    4,    6,    4,    7,    7,    8,    2, & ! 9   
     &      2,    5,    2,    3,    3,    4,    3,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    9,    2,    2, & ! 2   
     &      4,    4,    4,    1,    1,    8,    1,    8,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    3,    5,   10, & ! 4   
     &      9,    8,    5,    6,    6,    5,    3,    4,    4,    3, & ! 5   
     &      3,    5,    7,    7,    7,    7,    4,    4,    6,    5, & ! 6   
     &      5,    6,    7,    8,    9,    3,    3,    5,    3,    3, & ! 7   
     &      3,    8,    6,    2,    4,    9,    8,    7,    5,    6, & ! 8   
     &      5,    5,    2,    3,    4,    2,    3,    5,    7,    7, & ! 9   
     &      6,    7,    1,    2,    4,    5,    4,    4,    8,    8, & ! O   
     &      9,    2,    2,    5,    3,    2,    3,    6,    6,    1, & ! 1   
     &      1,    3,    3,    9,   12,    6,    4,    7,    7,    5, & ! 2   
     &      2,    3,    5,    2,    3,    5,    7,    6,    5,    6, & ! 3   
     &      2,    2,    3,    4,    3,    5,    6,    6,    8,    2, & ! 4   
     &      2,    5,    2,    3,    3,    4,    3,    2,    6,    5, & ! 5   
     &      1,    1,    0,    3,    3,    3,    3,    3,    3,    2, & ! 6   
     &      2,    2,    3,    4,    1,    1,    4,    9,    4,    6, & ! 7   
     &      4,    4,    3,    7,    6,    1,    1,    1,    1,    1, & ! 8   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    2, & ! 9   
     &      6,    7,    8,    8,    6,   11,    2,    2,    7,    7, & ! O   
     &      6,    7,    7,    5,    6,    7,    5,    5,    1,    1, & ! 1   
     &      3,    1,    4,    3,    3,    4,    3,    3,    2,    3, & ! 2   
     &      1,    8,    3,    1,    8,   11,    9,   10,    7,   12, & ! 3   
     &     12,   11,    1,    1,   14,   12,    4,    2,    2,    7, & ! 4   
     &      5,    2,    3,    1,    1,    1,    8,   10,    9,    7, & ! 5   
     &      3,    3,    2,    2,    7,    7,    7,    6,    5,    6, & ! 6   
     &      9,   10,    3,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    3,    4,    4,    3,    6, & ! 8   
     &      4,    4,    5,    7,    9,    3,    1,    5,    7,    2, & ! 9   
     &      2,    5,    3,    5,    7,    8,   11,    3,    3,    2, & ! O   
     &      2,    3,    4,    4,    4,    4,    1,    2,    2,    1, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    1,    2,    0,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    2,   10,    7, & ! 2   
     &      7,    7,    6,    6,    6,    6,    6,    6/     !  3   

      INTEGER, PARAMETER :: NMPHOT =  71
      INTEGER            :: IPH( NMPHOT,3 )

      DATA ( IPH( IRXXN,1 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & ! O   
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & ! 1   
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & ! 2   
     &     31,   32,   33,  124,  460,  461,  462,  463,  464,  465, & ! 3   
     &    466,  467,  468,  469,  470,  471,  472,  473,  474,  475, & ! 4   
     &    476,  477,  478,  479,  480,  481,  482,  483,  484,  485, & ! 5   
     &    486,  487,  488,  489,  510,  511,  512,  513,  514,  515, & ! 6   
     &    516/     !  7   

      DATA ( IPH( IRXXN,2 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & ! O   
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & ! 1   
     &     21,   22,   23,   24,   24,   24,   25,   26,   26,   27, & ! 2   
     &     28,   29,   30,   26,   31,   14,   32,   26,   33,   26, & ! 3   
     &     26,   26,   19,   19,   26,   26,   26,   13,   34,   35, & ! 4   
     &     36,   36,   34,   34,   34,   34,   34,   34,   34,   36, & ! 5   
     &     37,   38,   39,   40,   26,   26,   37,   16,   16,   35, & ! 6   
     &     34/     !  7   

      DATA ( IPH( IRXXN,3 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & ! O   
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & ! 1   
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & ! 2   
     &     31,   32,   33,   34,   35,   36,   37,   38,   39,   40, & ! 3   
     &     41,   42,   43,   44,   45,   46,   47,   48,   49,   50, & ! 4   
     &     51,   52,   53,   54,   55,   56,   57,   58,   59,   60, & ! 5   
     &     61,   62,   63,   64,   65,   66,   67,   68,   69,   70, & ! 6   
     &     71/     !  7   

      INTEGER, PARAMETER :: MHETERO =  31
      INTEGER            :: IHETERO( MHETERO,2 )

      DATA ( IHETERO( IRXXN,1 ), IRXXN = 1, MHETERO ) / & 
     &    535,  536,  538,  539,  540,  541,  542,  543,  544,  545, & 
     &    546,  547,  548,  549,  550,  551,  552,  553,  554,  555, & 
     &    556,  557,  558,  559,  560,  561,  562,  563,  564,  565, & 
     &    566/

      DATA ( IHETERO( IRXXN,2 ), IRXXN = 1, MHETERO ) / & 
     &      1,    2,    3,    3,    3,    4,    5,    6,    7,    8, & 
     &      9,   10,   11,   12,   13,   14,   15,   16,   17,   18, & 
     &     18,   19,   20,   21,   22,   23,   24,   25,    6,    7, & 
     &     26/

      INTEGER, PARAMETER :: NPHOTAB =  40
      CHARACTER( 16 )    :: PHOTAB( NPHOTAB )

      DATA ( PHOTAB( IRXXN ), IRXXN = 1, NPHOTAB ) / & 
     &   'O3O3P_NASA06    ', 'O3O1D_NASA06    ', 'H2O2_RACM2      ', & 
     &   'NO2_RACM2       ', 'NO3NO_RACM2     ', 'NO3NO2_RACM2    ', & 
     &   'HONO_RACM2      ', 'HNO3_RACM2      ', 'HNO4_RACM2      ', & 
     &   'HCHO_MOL_RACM2  ', 'HCHO_RAD_RACM2  ', 'CH3CHO_RACM2    ', & 
     &   'ALD_RACM2       ', 'CH3COCH3_RACM2  ', 'UALD_RACM2      ', & 
     &   'MEK_RACM2       ', 'KET_RACM2       ', 'HKET_RACM2      ', & 
     &   'MACR_RACM2      ', 'MVK_RACM2       ', 'GLYH2_RACM2     ', & 
     &   'GLYF_RACM2      ', 'GLYHX_RACM2     ', 'MGLY_RACM2      ', & 
     &   'BALD_RACM2      ', 'OP1_RACM2       ', 'PAA_RACM2       ', & 
     &   'ONIT_RACM2      ', 'PAN1_RACM2      ', 'PAN2_RACM2      ', & 
     &   'HPALD_RACM2     ', 'HOCCHO_RACM2    ', 'PYRUACID_RACM2  ', & 
     &   'ISOPNB_RACM2    ', 'ISOPND_RACM2    ', 'IMONIT_RACM2    ', & 
     &   'PROPNN_RACM2    ', 'ETHLN_RACM2     ', 'MACRN_RACM2     ', & 
     &   'MVKN_RACM2      '/

      INTEGER, PARAMETER :: NHETERO =  26
      CHARACTER( 16 )    :: HETERO( NHETERO )

      DATA ( HETERO( IRXXN ), IRXXN = 1, NHETERO ) / & 
     &   'HETERO_N2O5IJ   ', 'HETERO_NO2      ', 'HETERO_IEPOX    ', &
     &   'HETERO_GLY      ', 'HETERO_MGLY     ', 'HETERO_ISOPND   ', &
     &   'HETERO_ISOPNB   ', 'HETERO_MVKN     ', 'HETERO_MACRN    ', &
     &   'HETERO_DHDN     ', 'HETERO_IMONIT   ', 'HETERO_UTONIT   ', &
     &   'HETERO_UTONIN   ', 'HETERO_TONIT    ', 'HETERO_TONIN    ', &
     &   'HETERO_TONIH    ', 'HETERO_HONIT    ', 'HETERO_INHE     ', &
     &   'HETERO_IHPN     ', 'HETERO_IHDPN    ', 'HETERO_IDHPN    ', &
     &   'HETERO_R4N      ', 'HETERO_R4NO     ', 'HETERO_INPD     ', &
     &   'HETERO_INPB     ', 'HETERO_ISNP     '/

      CHARACTER( 16 )    :: RXLABEL( NRXNS )

      DATA ( RXLABEL( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &    'R001            ', 'R002            ', 'R003            ', & ! 0   
     &    'R004            ', 'R005            ', 'R006            ', & ! 1   
     &    'R007            ', 'R008            ', 'R009            ', & ! 2   
     &    'R010            ', 'R011            ', 'R012            ', & ! 3   
     &    'R013            ', 'R014            ', 'R015            ', & ! 4   
     &    'R016            ', 'R017            ', 'R018            ', & ! 5   
     &    'R019            ', 'R020            ', 'R021            ', & ! 6   
     &    'R022            ', 'R023            ', 'R024            ', & ! 7   
     &    'R025            ', 'R026            ', 'R027            ', & ! 8   
     &    'R028            ', 'R029            ', 'R030            ', & ! 9   
     &    'R031            ', 'R032            ', 'R033            ', & ! 0   
     &    'R034            ', 'R035            ', 'R036            ', & ! 1   
     &    'R037            ', 'R038            ', 'R039            ', & ! 2   
     &    'R040            ', 'R041            ', 'R042            ', & ! 3   
     &    'R043            ', 'R044            ', 'R045            ', & ! 4   
     &    'R046            ', 'R047            ', 'R048            ', & ! 5   
     &    'R049            ', 'R050            ', 'R051            ', & ! 6   
     &    'R052            ', 'R053            ', 'R054            ', & ! 7   
     &    'R055            ', 'R056            ', 'R057            ', & ! 8   
     &    'R058            ', 'R059            ', 'R060            ', & ! 9   
     &    'R061            ', 'R062            ', 'R063            ', & ! 0   
     &    'R064            ', 'R065            ', 'R066            ', & ! 1   
     &    'R067            ', 'R068            ', 'R069            ', & ! 2   
     &    'R070            ', 'R071            ', 'R072            ', & ! 3   
     &    'R073            ', 'R074            ', 'R075            ', & ! 4   
     &    'R076            ', 'R077            ', 'R078            ', & ! 5   
     &    'R079            ', 'R080            ', 'R081            ', & ! 6   
     &    'R082            ', 'R083            ', 'R084            ', & ! 7   
     &    'R085            ', 'R086            ', 'R087            ', & ! 8   
     &    'R088            ', 'R089            ', 'R090            ', & ! 9   
     &    'R091            ', 'R092            ', 'R093            ', & ! 0   
     &    'R094            ', 'R095            ', 'R096            ', & ! 1   
     &    'R097            ', 'R098            ', 'R099            ', & ! 2   
     &    'R100            ', 'R101            ', 'R102            ', & ! 3   
     &    'R103            ', 'R104            ', 'R105            ', & ! 4   
     &    'R106            ', 'R107            ', 'R108            ', & ! 5   
     &    'R109            ', 'R110            ', 'R111            ', & ! 6   
     &    'R112            ', 'R113            ', 'R114            ', & ! 7   
     &    'R115            ', 'R116            ', 'R117            ', & ! 8   
     &    'R118            ', 'R119            ', 'R120            ', & ! 9   
     &    'R121            ', 'R122            ', 'R123            ', & ! 0   
     &    'R124            ', 'R125            ', 'R126            ', & ! 1   
     &    'R127            ', 'R128            ', 'R129            ', & ! 2   
     &    'R130            ', 'R131            ', 'R132            ', & ! 3   
     &    'R133            ', 'R134            ', 'R135            ', & ! 4   
     &    'R136            ', 'R137            ', 'R138            ', & ! 5   
     &    'R139            ', 'R140            ', 'R141            ', & ! 6   
     &    'R142            ', 'R143            ', 'R144            ', & ! 7   
     &    'R145            ', 'R146            ', 'R147            ', & ! 8   
     &    'R148            ', 'R149            ', 'R150            ', & ! 9   
     &    'R151            ', 'R152            ', 'R153            ', & ! 0   
     &    'R154            ', 'R155            ', 'R156            ', & ! 1   
     &    'R157            ', 'R158            ', 'R159            ', & ! 2   
     &    'R160            ', 'R161            ', 'R162            ', & ! 3   
     &    'R163            ', 'R164            ', 'R165            ', & ! 4   
     &    'R166            ', 'R167            ', 'R168            ', & ! 5   
     &    'R169            ', 'R170            ', 'R171            ', & ! 6   
     &    'R172            ', 'R173            ', 'R174            ', & ! 7   
     &    'R175            ', 'R176            ', 'R177            ', & ! 8   
     &    'R178            ', 'R179            ', 'R180            ', & ! 9   
     &    'R181            ', 'R182            ', 'R183            ', & ! 0   
     &    'R184            ', 'R185            ', 'R186            ', & ! 1   
     &    'R187            ', 'R188            ', 'R189            ', & ! 2   
     &    'R190            ', 'R191            ', 'R192            ', & ! 3   
     &    'R193            ', 'R194            ', 'R195            ', & ! 4   
     &    'R196            ', 'R197            ', 'R198            ', & ! 5   
     &    'R199            ', 'R200            ', 'R201            ', & ! 6   
     &    'R202            ', 'R203            ', 'R204            ', & ! 7   
     &    'R205            ', 'R206            ', 'R207            ', & ! 8   
     &    'R208            ', 'R209            ', 'R210            ', & ! 9   
     &    'R211            ', 'R212            ', 'R213            ', & ! 0   
     &    'R214            ', 'R215            ', 'R216            ', & ! 1   
     &    'R217            ', 'R218            ', 'R219            ', & ! 2   
     &    'R220            ', 'R221            ', 'R222            ', & ! 3   
     &    'R223            ', 'R224            ', 'R225            ', & ! 4   
     &    'R226            ', 'R227            ', 'R228            ', & ! 5   
     &    'R229            ', 'R230            ', 'R231            ', & ! 6   
     &    'R232            ', 'R233            ', 'R234            ', & ! 7   
     &    'R235            ', 'R236            ', 'R237            ', & ! 8   
     &    'R238            ', 'R239            ', 'R240            ', & ! 9   
     &    'R241            ', 'R242            ', 'R243            ', & ! 0   
     &    'R244            ', 'R245            ', 'R246            ', & ! 1   
     &    'R247            ', 'R248            ', 'R249            ', & ! 2   
     &    'R250            ', 'R251            ', 'R252            ', & ! 3   
     &    'R253            ', 'R254            ', 'R255            ', & ! 4   
     &    'R256            ', 'R257            ', 'R258            ', & ! 5   
     &    'R259            ', 'R260            ', 'R261            ', & ! 6   
     &    'R262            ', 'R263            ', 'R264            ', & ! 7   
     &    'R265            ', 'R266            ', 'R267            ', & ! 8   
     &    'R268            ', 'R269            ', 'R270            ', & ! 9   
     &    'R271            ', 'R272            ', 'R273            ', & ! 0   
     &    'R274            ', 'R275            ', 'R276            ', & ! 1   
     &    'R277            ', 'R278            ', 'R279            ', & ! 2   
     &    'R280            ', 'R281            ', 'R282            ', & ! 3   
     &    'R283            ', 'R284            ', 'R285            ', & ! 4   
     &    'R286            ', 'R287            ', 'R288            ', & ! 5   
     &    'R289            ', 'R290            ', 'R291            ', & ! 6   
     &    'R292            ', 'R293            ', 'R294            ', & ! 7   
     &    'R295            ', 'R296            ', 'R297            ', & ! 8   
     &    'R298            ', 'R299            ', 'R300            ', & ! 9   
     &    'R301            ', 'R302            ', 'R303            ', & ! 0   
     &    'R304            ', 'R305            ', 'R306            ', & ! 1   
     &    'R307            ', 'R308            ', 'R309            ', & ! 2   
     &    'R310            ', 'R311            ', 'R312            ', & ! 3   
     &    'R313            ', 'R314            ', 'R315            ', & ! 4   
     &    'R316            ', 'R317            ', 'R318            ', & ! 5   
     &    'R319            ', 'R320            ', 'R321            ', & ! 6   
     &    'R322            ', 'R323            ', 'R324            ', & ! 7   
     &    'R325            ', 'R326            ', 'R327            ', & ! 8   
     &    'R328            ', 'R329            ', 'R330            ', & ! 9   
     &    'R331            ', 'R332            ', 'R333            ', & ! 0   
     &    'R334            ', 'R335            ', 'R336            ', & ! 1   
     &    'R337            ', 'R338            ', 'R339            ', & ! 2   
     &    'R340            ', 'R341            ', 'R342            ', & ! 3   
     &    'R343            ', 'R344            ', 'R345            ', & ! 4   
     &    'R346            ', 'R347            ', 'R348            ', & ! 5   
     &    'R349            ', 'R350            ', 'R351            ', & ! 6   
     &    'R352            ', 'R353            ', 'R354            ', & ! 7   
     &    'R355            ', 'R356            ', 'R357            ', & ! 8   
     &    'R358            ', 'R359            ', 'R360            ', & ! 9   
     &    'R361            ', 'R362            ', 'R363            ', & ! 0   
     &    'R364            ', 'R365            ', 'R366            ', & ! 1   
     &    'R367            ', 'R368            ', 'R369            ', & ! 2   
     &    'R370            ', 'R371            ', 'R372            ', & ! 3   
     &    'R373            ', 'R374            ', 'R375            ', & ! 4   
     &    'R376            ', 'R377            ', 'R378            ', & ! 5   
     &    'R379            ', 'R380            ', 'R381            ', & ! 6   
     &    'R382            ', 'R383            ', 'R384            ', & ! 7   
     &    'R385            ', 'R386            ', 'R387            ', & ! 8   
     &    'R388            ', 'R389            ', 'R390            ', & ! 9   
     &    'R391            ', 'R392            ', 'R393            ', & ! 0   
     &    'R384            ', 'R395            ', 'R396            ', & ! 1   
     &    'R397            ', 'R398            ', 'R399            ', & ! 2   
     &    'R400            ', 'R401            ', 'R402            ', & ! 3   
     &    'R403            ', 'R404            ', 'R405            ', & ! 4   
     &    'R406            ', 'R407            ', 'R408            ', & ! 5   
     &    'R409            ', 'R410            ', 'R411            ', & ! 6   
     &    'R412            ', 'R413            ', 'R414            ', & ! 7   
     &    'R415            ', 'R416            ', 'R417            ', & ! 8   
     &    'R418            ', 'R419            ', 'R420            ', & ! 9   
     &    'R421            ', 'R422            ', 'R423            ', & ! 0   
     &    'R424            ', 'R425            ', 'R426            ', & ! 1   
     &    'R427            ', 'R428            ', 'R429            ', & ! 2   
     &    'R430            ', 'R431            ', 'R432            ', & ! 3   
     &    'R433            ', 'R434            ', 'R435            ', & ! 4   
     &    'R436            ', 'R437            ', 'R438            ', & ! 5   
     &    'R439            ', 'R440            ', 'R441            ', & ! 6   
     &    'R442            ', 'R443            ', 'R444            ', & ! 7   
     &    'R445            ', 'R446            ', 'R448            ', & ! 8   
     &    'R449            ', 'R450            ', 'R451            ', & ! 9   
     &    'R452            ', 'R453            ', 'R454            ', & ! 0   
     &    'R455            ', 'R456            ', 'R457            ', & ! 1   
     &    'R458            ', 'R459            ', 'R460            ', & ! 2   
     &    'R461            ', 'R462            ', 'R463            ', & ! 3   
     &    'R464            ', 'R465            ', 'R466            ', & ! 4   
     &    'R467            ', 'R468            ', 'R469            ', & ! 5   
     &    'R470            ', 'R471            ', 'R472            ', & ! 6   
     &    'R473            ', 'R474            ', 'R475            ', & ! 7   
     &    'R476            ', 'R477            ', 'R478            ', & ! 8   
     &    'R479            ', 'R480            ', 'R481            ', & ! 9   
     &    'R482            ', 'R483            ', 'R484            ', & ! 0   
     &    'R485            ', 'R486            ', 'R487            ', & ! 1   
     &    'R488            ', 'R489            ', 'R490            ', & ! 2   
     &    'R491            ', 'R492            ', 'R493            ', & ! 3   
     &    'R494            ', 'R495            ', 'R496            ', & ! 4   
     &    'R497            ', 'R498            ', 'R499            ', & ! 5   
     &    'R500            ', 'R501            ', 'R502            ', & ! 6   
     &    'R503            ', 'R504            ', 'R505            ', & ! 7   
     &    'R506            ', 'R507            ', 'R508            ', & ! 8   
     &    'R509            ', 'R510            ', 'R511            ', & ! 9   
     &    'R512            ', 'R513            ', 'R514            ', & ! 0   
     &    'R515            ', 'R516            ', 'R517            ', & ! 1   
     &    'R518            ', 'R519            ', 'R520            ', & ! 2   
     &    'R521            ', 'R522            ', 'SA01            ', & ! 3   
     &    'SA02            ', 'SA03            ', 'SA04            ', & ! 4   
     &    'SA05            ', 'SA06            ', 'SA07            ', & ! 5   
     &    'SA08            ', 'SA09            ', 'SA10            ', & ! 6   
     &    'SA11            ', 'SA12            ', 'SA13            ', & ! 7   
     &    'HET_N2O5        ', 'HET_N02         ', 'HAL_Ozone       ', & ! 8   
     &    'HET_IEPOX       ', 'HET_IEPOX       ', 'HET_IEPOX       ', & ! 9   
     &    'HET_GLY         ', 'HET_MGLY        ', 'HET_ISOPND      ', & ! 0   
     &    'HET_ISOPNB      ', 'HET_MVKN        ', 'HET_MACRN       ', & ! 1   
     &    'HET_DHDN        ', 'HET_IMONIT      ', 'HET_UTONIT      ', & ! 2   
     &    'HET_UTONIN      ', 'HET_TONIT       ', 'HET_TONIN       ', & ! 3   
     &    'HET_TONIH       ', 'HET_HONIT       ', 'HET_INHED       ', & ! 4   
     &    'HET_INHEB       ', 'HET_IHPN        ', 'HET_IHDPN       ', & ! 5   
     &    'HET_IDHPN       ', 'HET_R4N         ', 'HET_R4NO        ', & ! 6   
     &    'HET_INPD        ', 'HET_INPB        ', 'HET_IHND        ', & ! 7   
     &    'HET_IHNB        ', 'HET_ISNP        ', 'HYD_AHISOPNDJ   ', & ! 8   
     &    'HYD_AHIHNDJ     ', 'HYD_AHINPDJ     ', 'HYD_AHISOPNBJ   ', & ! 9   
     &    'HYD_AHINPBJ     ', 'HYD_AHIHNBJ     ', 'HYD_AHMVKNJ     ', & ! 0   
     &    'HYD_AHMACRNJ    ', 'HYD_AHDHDNJ     ', 'HYD_AHIMONITJ   ', & ! 1   
     &    'HYD_AHUTONITJ   ', 'HYD_AHUTONINJ   ', 'HYD_AHTONITJ    ', & ! 2   
     &    'HYD_AHTONINJ    ', 'HYD_AHTONIHJ    ', 'HYD_AHHONITJ    ', & ! 3   
     &    'HYD_AHIHPNJ     ', 'HYD_AHIHDPNJ    ', 'HYD_AHIDHPNJ    ', & ! 4   
     &    'HYD_AHR4NJ      ', 'HYD_AHR4NOJ     ', 'HYD_AHISNPJ     ', & ! 5   
     &    'HYD_AHINHEJ     ', 'HYD_AISOPNDJ    ', 'HYD_AIHNDJ      ', & ! 6   
     &    'HYD_AINPDJ      ', 'HYD_AISOPNBJ    ', 'HYD_AINPBJ      ', & ! 7   
     &    'HYD_AIHNBJ      ', 'HYD_AMVKNJ      ', 'HYD_AMACRNJ     ', & ! 8   
     &    'HYD_ADHDNJ      ', 'HYD_AIMONITJ    ', 'HYD_AUTONITJ    ', & ! 9   
     &    'HYD_AUTONINJ    ', 'HYD_ATONITJ     ', 'HYD_ATONINJ     ', & ! 0   
     &    'HYD_ATONIHJ     ', 'HYD_AHONITJ     ', 'HYD_AIHPNJ      ', & ! 1   
     &    'HYD_AIHDPNJ     ', 'HYD_AIDHPNJ     ', 'HYD_AR4NJ       ', & ! 2   
     &    'HYD_AR4NOJ      ', 'HYD_AISNPJ      ', 'HYD_AINHEJ      ', & ! 3   
     &    'OLIG_XYLENE1    ', 'OLIG_XYLENE2    ', 'OLIG_TOLUENE1   ', & ! 4   
     &    'OLIG_TOLUENE2   ', 'OLIG_BENZENE1   ', 'OLIG_BENZENE2   ', & ! 5   
     &    'OLIG_TERPENE1   ', 'OLIG_TERPENE2   ', 'OLIG_ISOPRENE1  ', & ! 6   
     &    'OLIG_ISOPRENE2  ', 'OLIG_SESQT1     ', 'OLIG_PAH1       ', & ! 7   
     &    'OLIG_PAH2       ', 'OLIG_ALK1       ', 'OLIG_ALK2       ', & ! 8   
     &    'PCSOA           ', 'POA_AGE1        ', 'POA_AGE2        ', & ! 9   
     &    'POA_AGE3        ', 'POA_AGE4        ', 'POA_AGE5        ', & ! 0   
     &    'POA_AGE6        ', 'POA_AGE7        ', 'POA_AGE8        ', & ! 1   
     &    'POA_AGE9        ', 'POA_AGE10       '/                   ! 2  

!    NSPECIAL     = Number of special rate coefficients
!    SPECIAL      = Names of special rate coefficients
!    NSPECIAL_RXN = Number of reactions with special rates
!    ISPECIAL     = Pointers to reactions using special rates and their special rate coefficients
!    MAXSPECTERMS = Max Number of each term type in  special rate coefficients
!    KC_COEFFS    = Coefficients of standard rate coefficients  times concentration terms 
!    INDEX_KTERMS  = Pointers to standard rate coefficients in  special rate coefficients
!    INDEX_CTERMS  = Pointers to species concentrations in  special rate coefficients
!    OPERATOR_COEFFS = Coefficients of preceeding special  rate coefficients used in special coefficient 
!    OPERATORS       = Pointers to preceeding special  rate coefficients used in special coefficient 

! Special Rate information not available ..
      INTEGER, PARAMETER :: NSPECIAL_RXN = 0
      INTEGER            :: ISPECIAL( 1, 2 )

! Special Rate information not available ...
      INTEGER, PARAMETER :: NSPECIAL = 0

! Special Rate information not available ...
      CHARACTER( 16 )    :: SPECIAL( 1 )

      INTEGER, PARAMETER :: MAXSPECTERMS = 700
      REAL( 8 )          :: KC_COEFFS( NSPECIAL + 1, MAXSPECTERMS)
      INTEGER            :: INDEX_KTERMS( NSPECIAL + 1, MAXSPECTERMS)
      INTEGER            :: INDEX_CTERMS( NSPECIAL + 1, MAXSPECTERMS)
      REAL( 8 )          :: OPERATOR_COEFFS( NSPECIAL + 1, MAXSPECTERMS)
      INTEGER            :: OPERATORS( NSPECIAL + 1, MAXSPECTERMS)


!    Steady-state species section
!    N_SS_SPC     = Number of species assumed to be in steady-state
!    SS_SPC_DIM   = Dimension paramete for steady-state species
!    SS_SPC       = Names of species assumed to be in steady-state
!    MAX_SS_LOSS  = Max no. of SS loss rxns for any SS species
!    MAX_SS_PROD  = Max no. of SS prod rxns for any SS species
!    N_LOSS_RXNS  = No. of SS loss rxns for each SS species
!    N_PROD_RXNS  = No. of SS prod rxns for each SS species
!    SS_LOSS_RXNS = List of SS loss rxns for each SS species
!    SS_PROD_RXNS = List of SS prod rxns for each SS species
!    SS_PROD_COEF = List of SS prod yields for each SS species
!    SS_RCT_IND   = SS species index if it is a rxn reactant

      INTEGER, PARAMETER :: N_SS_SPC =   0

      INTEGER, PARAMETER :: SS_SPC_DIM =   1

      INTEGER, PARAMETER :: MAX_SS_LOSS =   0

      INTEGER, PARAMETER :: MAX_SS_PROD =   0

      CHARACTER( 16 )    :: SS_SPC( 1 )

      INTEGER            :: N_LOSS_RXNS( 1 )
      INTEGER            :: N_PROD_RXNS( 1 )
      INTEGER            :: SS_LOSS_RXNS( 1, 1 )
      INTEGER            :: SS_PROD_RXNS( 1, 1 )
      INTEGER            :: SS_RCT_IND( 1 )

      REAL               :: SS_PROD_COEF( 1,1 ) 
       LOGICAL,  PARAMETER :: USE_SPECIAL_RATES = .FALSE.
! pointers and names to specific photolysis rates
       INTEGER, PARAMETER  :: IJ_O3O3P_NASA06     =   1
       INTEGER, PARAMETER  :: IJ_O3O1D_NASA06     =   2
       INTEGER, PARAMETER  :: IJ_H2O2_RACM2       =   3
       INTEGER, PARAMETER  :: IJ_NO2_RACM2        =   4
       INTEGER, PARAMETER  :: IJ_NO3NO_RACM2      =   5
       INTEGER, PARAMETER  :: IJ_NO3NO2_RACM2     =   6
       INTEGER, PARAMETER  :: IJ_HONO_RACM2       =   7
       INTEGER, PARAMETER  :: IJ_HNO3_RACM2       =   8
       INTEGER, PARAMETER  :: IJ_HNO4_RACM2       =   9
       INTEGER, PARAMETER  :: IJ_HCHO_MOL_RACM2   =  10
       INTEGER, PARAMETER  :: IJ_HCHO_RAD_RACM2   =  11
       INTEGER, PARAMETER  :: IJ_CH3CHO_RACM2     =  12
       INTEGER, PARAMETER  :: IJ_ALD_RACM2        =  13
       INTEGER, PARAMETER  :: IJ_CH3COCH3_RACM2   =  14
       INTEGER, PARAMETER  :: IJ_UALD_RACM2       =  15
       INTEGER, PARAMETER  :: IJ_MEK_RACM2        =  16
       INTEGER, PARAMETER  :: IJ_KET_RACM2        =  17
       INTEGER, PARAMETER  :: IJ_HKET_RACM2       =  18
       INTEGER, PARAMETER  :: IJ_MACR_RACM2       =  19
       INTEGER, PARAMETER  :: IJ_MVK_RACM2        =  20
       INTEGER, PARAMETER  :: IJ_GLYH2_RACM2      =  21
       INTEGER, PARAMETER  :: IJ_GLYF_RACM2       =  22
       INTEGER, PARAMETER  :: IJ_GLYHX_RACM2      =  23
       INTEGER, PARAMETER  :: IJ_MGLY_RACM2       =  24
       INTEGER, PARAMETER  :: IJ_BALD_RACM2       =  25
       INTEGER, PARAMETER  :: IJ_OP1_RACM2        =  26
       INTEGER, PARAMETER  :: IJ_PAA_RACM2        =  27
       INTEGER, PARAMETER  :: IJ_ONIT_RACM2       =  28
       INTEGER, PARAMETER  :: IJ_PAN1_RACM2       =  29
       INTEGER, PARAMETER  :: IJ_PAN2_RACM2       =  30
       INTEGER, PARAMETER  :: IJ_HPALD_RACM2      =  31
       INTEGER, PARAMETER  :: IJ_HOCCHO_RACM2     =  32
       INTEGER, PARAMETER  :: IJ_PYRUACID_RACM2   =  33
       INTEGER, PARAMETER  :: IJ_ISOPNB_RACM2     =  34
       INTEGER, PARAMETER  :: IJ_ISOPND_RACM2     =  35
       INTEGER, PARAMETER  :: IJ_IMONIT_RACM2     =  36
       INTEGER, PARAMETER  :: IJ_PROPNN_RACM2     =  37
       INTEGER, PARAMETER  :: IJ_ETHLN_RACM2      =  38
       INTEGER, PARAMETER  :: IJ_MACRN_RACM2      =  39
       INTEGER, PARAMETER  :: IJ_MVKN_RACM2       =  40
       INTEGER, PARAMETER  :: IK_HETERO_N2O5IJ    =   1
       INTEGER, PARAMETER  :: IK_HETERO_NO2       =   2
       INTEGER, PARAMETER  :: IK_HETERO_IEPOX     =   3
       INTEGER, PARAMETER  :: IK_HETERO_GLY       =   4
       INTEGER, PARAMETER  :: IK_HETERO_MGLY      =   5
       INTEGER, PARAMETER  :: IK_HETERO_ISOPND    =   6
       INTEGER, PARAMETER  :: IK_HETERO_ISOPNB    =   7
       INTEGER, PARAMETER  :: IK_HETERO_MVKN      =   8
       INTEGER, PARAMETER  :: IK_HETERO_MACRN     =   9
       INTEGER, PARAMETER  :: IK_HETERO_DHDN      =  10
       INTEGER, PARAMETER  :: IK_HETERO_IMONIT    =  11
       INTEGER, PARAMETER  :: IK_HETERO_UTONIT    =  12
       INTEGER, PARAMETER  :: IK_HETERO_UTONIN    =  13
       INTEGER, PARAMETER  :: IK_HETERO_TONIT     =  14
       INTEGER, PARAMETER  :: IK_HETERO_TONIN     =  15
       INTEGER, PARAMETER  :: IK_HETERO_TONIH     =  16
       INTEGER, PARAMETER  :: IK_HETERO_HONIT     =  17
       INTEGER, PARAMETER  :: IK_HETERO_INHE      =  18
       INTEGER, PARAMETER  :: IK_HETERO_IHPN      =  19
       INTEGER, PARAMETER  :: IK_HETERO_IHDPN     =  20
       INTEGER, PARAMETER  :: IK_HETERO_IDHPN     =  21
       INTEGER, PARAMETER  :: IK_HETERO_R4N       =  22
       INTEGER, PARAMETER  :: IK_HETERO_R4NO      =  23
       INTEGER, PARAMETER  :: IK_HETERO_INPD      =  24
       INTEGER, PARAMETER  :: IK_HETERO_INPB      =  25
       INTEGER, PARAMETER  :: IK_HETERO_ISNP      =  26
       END MODULE RXNS_DATA
