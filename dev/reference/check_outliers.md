# Annotate a "nacho" object for outliers

Add or update `"is_outlier"` column in the `"nacho"` field of an object
from a call to
[`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md) or
[`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md)
(`nacho_object$nacho`), using the current quality-control thresholds.

## Usage

``` r
check_outliers(nacho_object)
```

## Arguments

- nacho_object:

  \[[list](https://rdrr.io/r/base/list.html)\] A list object of class
  `"nacho"` obtained from
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md).

## Value

A \[[list](https://rdrr.io/r/base/list.html)\] object of class
`"nacho"`.

## Examples

``` r

data(GSE74821)
nacho_object <- check_outliers(GSE74821)
head(nacho_object$nacho)
#> Key: <IDFILE>
#>                                                                IDFILE
#>                                                                <char>
#> 1: GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 2: GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 3: GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 4: GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 5: GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 6: GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#>                                                                                                                                          file_path
#>                                                                                                                                             <char>
#> 1: /private/var/folders/gn/mxv05rj52wd1yg1hb018s4s40000gn/T/RtmpnCdvu5/GSE74821/GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 2: /private/var/folders/gn/mxv05rj52wd1yg1hb018s4s40000gn/T/RtmpnCdvu5/GSE74821/GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 3: /private/var/folders/gn/mxv05rj52wd1yg1hb018s4s40000gn/T/RtmpnCdvu5/GSE74821/GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 4: /private/var/folders/gn/mxv05rj52wd1yg1hb018s4s40000gn/T/RtmpnCdvu5/GSE74821/GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 5: /private/var/folders/gn/mxv05rj52wd1yg1hb018s4s40000gn/T/RtmpnCdvu5/GSE74821/GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 6: /private/var/folders/gn/mxv05rj52wd1yg1hb018s4s40000gn/T/RtmpnCdvu5/GSE74821/GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#>                   title geo_accession                status submission_date
#>                  <char>        <char>                <char>          <char>
#> 1: LGWU101986-1036378WU    GSM1934697 Public on Nov 10 2015     Nov 09 2015
#> 2: LGWU101986-1036378WU    GSM1934697 Public on Nov 10 2015     Nov 09 2015
#> 3: LGWU101986-1036378WU    GSM1934697 Public on Nov 10 2015     Nov 09 2015
#> 4: LGWU101986-1036378WU    GSM1934697 Public on Nov 10 2015     Nov 09 2015
#> 5: LGWU101986-1036378WU    GSM1934697 Public on Nov 10 2015     Nov 09 2015
#> 6: LGWU101986-1036378WU    GSM1934697 Public on Nov 10 2015     Nov 09 2015
#>    last_update_date   type channel_count           source_name_ch1 organism_ch1
#>              <char> <char>        <char>                    <char>       <char>
#> 1:      Nov 10 2015    RNA             1 Breast Tumor CALGB 101986 Homo sapiens
#> 2:      Nov 10 2015    RNA             1 Breast Tumor CALGB 101986 Homo sapiens
#> 3:      Nov 10 2015    RNA             1 Breast Tumor CALGB 101986 Homo sapiens
#> 4:      Nov 10 2015    RNA             1 Breast Tumor CALGB 101986 Homo sapiens
#> 5:      Nov 10 2015    RNA             1 Breast Tumor CALGB 101986 Homo sapiens
#> 6:      Nov 10 2015    RNA             1 Breast Tumor CALGB 101986 Homo sapiens
#>    characteristics_ch1     characteristics_ch1.1
#>                 <char>                    <char>
#> 1:      gender: female tissue type: Breast Tumor
#> 2:      gender: female tissue type: Breast Tumor
#> 3:      gender: female tissue type: Breast Tumor
#> 4:      gender: female tissue type: Breast Tumor
#> 5:      gender: female tissue type: Breast Tumor
#> 6:      gender: female tissue type: Breast Tumor
#>                                characteristics_ch1.2 biomaterial_provider_ch1
#>                                               <char>                   <char>
#> 1: fixative: Formalin-Fixed Paraffin Embedded Tissue                    CALGB
#> 2: fixative: Formalin-Fixed Paraffin Embedded Tissue                    CALGB
#> 3: fixative: Formalin-Fixed Paraffin Embedded Tissue                    CALGB
#> 4: fixative: Formalin-Fixed Paraffin Embedded Tissue                    CALGB
#> 5: fixative: Formalin-Fixed Paraffin Embedded Tissue                    CALGB
#> 6: fixative: Formalin-Fixed Paraffin Embedded Tissue                    CALGB
#>    treatment_protocol_ch1 growth_protocol_ch1 molecule_ch1
#>                    <char>              <char>       <char>
#> 1:                   <NA>                 N/A    total RNA
#> 2:                   <NA>                 N/A    total RNA
#> 3:                   <NA>                 N/A    total RNA
#> 4:                   <NA>                 N/A    total RNA
#> 5:                   <NA>                 N/A    total RNA
#> 6:                   <NA>                 N/A    total RNA
#>                                                           extract_protocol_ch1
#>                                                                         <char>
#> 1: http://www.nanostring.com/media/pdf/TN_Using_nCounter_with_FFPE_Samples.pdf
#> 2: http://www.nanostring.com/media/pdf/TN_Using_nCounter_with_FFPE_Samples.pdf
#> 3: http://www.nanostring.com/media/pdf/TN_Using_nCounter_with_FFPE_Samples.pdf
#> 4: http://www.nanostring.com/media/pdf/TN_Using_nCounter_with_FFPE_Samples.pdf
#> 5: http://www.nanostring.com/media/pdf/TN_Using_nCounter_with_FFPE_Samples.pdf
#> 6: http://www.nanostring.com/media/pdf/TN_Using_nCounter_with_FFPE_Samples.pdf
#>    label_ch1 label_protocol_ch1 taxid_ch1 hyb_protocol scan_protocol
#>       <char>             <char>    <char>       <char>        <char>
#> 1:        NA               <NA>      9606         <NA>          <NA>
#> 2:        NA               <NA>      9606         <NA>          <NA>
#> 3:        NA               <NA>      9606         <NA>          <NA>
#> 4:        NA               <NA>      9606         <NA>          <NA>
#> 5:        NA               <NA>      9606         <NA>          <NA>
#> 6:        NA               <NA>      9606         <NA>          <NA>
#>    data_processing platform_id    contact_name     contact_email contact_phone
#>             <char>      <char>          <char>            <char>        <char>
#> 1:            <NA>    GPL21114 Sherri,R,Davies daviess@wustl.edu    3147473073
#> 2:            <NA>    GPL21114 Sherri,R,Davies daviess@wustl.edu    3147473073
#> 3:            <NA>    GPL21114 Sherri,R,Davies daviess@wustl.edu    3147473073
#> 4:            <NA>    GPL21114 Sherri,R,Davies daviess@wustl.edu    3147473073
#> 5:            <NA>    GPL21114 Sherri,R,Davies daviess@wustl.edu    3147473073
#> 6:            <NA>    GPL21114 Sherri,R,Davies daviess@wustl.edu    3147473073
#>        contact_institute     contact_address contact_city contact_state
#>                   <char>              <char>       <char>        <char>
#> 1: Washington University 4950 Parkview Place  Saint Louis            MO
#> 2: Washington University 4950 Parkview Place  Saint Louis            MO
#> 3: Washington University 4950 Parkview Place  Saint Louis            MO
#> 4: Washington University 4950 Parkview Place  Saint Louis            MO
#> 5: Washington University 4950 Parkview Place  Saint Louis            MO
#> 6: Washington University 4950 Parkview Place  Saint Louis            MO
#>    contact_zip/postal_code contact_country
#>                     <char>          <char>
#> 1:                   63110             USA
#> 2:                   63110             USA
#> 3:                   63110             USA
#> 4:                   63110             USA
#> 5:                   63110             USA
#> 6:                   63110             USA
#>                                                                                                                       supplementary_file
#>                                                                                                                                   <char>
#> 1: ftp://ftp.ncbi.nlm.nih.gov/geo/samples/GSM1934nnn/GSM1934697/suppl/GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 2: ftp://ftp.ncbi.nlm.nih.gov/geo/samples/GSM1934nnn/GSM1934697/suppl/GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 3: ftp://ftp.ncbi.nlm.nih.gov/geo/samples/GSM1934nnn/GSM1934697/suppl/GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 4: ftp://ftp.ncbi.nlm.nih.gov/geo/samples/GSM1934nnn/GSM1934697/suppl/GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 5: ftp://ftp.ncbi.nlm.nih.gov/geo/samples/GSM1934nnn/GSM1934697/suppl/GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 6: ftp://ftp.ncbi.nlm.nih.gov/geo/samples/GSM1934nnn/GSM1934697/suppl/GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#>    data_row_count                            fixative:ch1 gender:ch1
#>            <char>                                  <char>     <char>
#> 1:             72 Formalin-Fixed Paraffin Embedded Tissue     female
#> 2:             72 Formalin-Fixed Paraffin Embedded Tissue     female
#> 3:             72 Formalin-Fixed Paraffin Embedded Tissue     female
#> 4:             72 Formalin-Fixed Paraffin Embedded Tissue     female
#> 5:             72 Formalin-Fixed Paraffin Embedded Tissue     female
#> 6:             72 Formalin-Fixed Paraffin Embedded Tissue     female
#>    tissue type:ch1 Header.header_FileVersion Header.header_SoftwareVersion
#>             <char>                    <char>                        <char>
#> 1:    Breast Tumor                       1.6                    2.1.1.0005
#> 2:    Breast Tumor                       1.6                    2.1.1.0005
#> 3:    Breast Tumor                       1.6                    2.1.1.0005
#> 4:    Breast Tumor                       1.6                    2.1.1.0005
#> 5:    Breast Tumor                       1.6                    2.1.1.0005
#> 6:    Breast Tumor                       1.6                    2.1.1.0005
#>    Sample_Attributes.sample_ID Sample_Attributes.sample_Owner
#>                         <char>                         <char>
#> 1:        LGWU101986-1036378WU                       palldred
#> 2:        LGWU101986-1036378WU                       palldred
#> 3:        LGWU101986-1036378WU                       palldred
#> 4:        LGWU101986-1036378WU                       palldred
#> 5:        LGWU101986-1036378WU                       palldred
#> 6:        LGWU101986-1036378WU                       palldred
#>    Sample_Attributes.sample_Comments Sample_Attributes.sample_Date
#>                               <char>                        <char>
#> 1:                      250 ng input                      20111102
#> 2:                      250 ng input                      20111102
#> 3:                      250 ng input                      20111102
#> 4:                      250 ng input                      20111102
#> 5:                      250 ng input                      20111102
#> 6:                      250 ng input                      20111102
#>    Sample_Attributes.sample_GeneRLF Sample_Attributes.sample_SystemAPF
#>                              <char>                             <char>
#> 1:                   RUO PAM50 C941                            n6_vDV1
#> 2:                   RUO PAM50 C941                            n6_vDV1
#> 3:                   RUO PAM50 C941                            n6_vDV1
#> 4:                   RUO PAM50 C941                            n6_vDV1
#> 5:                   RUO PAM50 C941                            n6_vDV1
#> 6:                   RUO PAM50 C941                            n6_vDV1
#>    Lane_Attributes.lane_ID Lane_Attributes.lane_FovCount
#>                     <char>                        <char>
#> 1:                       2                          1155
#> 2:                       2                          1155
#> 3:                       2                          1155
#> 4:                       2                          1155
#> 5:                       2                          1155
#> 6:                       2                          1155
#>    Lane_Attributes.lane_FovCounted Lane_Attributes.lane_ScannerID
#>                             <char>                         <char>
#> 1:                            1155                           DA26
#> 2:                            1155                           DA26
#> 3:                            1155                           DA26
#> 4:                            1155                           DA26
#> 5:                            1155                           DA26
#> 6:                            1155                           DA26
#>    Lane_Attributes.lane_StagePosition Lane_Attributes.lane_BindingDensity
#>                                <char>                              <char>
#> 1:                                  1                                0.30
#> 2:                                  1                                0.30
#> 3:                                  1                                0.30
#> 4:                                  1                                0.30
#> 5:                                  1                                0.30
#> 6:                                  1                                0.30
#>    Lane_Attributes.lane_CartridgeID Messages  CodeClass   Name   Accession
#>                              <char>   <char>     <char> <char>      <char>
#> 1:                  20111102-9741-1     NULL Endogenous  FOXA1 NM_004496.2
#> 2:                  20111102-9741-1     NULL Endogenous   EXO1 NM_006027.3
#> 3:                  20111102-9741-1     NULL Endogenous   CDH3 NM_001793.3
#> 4:                  20111102-9741-1     NULL Endogenous  BIRC5 NM_001168.2
#> 5:                  20111102-9741-1     NULL Endogenous  MKI67 NM_002417.2
#> 6:                  20111102-9741-1     NULL Endogenous  CCNE1 NM_001238.1
#>    Count     Date     ID    BD ScannerID StagePosition     CartridgeID   FoV
#>    <int>   <char> <char> <num>    <char>        <char>          <char> <num>
#> 1:  2845 20111102      2   0.3      DA26             1 20111102-9741-1   100
#> 2:   388 20111102      2   0.3      DA26             1 20111102-9741-1   100
#> 3:   115 20111102      2   0.3      DA26             1 20111102-9741-1   100
#> 4:   682 20111102      2   0.3      DA26             1 20111102-9741-1   100
#> 5:   775 20111102      2   0.3      DA26             1 20111102-9741-1   100
#> 6:   342 20111102      2   0.3      DA26             1 20111102-9741-1   100
#>        PCL   LoD      MC  MedC    PC01     PC02      PC03      PC04      PC05
#>      <num> <num>   <num> <num>   <num>    <num>     <num>     <num>     <num>
#> 1: 0.98857 15.46 2049.32   672 1.18781 4.599829 -1.525226 -2.133031 0.4390433
#> 2: 0.98857 15.46 2049.32   672 1.18781 4.599829 -1.525226 -2.133031 0.4390433
#> 3: 0.98857 15.46 2049.32   672 1.18781 4.599829 -1.525226 -2.133031 0.4390433
#> 4: 0.98857 15.46 2049.32   672 1.18781 4.599829 -1.525226 -2.133031 0.4390433
#> 5: 0.98857 15.46 2049.32   672 1.18781 4.599829 -1.525226 -2.133031 0.4390433
#> 6: 0.98857 15.46 2049.32   672 1.18781 4.599829 -1.525226 -2.133031 0.4390433
#>         PC06     PC07      PC08       PC09       PC10 Positive_factor
#>        <num>    <num>     <num>      <num>      <num>           <num>
#> 1: -1.422065 2.353212 -1.186074 0.03486358 -0.2710683       0.8074219
#> 2: -1.422065 2.353212 -1.186074 0.03486358 -0.2710683       0.8074219
#> 3: -1.422065 2.353212 -1.186074 0.03486358 -0.2710683       0.8074219
#> 4: -1.422065 2.353212 -1.186074 0.03486358 -0.2710683       0.8074219
#> 5: -1.422065 2.353212 -1.186074 0.03486358 -0.2710683       0.8074219
#> 6: -1.422065 2.353212 -1.186074 0.03486358 -0.2710683       0.8074219
#>    Negative_factor House_factor is_outlier Count_Norm
#>              <num>        <num>     <lgcl>      <num>
#> 1:        16.28558    0.6221572      FALSE       1421
#> 2:        16.28558    0.6221572      FALSE        187
#> 3:        16.28558    0.6221572      FALSE         50
#> 4:        16.28558    0.6221572      FALSE        334
#> 5:        16.28558    0.6221572      FALSE        381
#> 6:        16.28558    0.6221572      FALSE        164
```
