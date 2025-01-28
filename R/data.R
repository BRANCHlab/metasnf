#' Mock ABCD anxiety data
#'
#' A randomly shuffled and anonymized copy of anxiety data from the NIMH Data
#'  archive. The original file used was pdem02.txt. The file was pre-processed
#'  by the abcdutils package (https://github.com/BRANCHlab/abcdutils) function
#'  `get_cbcl_anxiety`.
#'
#' @format ## `abcd_anxiety`
#' A data frame with 275 rows and 2 columns:
#' \describe{
#'     \item{patient}{The unique identifier of the ABCD dataset}
#'     \item{cbcl_anxiety_r}{Ordinal value of impairment on CBCL anxiety, either 0 (no impairment), 1 (borderline clinical), or 2 (clinically impaired)}
#' }
#' @source
#'  Though this data is no longer "real" ABCD data, the reference for using ABCD as a data source is below:
#'
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive DevelopmentSM (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
"abcd_anxiety"

#' Mock ABCD depression data
#'
#' A randomly shuffled and anonymized copy of depression data from the NIMH
#'  Data archive. The original file used was pdem02.txt. The file was
#'  pre-processed by the abcdutils package
#'  (https://github.com/BRANCHlab/abcdutils) function `get_cbcl_depress`.
#'
#' @format ## `abcd_depress`
#' A data frame with 275 rows and 2 columns:
#' \describe{
#'     \item{patient}{The unique identifier of the ABCD dataset}
#'     \item{cbcl_depress_r}{Ordinal value of impairment on CBCL anxiety, either 0 (no impairment), 1 (borderline clinical), or 2 (clinically impaired)}
#' }
#' @source
#'  Though this data is no longer "real" ABCD data, the reference for using ABCD as a data source is below:
#'
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive DevelopmentSM (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
"abcd_depress"


#' Mock ABCD subcortical volumes data
#'
#' A randomly shuffled and anonymized copy of subcortical volume data from the NIMH Data
#'  archive. The original file used was smrip10201.txt The file was pre-processed
#'  by the abcdutils package (https://github.com/BRANCHlab/abcdutils) function
#'  `get_subc_v`.
#'
#' @format ## `abcd_subc_v`
#' A data frame with 174 rows and 31 columns:
#' \describe{
#'     \item{patient}{The unique identifier of the ABCD dataset}
#'     \item{...}{Subcortical volumes of various ROIs (mm^3, I think)}
#' }
#' @source
#'  Though this data is no longer "real" ABCD data, the reference for using ABCD as a data source is below:
#'
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive DevelopmentSM (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
"abcd_subc_v"


#' Mock ABCD cortical thickness data
#'
#' A randomly shuffled and anonymized copy of cortical thickness data from the NIMH Data
#'  archive. The original file used was mrisdp10201.txt The file was pre-processed
#'  by the abcdutils package (https://github.com/BRANCHlab/abcdutils) function
#'  `get_cort_t`.
#'
#' @format ## `abcd_cort_t`
#' A data frame with 188 rows and 152 columns:
#' \describe{
#'     \item{patient}{The unique identifier of the ABCD dataset}
#'     \item{...}{Cortical thicknesses of various ROIs (mm^3, I think)}
#' }
#' @source
#'  Though this data is no longer "real" ABCD data, the reference for using ABCD as a data source is below:
#'
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive DevelopmentSM (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
"abcd_cort_t"


#' Mock ABCD cortical surface area data
#'
#' A randomly shuffled and anonymized copy of cortical surface area data from the NIMH Data
#'  archive. The original file used was mrisdp10201.txt The file was pre-processed
#'  by the abcdutils package (https://github.com/BRANCHlab/abcdutils) function
#'  `get_cort_t`.
#'
#' @format ## `abcd_cort_sa`
#' A data frame with 188 rows and 152 columns:
#' \describe{
#'     \item{patient}{The unique identifier of the ABCD dataset}
#'     \item{...}{Cortical surface areas of various ROIs (mm^2, I think)}
#' }
#' @source
#'  Though this data is no longer "real" ABCD data, the reference for using ABCD as a data source is below:
#'
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive DevelopmentSM (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
"abcd_cort_sa"

#' Mock ABCD income data
#'
#' A randomly shuffled and anonymized copy of income data from the NIMH Data
#'  archive. The original file used was pdem02.txt The file was pre-processed
#'  by the abcdutils package (https://github.com/BRANCHlab/abcdutils) function
#'  `get_income`.
#'
#' @format ## `abcd_income`
#' A data frame with 300 rows and 2 columns:
#' \describe{
#'     \item{patient}{The unique identifier of the ABCD dataset}
#'     \item{household_income}{Household income in 3 category levels (low = 1, medium = 2, high = 3)}
#' }
#' @source
#'  Though this data is no longer "real" ABCD data, the reference for using ABCD as a data source is below:
#'
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive DevelopmentSM (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
"abcd_income"

#' Mock ABCD income data
#'
#' Like abcd_income, but with no NAs in patient column
#'
#' @format ## `abcd_income`
#' A data frame with 300 rows and 2 columns:
#' \describe{
#'     \item{patient}{The unique identifier of the ABCD dataset}
#'     \item{household_income}{Household income in 3 category levels (low = 1, medium = 2, high = 3)}
#' }
#' @source
#'  Though this data is no longer "real" ABCD data, the reference for using ABCD as a data source is below:
#'
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive DevelopmentSM (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
"abcd_h_income"


#' Mock ABCD pubertal status data
#'
#' A randomly shuffled and anonymized copy of pubertal status data from the NIMH Data
#'  archive. The original files used were abcd_ssphp01.txt and abcd_ssphy01.txt. The file was pre-processed
#'  by the abcdutils package (https://github.com/BRANCHlab/abcdutils) function
#'  `get_pubertal_status`.
#'
#' @format ## `abcd_pubertal`
#' A data frame with 275 rows and 2 columns:
#' \describe{
#'     \item{patient}{The unique identifier of the ABCD dataset}
#'     \item{pubertal_status}{Average reported pubertal status between child and parent (1-5 categorical scale)}
#' }
#' @source
#'  Though this data is no longer "real" ABCD data, the reference for using ABCD as a data source is below:
#'
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive DevelopmentSM (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
"abcd_pubertal"

#' Mock ABCD "colour" data
#'
#' A randomly shuffled and anonymized copy of depression data from the NIMH
#'  Data archive. The original file used was pdem02.txt. The file was
#'  pre-processed by the abcdutils package
#'  (https://github.com/BRANCHlab/abcdutils) function `get_cbcl_depress`.
#'  The data was transformed into categorical colour values to demonstrate
#'  the Chi-squared test capabilities of `extend_solutions`.
#'
#' @format ## `abcd_colour`
#' A data frame with 275 rows and 2 columns:
#' \describe{
#'     \item{patient}{The unique identifier of the ABCD dataset}
#'     \item{colour}{Categorical transformation of `cbcl_depress`.}
#' }
#' @source
#'  Though this data is no longer "real" ABCD data, the reference for using ABCD as a data source is below:
#'
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive DevelopmentSM (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
"abcd_colour"

#' Mock ABCD anxiety data
#'
#' Like the mock data frame "abcd_colour", but with "unique_id" as the "uid".
#'
#' @format ## `anxiety`
#' A data frame with 275 rows and 2 columns:
#' \describe{
#'     \item{unique_id}{The unique identifier of the ABCD dataset}
#'     \item{cbcl_anxiety_r}{Ordinal value of impairment on CBCL anxiety, either 0 (no impairment), 1 (borderline clinical), or 2 (clinically impaired)}
#' }
#' @source
#'  Though this data is no longer "real" ABCD data, the reference for using ABCD as a data source is below:
#'
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive DevelopmentSM (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
"anxiety"

#' Mock ABCD depression data
#'
#' Like the mock data frame "abcd_depress", but with "unique_id" as the "uid".
#'
#' @format ## `depress`
#' A data frame with 275 rows and 2 columns:
#' \describe{
#'     \item{unique_id}{The unique identifier of the ABCD dataset}
#'     \item{cbcl_depress_r}{Ordinal value of impairment on CBCL anxiety, either 0 (no impairment), 1 (borderline clinical), or 2 (clinically impaired)}
#' }
#' @source
#'  Though this data is no longer "real" ABCD data, the reference for using ABCD as a data source is below:
#'
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive DevelopmentSM (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
"depress"


#' Mock ABCD subcortical volumes data
#'
#' Like the mock data frame "abcd_subc_v", but with "unique_id" as the "uid".
#'
#' @format ## `subc_v`
#' A data frame with 174 rows and 31 columns:
#' \describe{
#'     \item{unique_id}{The unique identifier of the ABCD dataset}
#'     \item{...}{Subcortical volumes of various ROIs (mm^3, I think)}
#' }
#' @source
#'  Though this data is no longer "real" ABCD data, the reference for using ABCD as a data source is below:
#'
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive DevelopmentSM (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
"subc_v"


#' Mock ABCD cortical thickness data
#'
#' Like the mock data frame "abcd_cort_t", but with "unique_id" as the "uid".
#'
#' @format ## `cort_t`
#' A data frame with 188 rows and 152 columns:
#' \describe{
#'     \item{unique_id}{The unique identifier of the ABCD dataset}
#'     \item{...}{Cortical thicknesses of various ROIs (mm^3, I think)}
#' }
#' @source
#'  Though this data is no longer "real" ABCD data, the reference for using ABCD as a data source is below:
#'
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive DevelopmentSM (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
"cort_t"


#' Mock ABCD cortical surface area data
#'
#' Like the mock data frame "abcd_cort_sa", but with "unique_id" as the "uid".
#'
#' @format ## `cort_sa`
#' A data frame with 188 rows and 152 columns:
#' \describe{
#'     \item{unique_id}{The unique identifier of the ABCD dataset}
#'     \item{...}{Cortical surface areas of various ROIs (mm^2, I think)}
#' }
#' @source
#'  Though this data is no longer "real" ABCD data, the reference for using ABCD as a data source is below:
#'
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive DevelopmentSM (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
"cort_sa"

#' Mock ABCD income data
#'
#' Like the mock data frame "abcd_h_income", but with "unique_id" as the "uid".
#'
#' @format ## `income`
#' A data frame with 300 rows and 2 columns:
#' \describe{
#'     \item{unique_id}{The unique identifier of the ABCD dataset}
#'     \item{household_income}{Household income in 3 category levels (low = 1, medium = 2, high = 3)}
#' }
#' @source
#'  Though this data is no longer "real" ABCD data, the reference for using ABCD as a data source is below:
#'
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive DevelopmentSM (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
"income"

#' Mock ABCD income data
#'
#' Like the mock data frame "abcd_cort_sa", but with "unique_id" as the "uid".
#'
#' @format ## `income`
#' A data frame with 300 rows and 2 columns:
#' \describe{
#'     \item{unique_id}{The unique identifier of the ABCD dataset}
#'     \item{household_income}{Household income in 3 category levels (low = 1, medium = 2, high = 3)}
#' }
#' @source
#'  Though this data is no longer "real" ABCD data, the reference for using ABCD as a data source is below:
#'
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive DevelopmentSM (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
"income"


#' Mock ABCD pubertal status data
#'
#' Like the mock data frame "abcd_pubertal", but with "unique_id" as the "uid".
#'
#' @format ## `pubertal`
#' A data frame with 275 rows and 2 columns:
#' \describe{
#'     \item{unique_id}{The unique identifier of the ABCD dataset}
#'     \item{pubertal_status}{Average reported pubertal status between child and parent (1-5 categorical scale)}
#' }
#' @source
#'  Though this data is no longer "real" ABCD data, the reference for using ABCD as a data source is below:
#'
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive DevelopmentSM (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
"pubertal"

#' Mock ABCD "colour" data
#'
#' Like the mock data frame "abcd_colour", but with "unique_id" as the "uid".
#'
#' @format ## `fav_colour`
#' A data frame with 275 rows and 2 columns:
#' \describe{
#'     \item{unique_id}{The unique identifier of the ABCD dataset}
#'     \item{colour}{Categorical transformation of `cbcl_depress`.}
#' }
#' @source
#'  Though this data is no longer "real" ABCD data, the reference for using ABCD as a data source is below:
#'
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive DevelopmentSM (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
"fav_colour"














#' Modification of SNFtool mock data frame "Data1"
#'
#' @format ## `expression_df`
#' A data frame with 200 rows and 3 columns:
#' \describe{
#'     \item{gene_1_expression}{Mock gene expression feature}
#'     \item{gene_2_expression}{Mock gene expression feature}
#'     \item{patient_id}{Random three-digit number uniquely identifying the patient}
#' }
#' @source
#' This data came from the SNFtool package, with slight modifications.
"expression_df"

#' Modification of SNFtool mock data frame "Data2"
#'
#' @format ## `methylation_df`
#' A data frame with 200 rows and 3 columns:
#' \describe{
#'     \item{gene_1_expression}{Mock gene methylation feature}
#'     \item{gene_2_expression}{Mock gene methylation feature}
#'     \item{patient_id}{Random three-digit number uniquely identifying the patient}
#' }
#' @source
#' This data came from the SNFtool package, with slight modifications.
"methylation_df"

#' Mock gender data
#'
#' @format ## `gender_df`
#' A data frame with 200 rows and 2 columns:
#' \describe{
#'     \item{patient_id}{Random three-digit number uniquely identifying the patient}
#'     \item{gender_df}{Mock gene methylation feature}
#' }
#' @source
#' This data came from the SNFtool package, with slight modifications.
"gender_df"

#' Mock diagnosis data
#'
#' This is the same data as `cancer_diagnosis_df`, with renamed features and columns.
#'
#' @format ## `diagnosis_df`
#' A data frame with 200 rows and 2 columns:
#' \describe{
#'     \item{patient_id}{Random three-digit number uniquely identifying the patient}
#'     \item{diagnosis}{Mock diagnosis feature}
#' }
#' @source
#' This data came from the SNFtool package, with slight modifications.
"diagnosis_df"

#' Mock diagnosis data
#'
#' This is the same data as `diagnosis_df`, with renamed features and columns.
#'
#' @format ## `cancer_diagnosis_df`
#' A data frame with 200 rows and 2 columns:
#' \describe{
#'     \item{patient_id}{Random three-digit number uniquely identifying the patient}
#'     \item{diagnosis}{Mock cancer diagnosis feature (1, 2, or 3)}
#' }
#' @source
#' This data came from the SNFtool package, with slight modifications.
"cancer_diagnosis_df"

#' Mock age data
#'
#' @format ## `age_df`
#' A data frame with 200 rows and 2 columns:
#' \describe{
#'     \item{patient_id}{Random three-digit number uniquely identifying the patient}
#'     \item{age}{Mock age feature}
#' }
#' @source
#' This data came from the SNFtool package, with slight modifications.
"age_df"
