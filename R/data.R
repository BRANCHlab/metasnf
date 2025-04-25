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
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive Development (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
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
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive Development (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
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
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive Development (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
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
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive Development (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
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
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive Development (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
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
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive Development (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
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
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive Development (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
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
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive Development (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
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
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive Development (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
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
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive Development (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
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
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive Development (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
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
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive Development (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
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
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive Development (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
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
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive Development (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
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
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive Development (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
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
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive Development (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
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
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive Development (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
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
#' Data used in the preparation of this article were obtained from the Adolescent Brain Cognitive Development (ABCD) Study (https://abcdstudy.org), held in the NIMH Data Archive (NDA). This is a multisite, longitudinal study designed to recruit more than 10,000 children age 9-10 and follow them over 10 years into early adulthood. The ABCD Study® is supported by the National Institutes of Health and additional federal partners under award numbers U01DA041048, U01DA050989, U01DA051016, U01DA041022, U01DA051018, U01DA051037, U01DA050987, U01DA041174, U01DA041106, U01DA041117, U01DA041028, U01DA041134, U01DA050988, U01DA051039, U01DA041156, U01DA041025, U01DA041120, U01DA051038, U01DA041148, U01DA041093, U01DA041089, U24DA041123, U24DA041147. A full list of supporters is available at https://abcdstudy.org/federal-partners.html. A listing of participating sites and a complete listing of the study investigators can be found at https://abcdstudy.org/consortium_members/. ABCD consortium investigators designed and implemented the study and/or provided data but did not necessarily participate in the analysis or writing of this report. This manuscript reflects the views of the authors and may not reflect the opinions or views of the NIH or ABCD consortium investigators.
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

#' Cached example solutions data frame
#'
#' An solutions data frame used as a cached example in the "a_complete_example.Rmd" vignette.
#'
#' @format ## `cache_a_complete_example_sol_df`
#' A solutions data frame with 20 cluster solutions and 87 observations.
#' @source
#' This data came from the metasnf package.
"cache_a_complete_example_sol_df"

#' Cached example extended solutions data frame
#'
#' An extended solutions data frame used as a cached example in the "a_complete_example.Rmd" vignette.
#'
#' @format ## `cache_a_complete_example_ext_sol_df`
#' Contains 20 cluster solutions, 87 observations, and p-values for 336 features.
#' @source
#' This data came from the metasnf package.
"cache_a_complete_example_ext_sol_df"

#' Cached example extended solutions data frame
#'
#' An extended solutions data frame used as a cached example in the "a_complete_example.Rmd" vignette.
#'
#' @format ## `cache_a_complete_example_lp_ext_sol_df`
#' Contains 5 cluster solutions, 74 observations, and p-values for 2 features.
#' @source
#' This data comes from the metasnf package.
"cache_a_complete_example_lp_ext_sol_df"

#' Mock example of an `ari_matrix` metasnf object
#'
#' An `ari_matrix` class object containing adjusted Rand indices (ARIs) between 20 cluster solutions.
#' Used as an example of an `ari_matrix` metasnf object.
#'
#' @format ## `mock_ari_matrix`
#' A 20 by 20 ARI matrix.
#' @source
#' This data comes from the metasnf package.
"mock_ari_matrix"

#' Mock example of a `data_list` metasnf object
#'
#' @format ## `mock_data_list`
#' A data list containing 4 data frames with 100 observations each:
#'     - subcortical volume (30 features)
#'     - cortical surface area (151 features)
#'     - household income (1 feature)
#'     - pubertal status (1 feature)
#' Used as an example of an `data_list` metasnf object.
#' @source
#' This data comes from the metasnf package.
"mock_data_list"

#' Mock example of a `snf_config` metasnf object
#'
#' @format ## `mock_snf_config`
#' An SNF config containing hyperparameters and functions defined for generating 20 cluster solutions from a data list.
#' The config has been specified to:
#'     - limit the `k` hyperparameter to 40
#'     - make use of uniformly distributed random weights
#'     - randomly select between using spectral clustering where the number of clusters can be 2, 5, decided by the eigen-gap heuristic, or decided by the rotation cost heuristic
#'     - use Gower distance for categorical and mixed data, Euclidean distance for ordinal data, and randomly select from Euclidean distance or standard/normalized Euclidean distance for continuous and discrete data
#' The config was built using the `mock_data_list` loaded into the namespace after calling `library("metasnf")`.
#' Used as an example of an `snf_config` metasnf object.
#' @source
#' This data comes from the metasnf package.
"mock_snf_config"

#' Mock example of a `solutions_df` metasnf object
#'
#' @format ## `mock_solutions_df`
#' A solutions data frame containing 20 cluster solutions generated from `mock_snf_config` and `mock_data_list`.
#' Used as an example of an `solutions_df` metasnf object.
#' @source
#' This data comes from the metasnf package.
"mock_solutions_df"

#' Mock example of a `t_solutions_df` metasnf object
#'
#' @format ## `mock_t_solutions_df`
#' A transposed solutions data frame containing 20 cluster solutions generated from `mock_solutions_df`.
#' Used as an example of a `t_solutions_df` metasnf object.
#' @source
#' This data comes from the metasnf package.
"mock_t_solutions_df"

#' Mock example of a `clust_fns_list` metasnf object
#'
#' @format ## `mock_clust_fns_list`
#' A `clust_fns_list` object containing two clustering functions covering 2 and 5 five cluster solution versions of spectral clustering.
#' Extracted from `mock_snf_config`.
#' @source
#' This data comes from the metasnf package.
"mock_clust_fns_list"

#' Mock example of a `dist_fns_list` metasnf object
#'
#' @format ## `mock_dist_fns_list`
#' A `dist_fns_list` object containing a variety of distance metrics.
#' Extracted from `mock_snf_config`.
#' @source
#' This data comes from the metasnf package.
"mock_dist_fns_list"

#' Mock example of a `settings_df` metasnf object
#'
#' @format ## `mock_settings_df`
#' Settings for 20 cluster solutions.
#' @source
#' This data comes from the metasnf package.
"mock_settings_df"

#' Mock example of a `weights_matrix` metasnf object
#'
#' @format ## `mock_weights_matrix`
#' A `weights_matrix` class object containing 20 sets of weights for 183 features.
#' @source
#' This data comes from the metasnf package.
"mock_weights_matrix"

#' Mock example of a `mc_solutions_df` metasnf object
#'
#' @format ## `mock_mc_solutions_df`
#' A meta cluster labeled solutions data frame derived from `mock_solutions_df`.
#' Contains 20 cluster solutions.
#' @source
#' This data comes from the metasnf package.
"mock_mc_solutions_df"

#' Mock example of a `rep_solutions_df` metasnf object
#'
#' A `solutions_df` class object derived by filtering the `mock_mc_solutions_df` to its representative solutions.
#'
#' @format ## `mock_rep_solutions_df`
#' Contains 4 cluster solutions.
#' @source
#' This data comes from the metasnf package.
"mock_rep_solutions_df"

#' Mock example of a `ext_solutions_df` metasnf object
#'
#' An `ext_solutions_df` class object generated by extending the `mock_rep_solutions_df` object against `mock_data_list` as the target data list.
#'
#' @format ## `mock_ext_solutions_df`
#' Contains 20 cluster solutions.
#' @source
#' This data comes from the metasnf package.
"mock_ext_solutions_df"
