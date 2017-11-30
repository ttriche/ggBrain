#' Possible theme settings for brain image plots
#'
#' \code{theme_black_bg} makes the background of the image black, \code{theme_no_ticks} removes tick marks from the axes, \code{theme_all_blank} removes additional aesthetic labels from the ggplot image, 
#' @export
#' @return
#' ggplot theme objects
#' @aliases theme_all_blank theme_no_ticks
#' @examples \dontrun{
#' library(oro.nifti)
#' library(ggplot2)
#' 
#' s_map1<-readNIfTI(system.file('seed_corr_1.nii.gz', package='ggBrain'))
#' template <- readNIfTI(system.file('template.nii.gz', package='ggBrain'))
#' 
#' dd<-ggBrain(brains=template,mask=template>0,
#'	mar=c(3,3),mar_ind=c(30,40),col_ind=c(1,2),
#'	type='structural',center_coords=FALSE)+
#'  scale_fill_continuous(low="black", high="white")
#' 
#' #without theme settings
#' dd
#' 
#' #with theme settings
#' dd + theme_black_bg()
#' dd + theme_black_bg() + theme_no_ticks()
#' dd + theme_black_bg() + theme_all_blank()
#'}
theme_black_bg<-function() theme(panel.background = element_rect(fill = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

#' @rdname theme_black_bg
#' @export
theme_all_blank<-function() theme(strip.background = element_blank(),strip.text.x = element_blank(),strip.text.y = element_blank(),axis.ticks = element_blank(),axis.text=element_blank())

#' @rdname theme_black_bg
#' @export
theme_no_ticks<-function() theme(axis.ticks = element_blank(),axis.text=element_blank())
