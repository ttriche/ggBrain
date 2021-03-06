#' Plotting brain images with ggplot
#'
#' \code{ggBrain} creates ggplot brain images with minimal user input. This function calls \code{getBrainFrame}, which generates a data frame for use in ggplot objects. Aesthetic changes to the resulting figures can be made with standard ggplot functions, such as \code{\link[ggplot2]{scale_fill_gradient2}}. The \code{getBrainFrame} function can also be directly accessed by the user, for a deeper control of aesthetics.
#' @param brains A list of 4-D arrays of brain images, with time on the fourth dimension. Alternatively, a list of 3-D arrays, a single 3-D, or a single 4-D array can be entered. 
#' @param mask A 3-D binary array of which voxels in the image to plot. If no mask is provided, the mask is set to be all voxels not equal to \code{NA}.
#' @param template A 3-D structural brain image over which to plot voxelwise statistics, such as p-values or seed correlations.
#' @param mar a numeric vector. The length of this vector should be equal to the number of panels in the figure. The \eqn{j^{th}} element of \code{mar} tells which margin of the image to slice over, for the \eqn{j^{th}} panel (see examples). 
#' @param mar_ind a numeric vector of the same length as \code{mar}.  The \eqn{j^{th}} element of \code{mar_ind} tells the slice number to be shown in the \eqn{j^{th}} panel. 
#' @param row_ind a numeric vector of the same length as \code{mar}, which partially determines the layout of the image plots.  The \eqn{j^{th}} element of \code{row_ind} tells the row number where the \eqn{j^{th}} panel should be positioned. 
#' @param col_ind a numeric vector of the same length as \code{mar}, which partially determines the layout of the image plots.  The \eqn{j^{th}} element of \code{row_ind} tells the column number where the \eqn{j^{th}} panel should be positioned.
#' @param brain_ind a vector of the same length as \code{mar}. The \eqn{j^{th}} element of \code{brain_ind} tells the index number of the appropriate brain image to be plotted in the \eqn{j^{th}} panel, from the list \code{brains}.
#' @param time a vector of the same length as \code{mar}, used for plotting snapshots of 4-D fMRI data over time. The \eqn{j^{th}} element of \code{time} tells the time index to be used for the \eqn{j^{th}} panel.
#' @param col_template a vector of colors to be used for displaying the template brain image.
#' @param type the type of brain image to be shown, either 'signed', 'positive', 'binary' or 'structural'. These types should be used when the objects in \code{brains} are, respectively, images of voxelwise statistics that can be positive or negative (e.g. seed correlations); images of voxelwise statistics that are all positive (e.g. p-values); binary masks to be shown on top of tempaltes; or structural brain images that do not contain voxelwise statistics. When setting \code{type='structural'} there is no need to input a template brain image.
#' @param binary_color the color to be used for plotting either binary masks or voxelwise, positive statistics (e.g. p-values). This argument will be used when \code{type} is set to either 'positive' or 'binary'. The color of 'signed' type figures can be changed using the standard ggplot functions for \code{fill} (see \code{\link[ggplot2]{scale_fill_gradient2}}).
#' @param combine_legend when \code{type} is set to \code{'signed'}, the default approach \code{ggBrain} takes is to map absolute value of the voxelwise statistic to alpha blending, and sign of the voxelwise statistic to fill. This results in two separate legends. When the \code{combine_legend} is set to TRUE, \code{ggBrain} creates a new custom scale that combines both binary coloring for sign, and gradual alpha blending for absolute value. Each voxel is binned according to it's value. The legend created shows the upper bounds for each bin. This argument is only used when \code{type} is set to \code{'signed'}.
#' @param breaks_legend the number of bins to use in creating a combined legend. This is only used when \code{combine_legend} is set to \code{TRUE}.
#' @param digits_legend the number of digits to be used in the combined legend for 'signed' plots. This is only used when \code{combine_legend} is set to \code{TRUE}.
#' @param signed_colors a vector of length two, with entries corresponding to the colors for negative and positive values of the overlaying voxelwise statistic. This argument is only used when \code{type} is set to \code{'signed'}.
#' @param fix_ratio whether the aspect ratio should be fixed, to avoid warping of images.
#' @param tri_planar a special mode for simultaneously plotting saggital, transverse, and coronal brain slices. If set to TRUE, a cross-hair will be plotted to show correspondance between these images (see examples).
#' @param lines_color color of the lines defined by \code{lines_mat}, or the lines defined by using \code{tri_planar=TRUE}.
#' @param center_coords whether the brains should be centered. If TRUE, axis tick marks will not be shown. If FALSE, axis tick marks will correspond to slice number
#' @param all_brain_and_time_inds_one a parameter specific to \code{getBrainFrame}, which can generally be ignored by the user. This arguments helps \code{getBrainFrame} be called from different contexts within \code{ggBrain}, particularly when contructing ggplot objects for the template brain. If set to \code{TRUE}, all elements of \code{brain_ind} and \code{time} will be forced to equal 1.
#' @param lines_mat a matrix with 3 columns, and one row for each line to be added to the figure. Each row of \code{lines_mat} contains a triplet of values, with the first element telling which panel the line should be placed in, the second element containing the margin to plot over, and the third element telling the slice index where the line should be placed. If \code{NULL}, no lines will be plotted.
#' @param  ... Parameters passed from \code{ggBrain} to \code{getBrainFrame}, such as \code{time} and \code{brain_ind}.
#'
#' 
#' @export
#' @import ggplot2 RColorBrewer
#' @return
#' \code{ggBrain} returns a ggplot object showing the desired brain images. Further aesthetic changes to the plotting can be added to this ggplot object using the usual ggplot notation (see examples). \code{getBrainFrame} outputs a "long" dataframe which can be used in ggplot objects. Accessing this dataframe directly allows the users to have more control over the plotting procedure hard-coded by \code{ggBrain}.
#' @aliases getBrainFrame 
#' @examples 
#' \dontrun{
#' 
#' 
#' #####################
#' # Load data
#' #####################
#' 
#' library(oro.nifti)
#' 
#' s_map1<-readNIfTI(system.file('seed_corr_1.nii.gz', package='ggBrain'))
#' s_map2<-readNIfTI(system.file('seed_corr_2.nii.gz', package='ggBrain'))
#' template <- readNIfTI(system.file('template.nii.gz', package='ggBrain'))
#' mask <- readNIfTI(system.file('brain_mask.nii.gz', package='ggBrain'))
#' seed_mask <- readNIfTI(system.file('seed_mask.nii.gz', package='ggBrain'))
#' nii1_trunc <- readNIfTI(system.file('subj_trunc_1.nii.gz', package='ggBrain'))
#' 
#' 
#' library(brainR)
#' 
#' hd_template <- readNIfTI(system.file("MNI152_T1_1mm_brain.nii.gz", package="brainR"))
#' 
#' 
#' 
#' 
#' 
#' #####################
#' # Generate plots
#' #####################
#' 
#' library(ggplot2)
#' 
#' ###############
#' # Simple examples, just one plot
#' 
#' # structural image (type = 'structural')
#' dd<-ggBrain(brains=hd_template,mask=hd_template>0,mar=3,mar_ind=93,type='structural')
#' dd
#' #now add aethetic changes with conventional ggplot code.
#' dd + scale_fill_continuous(low="black", high="white")+ theme_black_bg()
#' 
#' # seed correlation (type = 'signed')
#' dd<-ggBrain(template=template,brains=s_map1,mask=mask,mar=3,mar_ind=30,type='signed')
#' dd
#' 
#' # positive voxelwise statistics (type='positive').
#' # Here we use absolute value of seed correlation,
#' # but a p-value map might also be applied.
#' dd<-ggBrain(template=template,brains=abs(s_map1),mask=mask,mar=3,mar_ind=30,type='positive')
#' dd + theme_black_bg() + scale_alpha(range=c(0,1)) #note, for type='signed', a scale is already included
#' 
#' # Further customization can be done by using
#' # getBrainFrame directly
#' bf<-getBrainFrame(brains=hd_template,mar=3,mar_ind=93,mask=hd_template>0,center_coords=FALSE)
#' ggplot()+geom_tile(data=bf, aes(x=row,y=col,fill=value))+facet_grid(row_ind~col_ind)+
#' 		theme_black_bg()+labs(x='',y='')+coord_fixed(ratio = 1)
#' 
#' 
#' # tri_planar basic example, cross-hairs show correspondence across plots
#' dd<-ggBrain(brains=s_map1,template=template,
#' 	mar =		c(1,2,3),
#' 	mar_ind =	c(37,18,30),
#' 	row_ind=	c(1,1,2),
#' 	col_ind=	c(2,1,1),
#' 	tri_planar=TRUE, lines_color='black',mask=mask)
#' dd + theme_bw() +theme_no_ticks()
#' 
#' 
#' ###################
#' # use grid.arrange to show the seed mask and the
#' # seed correlation.
#' # Since these are on different scales, we use
#' # grid.arrange to show them separately.
#' mar   =   c(1,2,3)
#' col_ind = factor(c(1,2,3),labels=c('Saggital','Coronal','Transverse'))
#' row_ind = c(1,1,1)
#' mar_ind=  c(37,18,30)
#' 
#' dd_mask<-ggBrain(brains=seed_mask,template=template,mar=mar,mar_ind=mar_ind,row_ind=row_ind,
#' 		col_ind=col_ind,type='binary',binary_color='black',tri_planar=TRUE,mask=mask)+
#' 	labs(alpha='Seed mask')+theme_black_bg()
#' 
#' 
#' dd_1<-ggBrain(brains=s_map1,template=template,mar=mar,mar_ind=mar_ind,row_ind=row_ind,
#' 		col_ind=col_ind,tri_planar=TRUE,mask=mask) + theme_black_bg()
#' 
#' library(gridExtra)
#' grid.arrange(dd_mask,dd_1)
#' 
#' 
#' # We can also show two seed correlation maps
#' # from two different subjects. Note, these maps
#' # are on the same scale, as correlations are
#' # standardized
#' dd<-ggBrain(brains=list(s_map1,s_map2),
#' 	brain_ind=c(1,1,1,2,2,2),
#' 	template=template,
#' 	mar=c(1,2,3,1,2,3),
#' 	mar_ind=c(37,18,30,37,18,30),
#' 	row_ind=c('Subject 1','Subject 1','Subject 1','Subject 2','Subject 2','Subject 2'),
#' 	col_ind=factor(c(1,2,3,1,2,3),labels=c('Saggital','Coronal','Transverse')),
#' 	mask=mask)
#' dd + ggtitle('Seed correlations for two subjects')+ theme_black_bg()
#' 
#' 
#' 
#' ###################
#' # row_ind and col_ind can be used to look at
#' # several slices
#' 
#' # instead of inputting the mask directly, we
#' # can set the masked out voxels to NA
#' hd_template_masked<-hd_template
#' hd_template_masked[hd_template_masked==0]<-NA
#' 
#' dd<-ggBrain(brains=hd_template_masked,
#' 	mar=rep(3,8),
#' 	mar_ind=floor(seq(140,50,length=8)),
#' 	col_ind=c(1,1,1,1,2,2,2,2),
#' 	row_ind=c(1,2,3,4,1,2,3,4),
#' 	type='structural')
#' dd+ theme_black_bg()
#' 
#' 
#' 
#' # We can also add a key to the above type of plot,
#' # using the lines_mat argument
#' mar=c(3,3,3,3,3,3,3,1)
#' mar_ind=c(floor(seq(140,50,length=7)),88)
#' col_ind=c(1,1,1,1,2,2,2,2)
#' row_ind=c(1,2,3,4,1,2,3,4)
#' lines_mat<-cbind(8,mar,mar_ind)[1:7,]
#' 
#' dd<-ggBrain(brains=hd_template,mar=mar,mar_ind=mar_ind,row_ind=row_ind,col_ind=col_ind,
#' 		mask=hd_template>0,type='structural',lines_mat=lines_mat)
#' 
#' 
#' dd+ theme_black_bg()
#' 
#' 
#' 
#' # The same type of plots can be made for
#' # seed correlations
#' mar_ind=c(floor(seq(50,20,length=7)),30) #reduce dimensions
#' # to match fMRI data
#'
#' lines_mat<-cbind(8,mar,mar_ind)[1:7,]
#' 
#' dd<-ggBrain(brains=s_map1,template=template,mar=mar,mar_ind=mar_ind,row_ind=row_ind,
#' 		col_ind=col_ind,mask=mask,lines_mat=lines_mat)
#' 
#' dd + theme_black_bg()
#' 
#' 
#' 
#' # We can also plot fMRI activation over time
#' dd<-ggBrain(brains=nii1_trunc,template=template,
#' 	mask=mask,
#' 	mar=rep(3,4),
#' 	mar_ind=rep(30,4),
#' 	row_ind=rep(1,4),
#' 	col_ind=paste('time',1:4),
#' 	time=1:4)
#' dd + theme_black_bg()
#' 
#' # Note, to change legend labels for figures of type='signed',
#' # we must change both the label for fill and for alpha,
#' # as ggBrain combines these two aesthetics into a custom,
#' # hardcoded legend (when combine_legend=TRUE). If separate 
#' # labels are given for fill and alpha, the two aesthetic dimensions 
#' # will appear in separate legends.
#' # For example:
#' dd <- ggBrain(template=template,brains=s_map1,mask=mask,mar=3,mar_ind=30,type='signed')
#' # ex1:
#' dd + labs(fill='new_label',alpha='new_label')
#' # ex2:
#' dd + labs(fill='sign',alpha='abs_value')
#' }
ggBrain<-function(brains,template=NULL,mar=1,mar_ind,row_ind=rep(1,length(mar_ind)),col_ind=rep(1,length(mar_ind)),col_template=rev(brewer.pal(8,'Greys')),type='signed',binary_color='darkred',combine_legend=TRUE,breaks_legend=8,digits_legend=2,signed_colors=brewer.pal(9,'RdYlBu')[c(1,9)],fix_ratio=TRUE,tri_planar=FALSE,lines_color='white',center_coords=TRUE,lines_mat=NULL, ...){
	
	if(class(brains)!='list') brains<-list(brains) #also in getBrainFrame, but we need it here too.

	keyLines<-
	strip_axis_labels<-
	fix_ratio_gg<-

	if(fix_ratio){
		fix_ratio_gg <- coord_fixed(ratio = 1)
	}

	if(center_coords) strip_axis_labels<-theme_no_ticks()
	
	if(tri_planar){
		if(length(mar)!=3) stop('tri_planar only works for 3 images at once')
		if(length(unique(mar))<3) stop('mar must contain three unique margins')
		
		#don't center points here, centering will be handled by ggLines	
		cp<-mar_ind[mar] #central point

		lines_mat<-matrix(NA,6,3)
		colnames(lines_mat)<-c('panel','mar','slice')
		#need all 3x2 orderings of panel, and margin to plot a line on.
		lines_mat[1,]<-c(1,mar[3],cp[3])
		lines_mat[2,]<-c(1,mar[2],cp[2])
		lines_mat[3,]<-c(2,mar[3],cp[3])
		lines_mat[4,]<-c(2,mar[1],cp[1])
		lines_mat[5,]<-c(3,mar[2],cp[2])
		lines_mat[6,]<-c(3,mar[1],cp[1])
	}

	if(!is.null(lines_mat)){
		keyLines<-ggLines(row_ind=row_ind,col_ind=col_ind,mar=mar,lines_mat=lines_mat,dim_image=dim(brains[[1]])[1:3],center_coords=TRUE,color=lines_color)
	}


	
	if(type=='structural'){
		bf<-getBrainFrame(brains=brains, mar=mar,mar_ind=mar_ind,row_ind=row_ind,col_ind=col_ind,center_coords=center_coords,...)
		out<-ggplot()+geom_tile(data=bf, aes(x=row,y=col,fill=value))+facet_just_unique(row_ind,col_ind)
		return(out+labs(x='',y='')+fix_ratio_gg+strip_axis_labels+keyLines$v+keyLines$h)
	}
	#if no template, plot on a white background
	if(is.null(template)){
		template<-is.na(brains[[1]])
		col_template='white'
	}
	#get background colors
	ggTemplate<-getggTemplate(col_template=col_template,brains=template,all_brain_and_time_inds_one=TRUE,mar=mar,mar_ind=mar_ind,row_ind=row_ind,col_ind=col_ind,center_coords=center_coords,...)
	#get brain long data matrix
	brainFrame<-getBrainFrame(brains=brains,mar=mar,mar_ind=mar_ind,row_ind=row_ind,col_ind=col_ind,center_coords=center_coords,...)
	#plot values to colors or alpha
	if(type=='signed'){
		if(combine_legend){
			names(brainFrame)[names(brainFrame)=='value']<-'value_raw'
			seqVal<-seq(min(brainFrame$value_raw),max(brainFrame$value_raw),length=breaks_legend+1)
			brainFrame$Value<-cut(brainFrame$value_raw, breaks=seqVal,include.lowest=TRUE)
			levels(brainFrame$Value)<-signif(seqVal[-1],digits=digits_legend)#Display upper bounds for bins. If we round with signif before this step, we can get NAs if the max is larger than the rounded down max.
			seqCol<-signed_colors[(seqVal>0)+1]#binary colors, varied alpha
			out <-ggTemplate + 
				geom_tile(data=brainFrame, aes(x=row,y=col,fill=Value,alpha=Value)) +
				scale_alpha_manual(values=abs(seqVal[-1])/max(abs(seqVal))) + 
				scale_fill_manual(values=seqCol[-1])
		}
		if(!combine_legend)
			out<-ggTemplate+geom_tile(data=brainFrame, aes(x=row,y=col,fill=as.factor(sign_no_zero(value)),alpha=abs(value)))
	} 
	if(type=='positive'){
		 out<-ggTemplate+geom_tile(data=brainFrame, aes(x=row,y=col,alpha=value),fill=binary_color)
	} 
	if(type=='binary'){
		out<-ggTemplate+geom_tile(data=brainFrame, aes(x=row,y=col,alpha=as.factor(value)),fill=binary_color)
	}
	
	return(out+labs(x='',y='')+fix_ratio_gg+strip_axis_labels+keyLines$v+keyLines$h)
}


