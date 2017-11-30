#  _       _                        _     
# (_)     | |                      | |    
#  _ _ __ | |_ ___ _ __ _ __   __ _| |___ 
# | | '_ \| __/ _ \ '__| '_ \ / _` | / __|
# | | | | | ||  __/ |  | | | | (_| | \__ \
# |_|_| |_|\__\___|_|  |_| |_|\__,_|_|___/
                                        
                                        
                                 
# internal helper functions
sub3dMat<-function(x,MARGIN,ind){
	if(MARGIN==1) out<-x[ind,,]
	if(MARGIN==2) out<-x[,ind,]
	if(MARGIN==3) out<-x[,,ind]
	out
}


# x is a 4D brain image
# mar, ind and time are scalar
array2long<-function(x,mask=!is.na(x[,,,1]),mar,ind,time=1,center_coords=TRUE){
	x_s<-sub3dMat(x[,,,time],mar,ind)
	mask_s<-sub3dMat(mask==1,mar,ind)
	coords<-which(mask_s,arr.ind=TRUE)
	if(center_coords) coords<-t(apply(coords,1,function(row_x) {row_x-dim(mask)[c(1:3)[-mar]]/2 }))
	value<-x_s[mask_s]
	out<-cbind(value,coords)
	out
}

facet_just_unique<-function(row_ind,col_ind){
	if( all(row_ind==row_ind[1]) &  all(col_ind==col_ind[1]))
		out<-NULL
	if( all(row_ind==row_ind[1]) & !all(col_ind==col_ind[1]))
		out<-facet_grid(.~col_ind)
	if(!all(row_ind==row_ind[1]) &  all(col_ind==col_ind[1]))
		out<-facet_grid(row_ind~.)
	if(!all(row_ind==row_ind[1]) & !all(col_ind==col_ind[1]))
		out<-facet_grid(row_ind~col_ind)

	return(out)
}

# ... is passed to getBrainFrame
getggTemplate<-function(col_template,row_ind,col_ind, ...){
	templateFrame<-getBrainFrame(row_ind=row_ind, col_ind=col_ind, ...)

	n<-length(col_template)
	if(n>1) col_cut<-as.numeric(cut(templateFrame$value,n))
	if(n==1) col_cut=1
	

	p<-ggplot()+facet_just_unique(row_ind,col_ind)

	for(i in 1:n){
		if(all(col_cut!=i)) next
		drop_ind<-which(names(templateFrame)=='value') #so it doesn't conflict with mappings to "value" later on
		templateFrame_col<-templateFrame[col_cut==i,-drop_ind]
		p<- p + geom_tile(data=templateFrame_col,aes(x=row,y=col),fill=col_template[i])
	}

	p
}

# (Now only an internal function)
# For creating ggplot line objects for use with \code{\link{ggBrain}}
#
# \code{getLinesFrame} creates long data frames which are transformed into line objects by code{ggLines}. \code{\link{ggBrain}} automatically calls \code{ggLines}. Alternatively, \code{getLinesFrame} can be used with \code{\link{getBrainFrame}}
# 
# @param row_ind should match the vector passed to \code{\link{ggBrain}} or \code{\link{getBrainFrame}}. 
# @param col_ind should match the vector passed to \code{\link{ggBrain}} or \code{\link{getBrainFrame}}.
# @param mar should match the vector passed to \code{\link{ggBrain}} or \code{\link{getBrainFrame}}.
# @param lines_mat a matrix with 3 columns, and one row for each line to be added to the brain images. Each row of \code{lines_mat} contains a triplet of values, with the first element telling the panel number where the line should be placed, the second element containing the margin to plot over, and the second element telling the slice index where the line should be placed.
# @param dim_image the dimension of the image template to plot lines on.
# @param center_coords should match the value passed to \code{\link{ggBrain}} or \code{\link{getBrainFrame}}.
# @param ... passed to \code{getLinesFrame}.
# @return
# \code{getLinesFrame} Returns two data frames, \code{h} and \code{v}, which can be use with \code{\link[ggplot2]{geom_hline}} and \code{\link[ggplot2]{geom_vline}} respectively. 
# @alias ggLines
getLinesFrame<-function(row_ind,col_ind,mar,lines_mat,dim_image=NULL,center_coords=FALSE){

	colnames(lines_mat)<-c('panel','mar','slice')

	long_lines<-data.frame(matrix(NA,nrow=nrow(lines_mat),ncol=4))
	colnames(long_lines)<-c('row_ind','col_ind','line_int','direction')
	long_lines[,'row_ind']<-row_ind[lines_mat[,'panel']]
	long_lines[,'col_ind']<-col_ind[lines_mat[,'panel']]
	long_lines[,'line_int']<-lines_mat[,'slice']

	if(center_coords) long_lines[,'line_int']<-lines_mat[,'slice']-dim_image[lines_mat[,'mar']]/2	

	if(any(mar[lines_mat[,'panel']]==lines_mat[,'mar'])) stop("Can't add lines the same margin that the panel is sliced from. Adjustment is needed for lines_mat argument")

	for(i in 1:nrow(lines_mat)){
		if(mar[lines_mat[i,'panel']]==1) vertical <- lines_mat[i,'mar']==2 #as opposed to 3
		if(mar[lines_mat[i,'panel']]==2) vertical <- lines_mat[i,'mar']==1 #as opposed to 3
		if(mar[lines_mat[i,'panel']]==3) vertical <- lines_mat[i,'mar']==1 #as opposed to 2
		long_lines[i,'direction']<-c('h','v')[vertical+1]
	}	

	long_lines
}


# ...no idea --t
#
ggLines<-function(color='white',...){
	
	lf<-getLinesFrame(...)
	
	out_h<-geom_hline(aes(yintercept=line_int),data=lf[lf$direction=='h',],col=color)
	out_v<-geom_vline(aes(xintercept=line_int),data=lf[lf$direction=='v',],col=color)

	if(all(lf$direction!='h')) out_h<-NULL
	if(all(lf$direction!='v')) out_v<-NULL

	return(list(v=out_v,h=out_h))
}





