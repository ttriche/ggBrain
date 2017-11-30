#' @rdname ggBrain
#' @export
getBrainFrame<-function(brains,mask=NULL,mar=1,mar_ind,row_ind=rep(1,length(mar_ind)),col_ind=rep(1,length(mar_ind)),brain_ind=rep(1,length(mar_ind)),all_brain_and_time_inds_one=FALSE,time=rep(1,length(mar_ind)),center_coords=FALSE){

	if(length(unique( length(row_ind),length(col_ind),length(mar),length(mar_ind), length(brain_ind),length(time) ))>1) stop('row_ind, col_ind, mar, mar_ind, brain_ind, and time arguments must all be of the same length.')

	row_ind<-as.factor(row_ind)
	col_ind<-as.factor(col_ind)

	out<-c()
	if(class(brains)!='list') brains<-list(brains)
	if(all_brain_and_time_inds_one){
		brain_ind<-
		time<-rep(1,length(mar_ind))
	}
	for(i in 1:length(brains)) if(length(dim(brains[[i]]))==3)
		brains[[i]]<- array(brains[[i]],dim=c(dim(brains[[i]]),1))

	if(is.null(mask)) mask<- !is.na(brains[[1]][,,,1])


	#have to make it a data frame asap so you can add row_ind as a factor or string, and have it not turn the whole array into strings which eventually become all factors and unplottable.
	for(i in 1:length(mar)){
		t_i<-data.frame(array2long(brains[[brain_ind[i] ]],mask,mar[i],mar_ind[i],time=time[i],center_coords=center_coords))
		t_i_lab<-cbind(t_i,'row_ind'=row_ind[i],'col_ind'=col_ind[i])
		out<-rbind(out,t_i_lab)
	}

	out
}


