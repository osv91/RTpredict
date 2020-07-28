#this script creates piecewise regression and uses it to recalculate predictions
# in proj_set files rt - retention in Metlin (in seconds), RT - retention in specified conditions (in minutes)

library(segmented)
proj_set_dir = '../data/proj_sets/'
eval_set_dir = '../data/eval_sets/'

get_projection_model<-function(proj_set_file){
  basename = strsplit(proj_set_file, '.csv')[[1]]
  basename = strsplit(basename, 'cross_')[[1]][2]
  print(basename)
  
  proj_set = read.csv(proj_set_file, sep=';', stringsAsFactors = F)
  proj_set = proj_set[proj_set$rt>200,]
  y=proj_set$RT
  x=proj_set$rt
  data = list(x=c(x),y=c(y))
  
  model = segmented(lm(y~x, data=data),seg.Z = ~x)
  return (model)
}

get_projections<-function(model, file, output_dir){
  data = read.csv(file, sep=';')
  #data = data[data$Binary_retained >0.3,]
  input = data.frame(x = data$Predicted_RT)
  y = predict(model, input)
  data['Projected_RT'] = y
  output_name = strsplit(file, '/')[[1]]
  print(output_name)
  output_name = output_name[length(output_name)]
  print(output_name)
  output_name = sub('pred', '/proj', output_name)
  print(output_name)
  output = paste(output_dir, output_name, sep='')
  print(output)
  write.table(data, output,  sep=
               ';')
  return(data)
}

#eval_set_files='unique_RPFDAMM.csv'
eval_set_files=list.files('../data/eval_sets/')
for (file in eval_set_files){

  proj_set_file = paste(proj_set_dir, sub('unique', 'cross', file), sep='')
  basename = strsplit(proj_set_file, '.csv')[[1]]
  basename = strsplit(basename, 'cross_')[[1]][2]

  output_dir = paste('../data/projected/', sub('unique', 'proj', file), sep='')
  model = get_projection_model(proj_set_file)
  eval_set = read.csv(paste(eval_set_dir, file, sep=''), sep=';')
  name=''
  for (name in eval_set$Name){
    name = paste(name, '.csv', sep='')
    pred_dir = paste('../data/predictions/isomers/', basename, sep='')
    proj_dir = paste('../data/projections/isomers/', basename, sep='')
    name = paste('/pred_morg_', name, sep='')
    f = paste(pred_dir, name, sep='')
    get_projections(model, f, proj_dir)
    
  }
  
}

