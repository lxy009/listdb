

data.list <- function(x) UseMethod('data.list')

data.list.list <- function(x){
  return(structure(x,class = c('data.list', 'list')))
}

temp = function(x){
  tmp = substitute(x)
  tmp1 = paste0('y$',tmp[[2]],tmp[[1]],tmp[[3]],collapse="")
  eval(parse(text= paste0("to_ret = function(y){",tmp1,"}")))
  return(to_ret)
}


#filter is a bad key word
filter_dl <- function(x, y){
  tmp = substitute(y)
  tmp1 = paste0('tmp_fnc = function(z){z$',tmp[[2]],tmp[[1]],tmp[[3]],"}",collapse="")
  # cat(tmp1)
  eval(parse(text = tmp1))
  return(x[sapply(x, FUN = function(q){tmp_fnc(q)})])
  # return(tmp_fnc)
}


test1 = list(list('a'=1,'b'=2,'c'=3),
             list('a'=4,'b'=5,'c'=6))
test2 = list(list('a'=1,'b'=2,'c'=list('x'=1,'y'=2,'z'=3)),
             list('a'=4,'b'=5,'c'=list('x'=1,'y'=2,'z'=3)))