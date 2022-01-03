makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  setting<<-function(y){
    x<<-y
    inv<<-NULL
  }
  getting<-function(){x}
  settingInv<-function(inverse){inv<<-inverse}
  gettingInv<-function(){inv}
  list(setting=setting,getting=getting,settingInv=settingInv,gettingInv=gettingInv)
  
}

cacheSolve<-function(x, ...){
  inv<-x$gettingInv()
  if(!is.null(inv)) {
    message('The cached data is')
    return(inv)
  }
  m<-x$getting()
  inv<-solve(m,...)
  x$settingInv(inv)
  inv
  
  
}