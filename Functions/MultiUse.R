extract<-function(x,y=input){
  y[[as.character(x)]]
}#used instead of input$x if x = a character string created using paste()

Contains<-function(x,y){
  if(length(x)>1&length(y)>1){
    z<-0
    for(i in 1:length(y)){
      z<-z+sum(x==y[i])
      if(z>0){break}}
    z>0
  }else{sum(x==y)>0}
}#returns true/false if x contains the value y

FreeFix<-function(txt){
  txt<-gsub(",","_",txt)
  txt<-gsub(":",".",txt)
  txt<-gsub("/"," or ",txt)
  txt
}#punctuation fix for free text(textInput),removes commas, colons and slashes

AddTo<-function(x,y){
  if(!(exists(x))){out<-y
  }else{out<-c(get(x),y)}
  out}#if x exists will add y to if not will make new object containing y

IfHassOther<-function(x,y){
  if(Contains(x,"Other")){
    out<-c(x[x!="Other"],FreeFix(y))
  }else{out<-x}
  out}# if x contains "Other" will add y and remove other

IfIsOther<-function(x,y){
  if(x=="Other"){
    out<-FreeFix(y)
  }else{out<-x}
  out}# if x equals "other" will return y else will return x
