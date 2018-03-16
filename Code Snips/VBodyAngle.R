VBodyAngle<-function(PH,AH,L){
    #calculates the angle of sloping of a vertebral body using the posterior body height, anterior body height and body length
  require(aspace)
  atan_d(PH/((0-PH)/((AH-PH)/L)))
}

