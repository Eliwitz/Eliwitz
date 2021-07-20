######Torus#######


# Function to set up the grid


dimensions=c(40, 60)
fracvac=0.1

createGrid <- function(dimensions, fracvac){
  population=numeric(dimensions[1]*dimensions[2])+1
  for (i in 1:length(population)*fracvac){
    population[i]=2
  }
  fitnesses=numeric(length(population))+1
  gridvec = sample(population, length(population), FALSE, fitnesses)
  output=array(gridvec,dimensions)
  return(output)
}


# Test code
# Should create a 6x4 grid with 18 2s and six 1s randomly distributed
print(createGrid(c(6,4), 0.75))



# Put a 0 in a 40X60 grid that you make with createGrid().
# No test code for this one.

begininfection <- function(thepopulation) {
  #thepopulation=(createGrid(dimensions, fracvac))
  go = TRUE
  while(go){
    infectedguy=runif(1, 1,length(thepopulation)+1)
    if (thepopulation[floor(infectedguy)]==1){
      thepopulation[floor(infectedguy)]=0
      go = FALSE
      
    }
  }
  return(thepopulation)
}




# Code to calculate the fraction of infected individuals at the end of the simulation
# This doesn't have to be a function.
# Either way, store the fraction in a variable called fractionInfected.

getfractionInfected <- function(thepopulation)
{
  infectedpop=0
  for (i in 1:length(thepopulation)){
    if (thepopulation[i] == 0) 
    {
      infectedpop=infectedpop+1
      
    }
  }
  thefracInfected=infectedpop/length(thepopulation)
  return(thefracInfected)
}
# Function to check whether two arrays are identical


areIdentical <- function (grid1, grid2){
  e=TRUE
  if (length(grid1[1, ]) == length(grid2[1, ])){
    for (i in 1:length(grid1))
    {
      if (grid1[i] != grid2[i]) {
        e=FALSE
      }
    }
  }else {
    e=FALSE
  }
  return(e)
}


Print( "Activity 14")

findNeightortoise <- function(thegrid, posi){
  
  height = length(thegrid[,1])
  width = length(thegrid[1,])
  
  north = c((posi[1]-1+height) %% height, posi[2]) 
  if (north[1]==0){
    north[1] = height
  }
  west = c(posi[1], (posi[2]-1+width) %% width)
  if (west[2]==0){
    west[2] = width
  }
  east = c(posi[1], (posi[2]+1+width) %% width)
  if (east[2]==0){
    east[2] = width
  }
  south= c((posi[1]+1+height) %% height, posi[2])
  if (south[1]==0){
    south[1] = height
  }
  tortoiseneighbors=rbind(north, west, east, south)
  return(tortoiseneighbors)
}

getsInfected2 <- function(thepopulation, dimensions)
{
  e=FALSE
  if (thepopulation[dimensions[1], dimensions[2]] == 1){
    potentinfectors=findNeightortoise(thepopulation, dimensions)
    for(i in 1:length(potentinfectors[ ,1])){
      testindexes=potentinfectors[i, ]
      #index1 = potentinfectors[i, ][1]
      #index2 = potentinfectors[i, ][2]
      if(thepopulation[testindexes[1], testindexes[2]] == 0){
        e=TRUE
        break
      }
    }
  }
  return(e)
}




#actual torus simulation

initialpop=(createGrid(c(60,60), 0.2))
nextpop=begininfection(initialpop)
spreadattempts=0
while (areIdentical(nextpop, initialpop) == FALSE){
  initialpop=nextpop
  if (spreadattempts > 1000){
    break
  }
  for (i in 1:length(initialpop[ ,2])){
    for (j in 1:length(initialpop[1, ])){
      if (initialpop[i,j]==0){} 
      #print(c(i,j)) }
      if (getsInfected2(initialpop,c(i,j)) == TRUE){
        nextpop[i,j]=0
      }
    }
  }
  spreadattempts=spreadattempts+1
  image(1:length(initialpop[ ,2]), 1:length(initialpop[1, ]), nextpop, xlab="population rows",ylab="population columns")
  Sys.sleep(0.1)
}


fractionInfected=(getfractionInfected(nextpop))
print(fractionInfected)