install.packages("MASS")
library(MASS)

set.seed(123) 
n=100 #no. of rows of data

#Apartment characteristic
#size in sq.feet
size=rnorm(n,mean = 1000,sd=200) 

#Selling value
y=100000+200*size+rnorm(n,mean = 0,sd=1)

data=data.frame(
  x=size,
  y=y
)
head(data)

#fitted model
x=size
model=lm(y~x)
summary(model)
plot(x,y,type = "p",xlab = "Apartment Size (in Sq.feet)",ylab = "Selling Value (in Rs.)")

#Algorithm 1
beta=round(coef(model),2); beta

#Two regression coefficients (intercept and slope)
min_intercept = 90000  # Minimum value for intercept
max_intercept = 110000 # Maximum value for intercept
min_slope = 180    # Minimum value for slope
max_slope = 220      # Maximum value for slope

# Strategy space for player 1
H1=data.frame(intercept=seq(min_intercept,max_intercept,by=200),
              slope=seq(min_slope,max_slope,by=0.4))

# Strategy space for player 2
H2=data.frame(intercept=seq(95000,115000,by=200),
              slope=seq(190,230,by=0.4))

#payoff function
payoff_function=function(x,y,player1_strategy,player2_strategy,tolerance){
  #Predicted values for both players
  predicted_y1 = player1_strategy[1] + player1_strategy[2] * x
  predicted_y2 = player2_strategy[1] + player2_strategy[2] * x
  #Initialize payoffs to 0
  player1_payoff = 0
  player2_payoff = 0
  #Check if both players predict correctly (within tolerance)
  if (abs(y - predicted_y1) <= tolerance & abs(y - predicted_y2) <= tolerance) {
    player1_payoff = 0.5
    player2_payoff = 0.5
  } else {  
    #Check if at least one player predicts correctly
    if (abs(y - predicted_y1) <= tolerance) {
      player1_payoff = 1
    }
    if (abs(y - predicted_y2) <= tolerance) {
      player2_payoff = 1
    }
  }
  #Return a list containing payoffs for both players
  return(list(player1_payoff = player1_payoff, player2_payoff = player2_payoff))
}

#illustration of payoff function
p1_strategy=c(100000,200)
p2_strategy=c(110000,220)
payoff_function(900,280000,p1_strategy,p2_strategy,20000)

data=as.matrix(data)
H1=as.matrix(H1)
H2=as.matrix(H2)

# Initialize empty matrices for player payoffs
A = matrix(nrow = 100, ncol = 100)
B = matrix(nrow = 100, ncol = 100)
head(data)

#defining tolerance (10 percent)
tolerance = 0.1*data[,2]

#constructing payoff matrices
for (i in 1:100) {
  for(j in 1:100){
    #defining tolerance (10 percent)
    tolerance = 0.1*data[j,2]
    A[i,j]=payoff_function(data[j,1],data[j,2],H1[i,],H2[j,],tolerance)$player1_payoff
    B[i,j]=payoff_function(data[j,1],data[j,2],H1[i,],H2[j,],tolerance)$player2_payoff
    j=j+1
  }
  i=i+1
}
dim(A)
dim(B)


#epsilon-better response dynamics
epsilon_brd=function(pay_matrix_1,pay_matrix_2,epsilon=0.005,max_iter=1000){
  n=nrow(pay_matrix_1)
  m=nrow(pay_matrix_2)
  # Initialize strategies randomly(choosing index as strategies)
  h1i=sample(1:n, 1)
  h2i=sample(1:m, 1)
  
  for(iter in 1:max_iter) {
    #Player 1's best response
    #the row indices in A where the payoffs against Player 2's 
    #current strategy y are within epsilon of the maximum payoff in that column
    best_responses_h1i = which(A[, h2i] >= (max(A[, h2i]) - epsilon))
    new_h1i = sample(best_responses_h1i, 1)
    
    # Player 2's best response
    best_responses_h2i <- which(B[h1i, ] >= (max(B[h1i, ]) - epsilon))
    new_h2i = sample(best_responses_h2i, 1)
    
    # Check for convergence
    if (new_h1i == h1i && new_h2i == h2i) {
      break
    }
    
    # Update strategies
    h1i = new_h1i
    h2i = new_h2i
  }
  list(player1_strategy = h1i, player2_strategy = h2i)
}

# Run epsilon-BRD
result = epsilon_brd(A, B)
print(result)
i1=result$player1_strategy
i2=result$player2_strategy
#PNE
pne=c(H1[i1,],H2[i2,]);pne
pfs=c(A[i1,i2],B[i1,i2]);pfs #corresponding payoffs













