#2.1----
state.zero = 123456789
state.one = 462436069
state.two = 521288629
state.three = 88675123
state.four = 5783321
state.five = 6615241
state.seed = state.five
#'@description rgenerator is a function that produce a pseudo-random number.
#'@methodIt when called generate one float as random number. The input of this function is seed of it.
#'@usage rgenerator(seed)
#'@input Seed: a single value, interpreted as an integer, or NULL
#'@output random number
#'@example randomNumber = rgenerator(100)
#'@relatedFunctions int_to_unit()
#'@export
#'
rgenerator <- function(seed){
  if (seed != state.seed){
    state.five = seed
    state.seed = seed
  }
  t = (bitwXor(state.zero, bitwShiftR(state.zero, 2)))
  state.zero <<- state.one
  state.one <<- state.two
  state.two <<- state.three
  state.three <<- state.four
  state.four <<- bitwXor((bitwXor(state.four, (bitwShiftL(state.four, 4)))), bitwXor(t, bitwShiftL(t, 1)))
  state.five <<- state.five + 362437
  return(int_to_unit(state.five + state.four)/1000)
}
#'@description helping function for rgenerator
#'@export
#'
int_to_unit <- function (x, adjustment=2^32) {
  x <- as.numeric(x)
  signs <- sign(x)
  x[signs < 0] <- x[signs < 0] + adjustment
  x
}


#2.2.1----
#'@description Random generator for the uniform distribution in the interval [a, b]
#'@method It takes a random number that created from rgenerator and calculates this number mod (b) and then adds (a) to the result and returns it.
#'@usage dugen(a, b)
#'@input a: The start of interval , b: The end of interval
#'@output Generate a random number in uniform distribution in the interval [a, b]
#'@example random Number = dugen(10, 15)
#'@relatedFunctions rgenerator(seed)
#'@export
#'
dugen <- function(a, b) # generate uniform random varible in [a, b]
{
  return (rgenerator(state.seed) %% b + a)
}

#2.2.2----
#'@description Random generator for the uniform distribution in the interval [0, 1].
#'@method It returns dugen(0,1). That means a random number in the interval [0, 1] with uniform distribution
#'@usage cugen()
#'@input NULL
#'@output Generate a random number in uniform distribution in the interval [0, 1]
#'@example random Number = cugen()
#'@relatedFunctions dugen()
#'@export
#'
cugen <- function() # generate uniform random varible in [0, 1]
{
  return (dugen(0, 1))
}
#2.3----
#'@description Bernoulli distribution is one the most simple as well as famous distributions.
#'@method generate a random number and check it with par and return 0 or 1
#'@usage brgen(par)
#'@input probability of success(par)
#'@output {0 , 1}
#'@example random Number = brgen(0.31)
#'@relatedFunctions cugen()
#'@export
#'
brgen <- function(par){
  cu <- cugen()
  if(cu <= par) return (1)
  else return (0)
}
#2.4----
#'@description As mentioned in project description binomial RV can be seen as the result of repeated Bernouli RV.
#'@method So with input of number of trials as n,n times in loop Bernouli RV will be executed and sum of results will be saved in variable and returned.
#'@usage bigen(a,n)
#'@input a = probability parameter, n = number of trial
#'@output [0 , n]
#'@example random Number = bigen(0.41 , 10)
#'@relatedFunctions brgen(a)
#'@export
#'
bigen <- function(a, n){
  sum <-0
  for (i in 0:n) {
    sum <- sum + brgen(a)
  }
  return(sum)
}
#2.5--------------------------------------------------
#'@description  Random generation for the geometric distribution with parameter p
#'@method count number of failer to reach succes
#'@usage gegen(p)
#'@input p: probability of success in each trial. 0 < p <= 1.
#'@output Generate a random number in geometric distribution.
#'@example random Number = gegen(0.3)
#'@relatedFunctions brgen(p)
#'@export
#'
gegen <- function(p){
  number.of.failer = 0
  while(brgen(p) == 0){
    number.of.failer = number.of.failer + 1
  }
  return(number.of.failer)
}
#2.6----
#'@description Random generator for the exponential distribution with parameter (lambda).
#'@method At first it takes a random uniform number with cugen() function(we named it x). Then it returns ( -1 / lambda) * log(x). we know that it is an random exponential number with parameter (lambda).
#'@usage expgen(lambda)
#'@input lambda: value of 1 / mean
#'@output Generate a random number in exponential distribution with parameter (lambda).
#'@example randomNumber = expgen(2)
#'@relatedFunctions cugen()
#'@export
#'
expgen <- function(landa)
{
  return (- (1 / landa) * log(cugen()))
}
#2.7-------------------------------------------------
#'@description Random generator for the gamma distribution.
#'@method first it take two number first number is for exponentioal lamda and second is for number of try and we call exponentioal k times.
#'@usage gagen(par, k)
#'@input par = lamda in exponentioal and k is number of try
#'@output Generate a random number in gamma distribution.
#'@example randomNumber = gagen(2 , 34)
#'@relatedFunctions expgen()
#'@export
#'
gagen <- function(par , k){
  sum <- 0
  for (i in 1:k){
    sum <- sum + expgen(par)
  }
  return (sum)
}
#2.8-------------------------------------------------
#'@description 2.8:As mentioned in project description if exponential RV is modeled as the waiting time before an arrival, the poisson RV variable can be modeled as the number of arrivals during period of time t.
#'@method So we execute expgen for getting exponential varaible for n time as sum of returned exponentiall variables get bigger or equal to given input "t" and finally return "n".
#'@usage pogen(a , b)
#'@input a = lamda, b = total time
#'@output Generate a random number in poisson distribution.
#'@example randomNumber = expgen(2 , 23)
#'@relatedFunctions expgen(a)
#'@export
#'
pogen <- function(a, b){
  val <- 0
  sum <- 0
  while (TRUE) {
    sum <- sum + expgen(a)
    val <- val + 1
    if(sum > b){
      return(val)
    }
  }
}
#2.9-------------------------------------------------
#'@description Random generation for the normal distribution with mean equal to u and standard deviation equal to s.
#'@method generate a Poisson number and then use scale and transition to achieve the desired mean and variance
#'@usage nogen(u, s)
#'@input u : value of mean , s : value of standard devision
#'@output generate a random number in normal distribution.
#'@example randomNumber = nogen(10, 2)
#'@relatedFunctions pogen(0.1, 100)
#'@export
#'
nogen <- function(u, s){
  random.landa = 10
  random.z = pogen(0.1, 100)
  random.x = (sqrt(s / random.landa)) * (random.z) + (u - sqrt(random.landa * s))
  return(random.x)
}
#2.10-------------------------------------------------
#'@description plot uniform distribution
#'@method generate many uniform number and then use ggplot2 and plot it
#'@usage uniform_visualization(a,b)
#'@input uniform inputs
#'@output plot
#'@example uniform_visualization(1,100)
#'@relatedFunctions dugen(a, b)
#'@improt ggplot2
#'@export
#'
library(ggplot2)

uniform_visualization <- function(a, b)
{
  random_array = c()
  for (i in 1: 10000)
  {
    random_array[i] = dugen(a, b + 1)
  }
  data =  data.frame(x = random_array)
  m <- ggplot(data, aes(x=data$x)) + geom_histogram()
  print(m)
}
#'@description plot bernoulli distribution
#'@method generate many bernoulli number and then use ggplot2 and plot it
#'@usage bernoulli_visualization(par)
#'@input bernoulli inputs
#'@output plot
#'@example bernoulli_visualization(0.33)
#'@relatedFunctions brgen(par)
#'@improt ggplot2
#'@export
#'
bernoulli_visualization <- function(par)
{
  random_array = c()
  for (i in 1: 10000)
  {
    random_array[i] = brgen(par)
  }
  data =  data.frame(x = random_array)
  m <- ggplot(data, aes(x=data$x)) + geom_histogram()
  print(m)
}
#'@description plot binomial distribution
#'@method generate many binomial number and then use ggplot2 and plot it
#'@usage binomial_visualization(p , n)
#'@input binomial inputs
#'@output plot
#'@example binomial_visualization(1,100)
#'@relatedFunctions bigen(p,n)
#'@improt ggplot2
#'@export
#'
binomial_visualization <- function(p , n)
{
  random_array = c()
  for (i in 1: 10000)
  {
    random_array[i] = bigen(p,n)
  }
  data =  data.frame(x = random_array)
  m <- ggplot(data, aes(x=data$x)) + geom_histogram()
  print(m)
}
#'@description plot geometric distribution
#'@method generate many geometric number and then use ggplot2 and plot it
#'@usage geometric_visualization(p)
#'@input geometric inputs
#'@output plot
#'@example geometric_visualization(0.43)
#'@relatedFunctions gegen(p)
#'@improt ggplot2
#'@export
#'
geometric_visualization <- function(p)
{
  random_array = c()
  for (i in 1: 10000)
  {
    random_array[i] = gegen(p)
  }
  data =  data.frame(x = random_array)
  m <- ggplot(data, aes(x=data$x)) + geom_histogram()
  print(m)
}
#'@description plot exponential distribution
#'@method generate many exponential number and then use ggplot2 and plot it
#'@usage exponential_visualization(landa)
#'@input exponential inputs
#'@output plot
#'@example exponential_visualization(2)
#'@relatedFunctions expgen(landa)
#'@improt ggplot2
#'@export
#'
exponential_visualization <- function(landa)
{
  random_array = c()
  for (i in 1: 10000)
  {
    random_array[i] = expgen(landa)
  }
  data =  data.frame(x = random_array)
  m <- ggplot(data, aes(x=data$x)) + geom_histogram()
  print(m)
}
#'@description plot gamma distribution
#'@method generate many gamma number and then use ggplot2 and plot it
#'@usage gamma_visualization(par, k)
#'@input gamma inputs
#'@output plot
#'@example gamma_visualization(2,100)
#'@relatedFunctions gagen(par, k)
#'@improt ggplot2
#'@export
#'
gamma_visualization <- function(par, k)
{
  random_array = c()
  for (i in 1: 10000)
  {
    random_array[i] = gagen(par, k)
  }
  data =  data.frame(x = random_array)
  m <- ggplot(data, aes(x=data$x)) + geom_histogram()
  print(m)
}
#'@description plot poisson distribution
#'@method generate many poisson number and then use ggplot2 and plot it
#'@usage poisson_visualization(landa, t)
#'@input poisson inputs
#'@output plot
#'@example poisson_visualization(2, 4)
#'@relatedFunctions pogen(landa, t)
#'@improt ggplot2
#'@export
#'
poisson_visualization <- function(landa, t)
{
  random_array = c()
  for (i in 1: 10000)
  {
    random_array[i] = pogen(landa, t)
  }
  data =  data.frame(x = random_array)
  m <- ggplot(data, aes(x=data$x)) + geom_histogram()
  print(m)
}
#'@description plot normal distribution
#'@method generate many normal number and then use ggplot2 and plot it
#'@usage normal_visualization(u, s)
#'@input normal inputs
#'@output plot
#'@example normal_visualization(1,100)
#'@relatedFunctions nogen(u, s)
#'@improt ggplot2
#'@export
#'
normal_visualization <- function(u, s)
{
  random_array = c()
  for (i in 1: 10000)
  {
    random_array[i] = nogen(u, s)
  }
  bin = 30
  data =  data.frame(x = random_array)
  m <- ggplot(data, aes(x=data$x)) + geom_histogram()
  print(m)
}
