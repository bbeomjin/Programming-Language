my_factorial = function(x)
{
  result = 1
  for (i in 1:x) {
    result = i * result
  }
  return(result)
}

my_factorial2 = function(x) 
{
  if (x <= 1) {
    return(x)
  }
  return(x * my_factorial2(x - 1))
}

my_factorial2(5)

gcd = function(a, b)
{
  d = min(a, b)
  cond = 1
  
  while(cond) {
    if ((a %% d == 0) & (b %% d == 0)) {
      cond = 0
      result = d
    }
    d = d - 1
  }
  
  return(result)
}

gcd2 = function(a, b)
{
  if (b == 0) {
    return(a)
  }
  return(gcd2(b, a %% b))
}

gcd(6, 12)
gcd(5, 3)

gcd(6, 12)
gcd2(a = 8, b = 6)

hanoi = function(n, from, to, aux)
{
  if (n == 1) {
    cat(from, "->", to, "\n")
    return(NULL)
  }
  
  hanoi(n - 1, from, aux, to)
  cat(from, "->", to, "\n")
  
  hanoi(n - 1, aux, to, from)
}

hanoi(3, 1, 3, 2)


