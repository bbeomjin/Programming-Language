# Midterm solution

problem1 = function(seed, n)
{
  set.seed(seed)
  k = 0
  
  for (i in 1:n) {
    toss = sample(c(0, 1), 1)
    if (toss == 1) {
      k = k + 1
    } else {
      k = 0
    }
    if (k == 3) {
      return(i)
    }
  }
  return(i)
}


problem2 = function(trinary_number)
{
  split_num = NULL
  cond = 1
  tmp = trinary_number
  
  while(cond > 0) {
    q = tmp %/% 10
    r = tmp %% 10
    split_num = append(split_num, r)
    
    if (q == 0) {
      cond = 0
    } else {
      tmp = q
    }
  }
  
  if (any(split_num >= 3)) {
    stop("It is not a valid number")
  }
  
  decimal = sum(split_num * 3^{0:(length(split_num) - 1)})
  
  binary = NULL
  cond2 = 1
  tmp2 = decimal
  
  while(cond2 > 0) {
    q2 = tmp2 %/% 2
    r2 = tmp2 %% 2
    binary = append(binary, r2)
    
    if (q2 < 2) {
      binary = append(binary, q2)
      cond2 = 0
    } else {
      tmp2 = q2
    }
  }
  binary_result = sum(rev(binary) * 10^{(length(binary) - 1):0})
  
  return(c(decimal, binary_result))
}

problem2(trinary_number = 121212)


fx = function(x) {x^5 - 7 * x^4 - 3 * x^3 + 79 * x^2 - 46 * x - 120}
dfx = function(x) {5 * x^4 - 28 * x^3 - 9 * x^2 + 158 * x - 46}
hx = function(x) {x^4 - 2 * x^3 - 3 * x^2 + 4 * x + 5}
dhx = function(x) {4 * x^3 - 6 * x^2 - 6 * x + 4}

problem3 = function(f, df, x0 = NULL, L, U, ...) {
  
  inner_newton = function(f, df, x0, L = -Inf, U = Inf, ...) {
    for (i in 1:2000) {
      x1 = x0 - f(x0) / df(x0)
      
      if (abs(x1 - x0) < 1e-8) {
        sol = x1
        break
      } else {
        x0 = x1
      }
    }
    return(sol)
  }
  
  if (is.null(x0)) {
    x0 = c(-1e+5, 1e+5) 
  }
  
  f_sol = inner_newton(f, df, x0[1], L, U)
  l_sol = inner_newton(f, df, x0[2], L, U)
  
  x0_path = seq(f_sol, l_sol, length.out = 1000)
  solution = vector(length = length(x0_path))
  
  for (i in 1:length(x0_path)) {
    solution[i] = inner_newton(f, df, x0_path[i])
  }
  
  result_sol = sort(unique(round(solution, 4)))
  if (any(abs(f(result_sol)) > 1e-6)) {
    warning("there are no solutions")
  }
  return(result_sol)
}

f1_sol = problem3(fx, dfx)
fx(f1_sol)

f3_sol = problem3(hx, dhx)
