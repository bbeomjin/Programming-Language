
# max function
my_max = function(x) 
{
  n = length(x)
  max_v = x[1]
  for (i in 2:n) {
    if (x[i] > max_v) {
      max_v = x[i]
    }
  }
  return(max_v)
}

# which.max function
my_which.max = function(x)
{
  n = length(x)
  max_v = x[1]
  max_ind = 1
  for (i in 2:n) {
    if (x[i] > max_v) {
      max_v = x[i]
      max_ind = i
    }
  }
  return(max_ind)
}

my_max_g = function(x, idx) {
  n = length(x)
  
  for (i in 1:idx) {
    max_v = my_max(x)
    x = x[x != max_v]
  }
  return(max_v)
}


x = rnorm(10)
my_max(x)
max(x)
my_max_g(x, 4)

my_which.max(x)
which.max(x)

# find same name
find_same_name = function(x)
{
  n = length(x)
  result = NULL
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (x[i] == x[j]) {
        result = append(result, x[i])
      }
    }
  }
  return(result)
}

name = c("a", "b", "c", "a", "b")
find_same_name(name)

matching_name = function(x)
{
  n = length(x)
  name1 = name2 = NULL
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      name1 = append(name1, x[i])
      name2 = append(name2, x[j])
    }
  }
  return(data.frame(name1, name2))
}

name = c("a", "b", "c")
matching_name(name)

my_unique = function(x)
{
  n = length(x)
  result = x[1]
  
  for (i in 1:n) {
    a = 0
    for (j in 1:(i - 1)) {
      if (x[i] == x[j]) {
        a = 1
        break
      }
    }
    
    if (a == 0) {
      result = append(result, x[i])
    }
  }
  return(result)
}

my_unique2 = function(x)
{
  n = length(x)
  same_ind = NULL
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (x[i] == x[j]) {
        same_ind = append(same_ind, j)
      }
    }
  }
  return(x[-same_ind])
}


name = c("a", "b", "c", "a", "b", "a")
my_unique(name)
my_unique2(name)



