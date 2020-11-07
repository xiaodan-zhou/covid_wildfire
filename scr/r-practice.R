# https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html
# https://www.brodieg.com/2020/05/05/on-nse/
# http://adv-r.had.co.nz/Computing-on-the-language.html


# case 1 
expr = expression(a + b * x)
expr

attributes(expr)
class(expr)
typeof(expr)

eval(expr) # Error in eval(expr) : object 'a' not found

substitute(expr, list(a=1,b=2,x=3)) 
# expr  ?????????????

substitute(expression(a + b * x), list(a=1,b=2))
# expression(1 + 2 * x)


# case 
a <- 1 # or a = 1

substitute(a) 
# a 
quote(a)
# a 



# case 2 
expr = substitute(x + y, list(x = 1))
expr # 1 + y
eval(expr, list(y = 2)) # 3


# case 3 
expr = quote(x + y)
expr # x + y

class(expr) # "call"
typeof(expr) # "language" 

eval(expr, list(x = 1, y = 2)) # 3


# case 4
e = new.env()
assign(x = "a",value = 1,envir = e)
substitute(a, env = e)
quote(a)
substitute(a)

### https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html
# Labelling
dpar = deparse(substitute(x+y))
dpar # "x + y"
typeof(dpar) # "character"

dpar = "x+y"
dpar # "x+y"
class(dpar) # "character"

dpar = deparse(quote(x+y))
dpar # "x + y"
typeof(dpar) # "character"

dpar = deparse(substitute(
  {
  a + b
  c + d
  }
  ))
dpar
dpar[2]


# formulas
# a language object (i.e. an unevaluated expression) with a class of formula 
# and an attribute that stores the environment:
f = ~ x + y + z
typeof(f)
attributes(f)
environment(f)
class(f)
length(f)
f[1]
f[2]
f[[1]]

f_rhs(f)
#> x + y + z
f_lhs(f)
#> NULL
f_env(f)

g <- y ~ x + z
length(g)
g[[1]] == "~"
g[[2]] == "y"
g[[3]]

foo <- list( str='R', vec=c(1,2,3), bool=TRUE )
foo[[1]]
foo[1]


a = 1 
typeof(as.name(substitute(a)))

f <- ~ 1 + 2 + 3
typeof(f)
eval(f) #??????????????????????

# eval 
# substitute
# call 
# as.name
# symbol
# quote
# eval.parent
# parse 
# bquote
