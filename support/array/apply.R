https://stackoverflow.com/questions/9950144/access-lapply-index-names-inside-fun/9950217

But note that you can always pass in extra arguments to the function, so the following works:

x <- list(a=11,b=12,c=13) # Changed to list to address concerns in commments
lapply(seq_along(x), function(y, n, i) { paste(n[[i]], y[[i]]) }, y=x, n=names(x))
Here I use lapply over the indices of x, but also pass in x and the names of x. As you can see, the order of the function arguments can be anything - lapply will pass in the "element" (here the index) to the first argument not specified among the extra ones. In this case, I specify y and n, so there's only i left...

Which produces the following:

[[1]]
[1] "a 11"

[[2]]
[1] "b 12"

[[3]]
[1] "c 13"
UPDATE Simpler example, same result:

lapply(seq_along(x), function(i) paste(names(x)[[i]], x[[i]]))
