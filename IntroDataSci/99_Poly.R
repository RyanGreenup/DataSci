
# https://stats.stackexchange.com/questions/104085/is-there-ever-a --------


x <- rnorm(1000)
raw.poly <- poly(x, 6, raw = TRUE)
orth.poly <- poly(x, 6, raw = FALSE)
cor(raw.poly)
cor(orth.poly)

y <- x*2 + 5*x**3-3*x**2 + rnorm(1000)
raw.mod <- lm(y~poly(x, y, raw = TRUE))
orth.mod <- lm(y~poly(x,6, raw = FALSE))
summary(raw.mod)
summary(orth.mod)

https://stackoverflow.com/a/30000214

https://stats.stackexchange.com/questions/104085/is-there-ever-a-reason-not-to-use-orthogonal-polynomials-when-fitting-regression