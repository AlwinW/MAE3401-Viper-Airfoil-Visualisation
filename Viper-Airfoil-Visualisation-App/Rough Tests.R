testraw <- read.table("test.dat", sep = " ",
                   skip = 12,
                   nrow = 9888)

test <- as.data.frame(matrix(c(t(testraw[2:6])), ncol=6))

colnames(test) <- c("X", "Y", "U", "V", "P", "vort_xy_plane")

ggplot(test, aes(x = X, y = Y, colour = P)) +
  geom_point() +
  geom_point(data = filter(test, P == 0), colour = "red") +
  coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(-1.5,1.5), expand = FALSE) +
  theme(aspect.ratio = 1)


ggplot(filter(test, X > -2.5 & X < 2.5 & Y > -2.5 & Y <2.5), aes(x = X, y = Y, z = P)) +
  geom_point()
  stat_contour()
  
ggplot(test, aes(x = X, y = Y, geom = P)) +
  xlim(c(-1.5,1.5)) +
  ylim(c(-1.5, 1.5)) +
  # geom_point() +
  stat_density2d() +
  theme(aspect.ratio = 1)
  
  
testsmall <- filter(test, X > -1.5 & X < 1.5 & Y > -1.5 & Y < 1.5) %>%
  arrange(Y) %>%
  arrange(X)
ggplot(testsmall, aes(x = X, y = Y, z = P)) +
  # geom_point() + 
  stat_density2d() +
  coord_fixed()

### Interp using akima
library(akima)
asdf <- with(testsmall, interp(x = X, y = Y, z = P))

filled.contour(x = asdf$x,
               y = asdf$y,
               z = asdf$z)

library(reshape2)
df <- melt(asdf$z, na.rm = TRUE)
names(df) <- c("X", "Y", "P")
df$X <- asdf$z[df$X]
df$Y <- asdf$z[df$Y]

ggplot(data = df, aes(x = X, y = Y, z = P)) +
  stat_contour()
+
  geom_point(data = filter(test, P == 0), colour = "red")

