a <- c(1, 2, 3)
b <- c(4, 5, 6)
c <- c(7, 8, 9)
df1 <- data.frame(a, b, c)
df1
b <- c(1, 3, 5)
c <- c(2, 4, 6)
df2 <- data.frame(b, c)
df2

df3 <- left_join(df1, df2, by="c", multiple="any")
# Merge the columns
common <- intersect(names(df1), names(df2))
common
for (i in common){
  ifelse(df3[paste(i, "y", sep=".")], df3[paste(i, "y", sep=".")], df3[paste(i, "x", sep=".")])
  df3[i] <- ifelse(df3[paste(i, "y", sep=".")], df3[paste(i, "y", sep=".")], df3[paste(i, "x", sep=".")])
  df3[i]
  # Remove old columns
  df3 <- subset(df3, -c(paste(i, "x", sep="."), paste(i, "y", sep=".")))
}  



df3$b <- ifelse(df3$b.y, df3$b.y, df3$b.x)

df3

#create data frame
df <- data.frame(A_points=c(1, 3, 3, 3, 5),
                 B_points=c(4, 5, 2, 3, 2))

#view data frame
df

df$winner <- ifelse(df$A_points > df$B_points, 'A',
                    ifelse(df$A_points < df$B_points, 'B', 'Tie'))

#view data frame
df


