# openmusesampling

This package contains functions to sample ISRCs using a Metropolis-Hastings uniform random-walk algorithm. Run the code below to see how it works. We are using a data with 1204 observations as our "population" and estimating the mean of the popularity index with samples of size 100.

```{r}
example <- openmusesampling(x = "popularity", data= sample_data)
example
```

The estimate mean should be close to the true mean in our data.

```{r}
mean(sample_data$popularity)
```

Check the histogram

```{r}
hist(example$samples$sample_mean_popularity,
     xlab = "Values",
     main = "Histogram of 1000 RW sample means with N = 100",
     cex.main = 1.5,
     cex.lab = 1.3,
     cex.axis = 1.3)
box()
points(x = mean(sample_data$popularity), y = 0, pch = 17, col = "red", cex = 2)
points(x = mean(example$samples$sample_mean_popularity), y = 0, pch = 2, col = "blue", cex = 2, lwd = 2)
legend("topright", c("True pop. mean", "Mean RW distr."), col = c("red", "blue"), lwd = 3,
       bty = "n")
```
