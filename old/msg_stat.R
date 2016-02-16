# read data
conn <- file("msg_size_by_domain.csv")
dat <- readLines(conn)

fin <- list()
mx <- matrix(nrow=0, ncol=4)
dom_vec <- c()
sizes_bulk <- c()

# put all data into a list
for (i in 1:length(dat)) {
  tmp <- dat[[i]]
  a <- strsplit(tmp, ",")
  
  domain <- a[[1]][1]
  dom_vec[i] <- domain
  
  sizes <- as.numeric( a[[1]][2:length(a[[1]])] )
  sizes_bulk <- c(sizes_bulk, sizes)
  
  # find values
  tmp <- list(domain, sizes, length(sizes), mean(sizes), median(sizes), sd(sizes))
  names(tmp) <- c("domain", "sizes", "msg_count", "mean", "median", "sd")
  
  fin[i] <- list(tmp)
  # here we can access values like this fin[[1]]$domain ...
  
  mx <- rbind(mx, c(fin[[i]]$msg_count, fin[[i]]$mean, 
                    fin[[i]]$median, fin[[i]]$sd))
  
}

df <- data.frame(mx)
names(df) <- c("msg_count", "mean", "median", "sd")


# Plot msg count
# df <- df[order(df$msg_count),]
plot(df$msg_count, main="Количество сообщений по доменам за период", xlab="номер домена",
     ylab="коли-во сообщений за период", col="blue", pch=19)
lines(df$msg_count, col="blue")

# Plot 
plot(df$mean, main="Cреднее значение размера сообщений по доменам", xlab="номер домена", 
     ylab="средний размер сообщения в байтах",
     pch=19, col="blue")
lines(df$mean, col="blue")
abline(h=mean(df$mean), col="red")

# Plot 
plot(df$median, main="Медиана размера сообщений по доменам", xlab="номер домена", 
     ylab="средний размер сообщения (медиана) в байтах",
     pch=19, col="blue")
lines(df$median, col="blue")
abline(h=mean(df$median), col="red")
txt <- paste("Средний размер сообщения (по медиане) равен", round(mean(df$median),0), "байт", sep=" ")
text(20, mean(df$median) + 20000, txt)

boxplot(fin[[1]][2], notched=TRUE)

