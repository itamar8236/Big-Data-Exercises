#library(zoo)

A <- read.delim("C:\\Users\\user\\Desktop\\Ex1\\table.tsv")
A$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )
A <- unique(A)
# count <- 0
# for (d in tmp$DateTime)
# {
#   t <- tmp[tmp$DateTime == d,]
  
#   if(nrow(t) > 1)
#   {
#     count <- count + 1
#   }
# }
# print(count)
# print(B[dgfghg])

B <- A[ order(A$DateTime), ]

#section a:
DF_for_cube <- B[, c("Net.generation", "Net.generation.1", "Net.generation.2", "Net.generation.3", "Net.generation.4", "Net.generation.5", "Net.generation.6", "Net.generation.7", "Net.generation.8", "Net.generation.9", "Net.generation.10")]
DF_for_cube$month <- format(as.POSIXct(B[, 'DateTime']), format = "%m")
DF_for_cube$day <- format(as.POSIXct(B[, 'DateTime']), format = "%d")

# r <- 1530:1535
# print(DF_for_cube["Net.generation"][r,])

# DF_for_cube$Net.generation[is.na(DF_for_cube$Net.generation)] <- mean(DF_for_cube$Net.generation, na.rm = TRUE)
# print("####################################################")
# print(DF_for_cube["Net.generation"][r,])
# print(B[sdgdgffgh])


data_cube_a <- by(DF_for_cube[,c("Net.generation", "Net.generation.1", "Net.generation.2", "Net.generation.3", "Net.generation.4", "Net.generation.5", "Net.generation.6", "Net.generation.7", "Net.generation.8", "Net.generation.9", "Net.generation.10")]
,DF_for_cube[,c("day", "month")],FUN=sum, na.rm = TRUE)
l <- list()
# print(head(data_cube_a))
for (d in c("07", "08", "09", "10", "11", "12", "13"))
{
    l <- append(l, (data_cube_a[d, "02"]) / 11)
}
# print(l)
# print(B[sdfgrh])

# # print(DF_for_cube[DF_for_cube$day == 01,])


# f <- DF_for_cube$month == "02"
# t <- DF_for_cube[DF_for_cube$month == "02" & DF_for_cube$day %in% c("07", "08", "09", "10", "11", "12", "13"),]
# #print(head(t))

# l <- list()
# for (d in c("01", "07", "08", "09", "10", "11", "12", "13"))
# {
#     t <- DF_for_cube[DF_for_cube$month == "02" & DF_for_cube$day == d,]
#     t <- t[,0:11]
#     t <- colSums(t, na.rm = TRUE)
#     t <- mean(t)
#     l <- append(l, t)
# }
# print(l)
# print(x[dfggfghgthj])

# #filter(expr, cell_type %in% c("bj fibroblast", "hesc"))

# # t <- filter(DF_for_cube, month %in% c(01, 02, 03))
# # print(head(t))

# # t <- filter(DF_for_cube, month == 02)
# # print(head(t))

# print(l)
# sliced_values <- data_cube_a[c("07", "08", "09", "10", "11", "12", "13"), "02"]
#M <- mean(sliced_values)

pdf("C:\\Users\\user\\Desktop\\Ex1\\Week3_power.pdf")

plot(seq(7, 13), l, xlab="Day", ylab="Gen", xlim = c(7,13), ylim = c(min(unlist(l)), max(unlist(l))))

lines(seq(7, 13), l, col = 'red', xlim = c(7,13), ylim = c(min(unlist(l)), max(unlist(l))), pch = 16)

#abline(M, 0, col = 'black', lw =2)
#text(10, M + 0.02 * M, paste("MEAN - ", M))

#print(data_cube_a)
# print(sliced_values)
print("end of section a")

#section b: 
DF2_for_cube <- B[,c("Demand", "Demand.1", "Demand.2", "Demand.3", "Demand.4", "Demand.5", "Demand.6", "Demand.7", "Demand.8", "Demand.9", "Demand.10")]

colnames(DF2_for_cube) <- c("BPAT", "CISO", "CPLE", "ERCO", "FPL", "ISNE", "MISO", "NYIS", "PACW", "PJM", "United States Lower 48 (region)")
DF2_for_cube$hour <- format(as.POSIXct(B[, 'DateTime']), format = "%H")
data_cube_b <- apply(DF2_for_cube[,1:11], 2, function(x) tapply(x, DF2_for_cube$hour, sum, na.rm = TRUE))
data_cube_b <- as.table(data_cube_b)
names(attributes(data_cube_b)$dimnames) <- c('hour', 'region')

# diced_values_morning <- data_cube_b[c('10', '11', '12', '13', '14', '15', '16', '17'), c('PJM', 'NYIS', 'ISNE', 'FPL', 'CPLE')]
# diced_values_night <- data_cube_b[c('20', '21', '22', '23', '00', '01', '02'), c('PJM', 'NYIS', 'ISNE', 'FPL', 'CPLE')]


a_list <- list()
b_list <- list()
plot(seq(10, 17), xlab="Time", ylab="Demand (normalized)", xlim = c(10, 17), ylim = c(-2, 2))
tot <- 0
for (loc in c('PJM', 'NYIS', 'ISNE', 'FPL', 'CPLE'))
{
  tot <- tot + 1
  y <- data_cube_b[c('10', '11', '12', '13', '14', '15', '16', '17'), loc]

  M <- mean(y)
  S <- sd(y)
  y <- (y - M) / S

  U <- matrix(data=c(1, 1, 1, 1, 1, 1, 1, 1, 10, 11, 12, 13, 14 ,15, 16, 17),ncol=8,byrow = T)
  U <- t(U)
  # print(y)
  n <- solve(t(U) %*% U) %*% t(U) %*% y
  a <- n[1]
  b <- n[2]
  abline(a, b, col = tot, lw =2)
  a_list <- append(a_list, a)
  b_list <- append(b_list, b)
}
a <- sum(unlist((a_list))) / tot
b <- sum(unlist((b_list))) / tot
abline(a, b, col = 'black', lw = 4, lt = 2)

##############################################################################

a_list <- list()
b_list <- list()
plot(20:26, xaxt='n', xlab="Time", ylab="Demand (normalized)", xlim = c(20, 26), ylim = c(-2, 2))
axis(1, at=20:26, labels = c('20', '21', '22', '23', '00', '01', '02'))
tot <- 0
for (loc in c('PJM', 'NYIS', 'ISNE', 'FPL', 'CPLE'))
{
  tot <- tot + 1
  y <- data_cube_b[c('20', '21', '22', '23', '00', '01', '02'), loc]
  names(y) <- c('20', '21', '22', '23', '24', '25', '26')

  M <- mean(y)
  S <- sd(y)
  y <- (y - M) / S

  U <- matrix(data=c(1, 1, 1, 1, 1, 1, 1, 20, 21, 22, 23, 24 ,25, 26),ncol=7,byrow = T)
  U <- t(U)
  # print(y)
  n <- solve(t(U) %*% U) %*% t(U) %*% y
  a <- n[1]
  b <- n[2]
  abline(a, b, col = tot, lw =2)
  a_list <- append(a_list, a)
  b_list <- append(b_list, b)
}
a <- sum(unlist((a_list))) / tot
b <- sum(unlist((b_list))) / tot
abline(a, b, col = 'black', lw = 4, lt = 2)

dev.off()
print('finished!')


# drilldown_morning <-
#   apply(diced_values_morning, 'hour', 
#       FUN = function(x) sum(x, na.rm = TRUE) )
# drilldown_night <-
#   apply(diced_values_night, 'hour', 
#       FUN = function(x) sum(x, na.rm = TRUE) )

# M <- mean(drilldown_morning)
# S <- sd(drilldown_morning)
# y <- (drilldown_morning - M) / S

# ###########
# # y <- drilldown_morning
# ###########

# U <- matrix(data=c(1, 1, 1, 1, 1, 1, 1, 1, 10, 11, 12, 13, 14 ,15, 16, 17),ncol=8,byrow = T)
# U <- t(U)

# n <- solve(t(U) %*% U) %*% t(U) %*% y
# a <- n[1]
# b <- n[2]

# #######################################

# plot(seq(10, 17), y, xlab="Time", ylab="Demand", xlim = c(10, 17), ylim = c(min(y), max(y)))


# abline(a, b, col = 'black', lw =2)

# M <- mean(drilldown_night)
# S <- sd(drilldown_night)
# y <- (drilldown_night - M) / S

# ###########
# # y <- drilldown_night
# ###########

# names(y) <- c('20', '21', '22', '23', '24', '25', '26')

# U <- matrix(data=c(1, 1, 1, 1, 1, 1, 1, 20, 21, 22, 23, 24 ,25, 26),ncol=7,byrow = T)
# U <- t(U)

# n <- solve(t(U) %*% U) %*% t(U) %*% y
# a <- n[1]
# b <- n[2]

# plot(20:26, y,xaxt='n', xlab="Time", ylab="Demand", xlim = c(20, 26), ylim = c(min(y), max(y)))
# axis(1, at=20:26, labels = c('20', '21', '22', '23', '00', '01', '02'))


# abline(a, b, col = 'black', lw =2)
# print('finished!')
# dev.off()