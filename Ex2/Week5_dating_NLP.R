# The following packages must be installed
library(stringr)
library(dplyr)
library(ggplot2)
library(mosaic)
library(xtable)
library(gridExtra)
library(stopwords)
library(quanteda)
library(bigmemory)
library(rpart)
library(rpart.plot)
library(caret)
library(factoextra)
library(ggfortify)

# Set rounding to 2 digits
options(digits=2)

## ----cache=TRUE, warning=FALSE, message=FALSE----------------------------
profiles <- read.csv( file.path( 'D:\\AVIEL\\Downloads\\R\\BigData_Ex-Wk04-main', 'okcupid_profiles.csv' ), header=TRUE, stringsAsFactors=FALSE)
n <- nrow(profiles)

str(profiles)

pdf( file.path( 'D:\\AVIEL\\Downloads\\R\\BigData_Ex-Wk04-main', 'Week5_dating.pdf') )
# Heights of all users
#favstats(height, data=profiles)

profiles.subset <- filter(profiles, height>=55 & height <=80)

# Histograms of user heights split by sex
histogram( ~ height | sex, width=1, layout=c(1,2), xlab="Height in inches",
           data=profiles.subset)

# Distributions of gender and diet
par1 <- par(mfrow=c(1, 2))
barplot(table(profiles$sex)/n, xlab="sex", ylab="proportion")
barplot(table(profiles$diet)/n, xlab="diet", ylab="proportion")

# Joint distribution of sex and sexual diet
addmargins ( xtabs(~ diet + sex, data=profiles) )
addmargins ( tally(diet ~ sex, data=profiles, format='proportion') )

#
# collapse the many variations of diet
#     anything             /
#     mostly anything     | ---      anything
#     strictly anything    \
#     halal                /
#     strictly halal       \ ---     halal
#     kosher                /
#     mostly kosher        | ---      kosher
#     strictly kosher       \
#     other                /
#     mostly other        | ---      other
#     strictly other       \
#     vegan                /
#     mostly vegan        | ---      vegan
#     strictly vegan       \
#     vegetarian             /
#     mostly vegetarian     | ---      vegetarian
#     strictly vegetarian    \
#
profiles$Diet <- sapply( profiles$diet, function(e) last( strsplit( e, "\\s", perl = T)[[1]] ) )

sex.by.diet <- tally(~ sex + Diet, data=profiles)
sex.by.diet
mosaicplot(sex.by.diet, main="Gender vs Diet", las=1)

essays <- select(profiles, starts_with("essay"))
essays <- apply(essays, MARGIN = 1, FUN = paste, collapse=" ")

html <- c( "<a", "class=.ilink.", "\n", "\\n", "<br ?/>", "/>", "/>\n<br" )
stop.words <-  c( "a", "am", "an", "and", "as", "at", "are", "be", "but", "can", "do", "for", "have", "i'm", "if", "in", "is", "it", "like", "love", "my", "of", "on", "or", "so", "that", "the", "to", "with", "you", "i" )

# list languages for a specific source
stopwords::stopwords_getlanguages("snowball")
##  [1] "da" "de" "en" "es" "fi" "fr" "hu" "ir" "it" "nl" "no" "pt" "ro" "ru" "sv"
head(stopwords::stopwords("english"), 20)
##  [1] "i"          "me"         "my"         "myself"     "we"
##  [6] "our"        "ours"       "ourselves"  "you"        "your"
## [11] "yours"      "yourself"   "yourselves" "he"         "him"
## [16] "his"        "himself"    "she"        "her"        "hers"

html.pat <- paste0( "(", paste(html, collapse = "|"), ")" )
html.pat
stop.words.pat <- paste0( "\\b(", paste(stop.words, collapse = "|"), ")\\b" )
stop.words.pat
essays <- str_replace_all(essays, html.pat, " ")
essays <- str_replace_all(essays, stop.words.pat, " ")


## ----cache=TRUE, warning=FALSE, message=FALSE----------------------------
profiles$has.book <- str_detect(essays, "book")
tally(has.book ~ sex, profiles, format='proportion')

## ----echo=FALSE, cache=TRUE, warning=FALSE, message=FALSE, results='asis'----
queries <- c("travel", "food", "wine", "beer", "football", "art")
output <- data.frame(word=queries, female=rep(0, length(queries)), male=rep(0, length(queries)))
for(i in 1:length(queries)) {
  query <- queries[i]
  has.query <- str_detect(essays, query)
  results <- table(has.query, profiles$sex)
  output[i, 2:3] <- results[2, ] / colSums(results)
}
print(xtable(output, digits=c(0, 0, 3, 3),
             caption ="Proportions of each gender using 'word' in essays.",
             label = "tab:word_use"), include.rownames=FALSE)

# Co-occurrence of `travel' and `wine.'
profiles$has.wine <- str_detect(essays, "wine")
profiles$has.travel <- str_detect(essays, "travel")
travel.vs.wine <- tally(~has.travel + has.wine, data=profiles)
mosaicplot(travel.vs.wine, xlab="travel", ylab="wine", main = "has Travel vs. has Wine")


# go back to one-column plots
par(par1)

# info about texts lengths
profiles$TextLength <- nchar(essays)
summary(profiles$TextLength)

# Visualize text lenght distribution
ggplot(profiles, aes(x = TextLength, fill = sex)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  xlim(20, 6000) +
  labs(y = "Count", x = "Text Length",
       title = "Text Length by Gender")

#
# hack to plot an xtable on a pdf page as part of a pdf sequence:
# qplot of the tableGrob
#
qplot(1:10, 1:10, geom = "blank") + theme_bw() +
  theme(line = element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.title = element_text(face = "bold")
  ) +
  annotation_custom(grob = tableGrob(output), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  labs(title = "", subtitle = "Proportions of each gender using word in essays", caption = "")

profiles$has.football <- str_detect(essays, "football")
results <- tally( ~ has.football + sex, data=profiles)
prop.test(x=results[1, ], n=colSums(results), alternative="two.sided")

male.words <- subset(essays, profiles$sex == "m") %>%
  str_split(" ") %>%
  unlist() %>%
  table() %>%
  sort(decreasing=TRUE) %>%
  names()
female.words <- subset(essays, profiles$sex == "f") %>%
  str_split(" ") %>%
  unlist() %>%
  table() %>%
  sort(decreasing=TRUE) %>%
  names()

# Top 25 male words:
print( male.words[1:25] )
# Top 25 female words
print( female.words[1:25] )

## ----cache=TRUE, warning=FALSE, message=FALSE----------------------------
# Words in the males top 500 that weren't in the females' top 500:
setdiff(male.words[1:500], female.words[1:500])
# Words in the male top 500 that weren't in the females' top 500:
setdiff(female.words[1:500], male.words[1:500])

# Tokenize essay texts
tokens <- tokens(essays, what = "word",
                     remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_hyphens = TRUE)

# Lower case the tokens.
tokens <- tokens_tolower(tokens)

# Use quanteda's built-in stopword list for English.
# NOTE - You should always inspect stopword lists for applicability to
#        your problem/domain.
tokens <- tokens_select(tokens, stopwords(),
                            selection = "remove")


# Perform stemming on the tokens.
tokens <- tokens_wordstem(tokens, language = "english")


# Create a bag-of-words model (document-term frequency matrix)
dfm <- dfm(tokens, tolower = FALSE)
dfm <- dfm_select(dfm, names(topfeatures(dfm, n = 2000)))

tf <- dfm / rowSums(dfm)
tf[is.na(tf)] <- 0


Nrow <- nrow(dfm)
idf <- log(Nrow/colSums(dfm))
tf.idf <- t(t(tf)*idf)
tf.idf <- as.matrix(tf.idf)
# Setup a the feature data frame with labels.
tf.idf <- cbind(Label = profiles$sex[1:dim(tf.idf)[1]], data.frame(tf.idf))


# Training the model
model <- train(
  Label ~ . ,
  method = "rpart",
  data = tf.idf,
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)

decTree <- model$finalModel

pred <- predict(decTree, type="class")
table(pred, tf.idf$Label)

dev.off()

pdf( file.path( 'D:\\AVIEL\\Downloads\\R\\BigData_Ex-Wk04-main', 'Week5_dating_NLP.pdf') ,width = 20, height = 20)

rpart.plot(decTree)


names <- data.frame(val = decTree$variable.importance)
names <- subset(names, val > 1)

rmv <- c(row.names(names), "Label")

# Cleanup column names.
tf.idf <- tf.idf[, !colnames(tf.idf) %in% c(rmv)]

tf.idf.PCA <- prcomp(tf.idf)


k <- 2
clusters <- kmeans(tf.idf.PCA$x[,1:2], k)
fviz_cluster(clusters, data =  tf.idf.PCA$x[,1:2],geom = "point",ellipse.type = "convex", ggtheme = theme_bw() , main = paste(c("clusters for K = ", k), collapse = " "))
plot(tf.idf.PCA, main = paste(c("PCAs for K = ", k), collapse = " "))

k <- 3
clusters <- kmeans(tf.idf.PCA$x[,1:2], k)
fviz_cluster(clusters, data =  tf.idf.PCA$x[,1:2],geom = "point",ellipse.type = "convex", ggtheme = theme_bw() , main = paste(c("clusters for K = ", k), collapse = " "))
plot(tf.idf.PCA, main = paste(c("PCAs for K = ", k), collapse = " "))

k <- 4
clusters <- kmeans(tf.idf.PCA$x[,1:2], k)
fviz_cluster(clusters, data =  tf.idf.PCA$x[,1:2],geom = "point",ellipse.type = "convex", ggtheme = theme_bw() , main = paste(c("clusters for K = ", k), collapse = " "))
plot(tf.idf.PCA, main = paste(c("PCAs for K = ", k), collapse = " "))

k <- 10
clusters <- kmeans(tf.idf.PCA$x[,1:2], k)
fviz_cluster(clusters, data =  tf.idf.PCA$x[,1:2],geom = "point",ellipse.type = "convex", ggtheme = theme_bw() , main = paste(c("clusters for K = ", k), collapse = " "))
plot(tf.idf.PCA, main = paste(c("PCAs for K = ", k), collapse = " "))

dev.off()

save(file = 'Week5_datingNLP.rdata', decTree, dfm, pca)