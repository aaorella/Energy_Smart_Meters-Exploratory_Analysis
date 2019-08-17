# Read text file
library(wordcloud)
library(RColorBrewer)
library(tm)
library(tidytext)
library(rtweet)
library(purrr)
library(tidyverse)
library(knitr)
library(lubridate)
library(ggplot2)

#Setting our working directory

setwd("C:\Users\Josue Sanchez\Desktop\EXPLO\ProyectoSegundoParcial\Energy_Smart_Meters-Exploratory_Analysis\tweets")

#Open .csv as table

tweets <-read.csv("Resultado1.txt", header = FALSE, sep = "\t", dec = ".")


#Changing header

tweets <- tweets %>% rename(autor = V2, fecha = V3,
                            texto = V4, tweet_id = V1)

# remove http elements manually

tweets$stripped_text <- gsub("http.*","",  tweets$texto)
tweets$stripped_text <- gsub("https.*","", tweets$stripped_text)


#tokennizer

limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}



# Se aplica la función de limpieza y tokenización a cada tweet

tweets <- tweets %>% mutate(texto_tokenizado = map(.x = texto,
                                                   .f = limpiar_tokenizar))



#tweets %>% select(texto_tokenizado) %>% head()
#tweets %>% slice(1) %>% select(texto_tokenizado) %>% pull()



#unnest process expansion 

tweets_tidy <- tweets %>% select(-texto) %>% unnest()
tweets_tidy <- tweets_tidy %>% rename(token = texto_tokenizado)
#head(tweets_tidy)




#stopwprds

lista_stopwords <- c("un","una","unas","unos","uno","sobre","todo","también","tras",
                     "otro","algún","alguno","alguna","algunos","algunas","ser","es",
                     "soy","eres","somos","sois","estoy","esta","estamos","estais","estan",
                     "como","en","para","atras","porque","por qué","estado","estaba","ante",
                     "antes","siendo","ambos","pero","por","poder","puede","puedo","podemos",
                     "podeis","pueden","fui","fue","fuimos","fueron","hacer","hago","hace",
                     "hacemos","haceis","hacen","cada","fin","incluso","primero","desde",
                     "conseguir","consigo","consigue","consigues","conseguimos","consiguen",
                     "ir","voy","va","vamos","vais","van","vaya","gueno","ha","tener","tengo",
                     "tiene","tenemos","teneis","tienen","el","la","lo","las","los","su","aqui",
                     "mio","tuyo","ellos","ellas","nos","nosotros","vosotros","vosotras","si",
                     "dentro","solo","solamente","saber","sabes","sabe","sabemos","sabeis","saben",
                     "ultimo","largo","bastante","haces","muchos","aquellos","aquellas","sus",
                     "entonces","tiempo","verdad","VERDADERO","verdadera","cierto","ciertos",
                     "cierta","ciertas","intentar","intento","intenta","intentas","intentamos",
                     "intentais","intentan","dos","bajo","arriba","encima","usar","uso","usas",
                     "usa","usamos","usais","usan","emplear","empleo","empleas","emplean",
                     "ampleamos","empleais","valor","muy","era","eras","eramos","eran",
                     "modo","bien","cual","cuando","donde","mientras","quien","con","entre",
                     "sin","trabajo","trabajar","trabajas","trabaja","trabajamos","trabajais",
                     "trabajan","podria","podrias","podriamos","podrian","podriais","yo","aquel")




lista_stopwords_en <- c('me', 'my', 'myself', 'we', 'our', 'ours', 'ourselves',
                     'you','your', 'yours', 'yourself', 'yourselves', 'he', 'him','his',
                     'himself', 'she', 'her', 'hers', 'herself', 'it', 'its', 'itself',
                     'they', 'them', 'their', 'theirs', 'themselves', 'what', 'which',
                     'who', 'whom', 'this', 'that', 'these', 'those', 'am', 'is', 'are',
                     'was', 'were', 'be', 'been', 'being', 'have', 'has', 'had',
                     'having', 'do', 'does', 'did', 'doing', 'a', 'an', 'the', 'and',
                     'but', 'if', 'or', 'because', 'as', 'until', 'while', 'of', 'at',
                     'by', 'for', 'with', 'about', 'against', 'between', 'into',
                     'through', 'during', 'before', 'after', 'above', 'below', 'to',
                     'from', 'up', 'down', 'in', 'out', 'on', 'off', 'over', 'under',
                     'again', 'further', 'then', 'once', 'here', 'there', 'when',
                     'where', 'why', 'how', 'all', 'any', 'both', 'each', 'few', 'more',
                     'most', 'other', 'some', 'such', 'no', 'nor', 'not', 'only', 'own',
                     'same', 'so', 'than', 'too', 'very', 's', 't', 'can', 'will',
                     'just', 'don', 'should', 'now', 'd', 'll', 'm', 'o', 're', 've',
                     'y', 'ain', 'aren', 'couldn', 'didn', 'doesn', 'hadn', 'hasn',
                     'haven', 'isn', 'ma', 'mightn', 'mustn', 'needn', 'shan',
                     'shouldn', 'wasn', 'weren', 'won', 'wouldn','i')

#clean data from spanish and english stopwords

tweets_tidy <- tweets_tidy %>% filter(!(token %in% lista_stopwords))
tweets_tidy <- tweets_tidy %>% filter(!(token %in% lista_stopwords_en))


#distribucion temporal
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Problema al graficar la distribucion de tweets por usuario (mucho usuarios)
ggplot(tweets, aes(x = as.Date(fecha), fill = autor)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "5 month") +
  labs(x = "fecha de publicación", y = "número de tweets") +
  facet_wrap(~ autor, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

#tweets cambio de formato de fecha
tweets$fecha <- anytime::anydate(tweets$fecha)
typeof(tweets$fecha)



#frecuencia de palabras

tweets_mes_anyo <- tweets %>% mutate(mes_anyo = format(fecha,"%Y-%m"))
tweets_mes_anyo %>% group_by(autor, mes_anyo) %>% summarise(n = n()) %>%
  ggplot(aes(x = mes_anyo, y = n, color = autor)) +
  geom_line(aes(group = autor)) +
  labs(title = "Número de tweets publicados", x = "fecha de publicación",
       y = "número de tweets") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 6),
        legend.position = "bottom")



#Words by author

tweets_tidy %>% group_by(autor) %>% summarise(n = n())
tweets_tidy %>%  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw() 



#words disctintion by author

tweets_tidy %>% select(autor, token) %>% distinct() %>%  group_by(autor) %>%
  summarise(palabras_distintas = n()) 

tweets_tidy %>% select(autor, token) %>% distinct() %>%
  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()





#mean lenght of tweets by users

tweets_tidy %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%                       
  group_by(autor) %>% summarise(media_longitud = mean(longitud),
                      sd_longitud = sd(longitud))




tweets_tidy %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%
  group_by(autor) %>%
  summarise(media_longitud = mean(longitud),
            sd_longitud = sd(longitud)) %>%
  ggplot(aes(x = autor, y = media_longitud)) +
  geom_col() +
  geom_errorbar(aes(ymin = media_longitud - sd_longitud,
                    ymax = media_longitud + sd_longitud)) +
  coord_flip() + theme_bw()





#most popular words by user

tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>% print(n=30)



#Representación de frecuencias

tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~autor,scales = "free", ncol = 1, drop = TRUE)

          #iconv(tweets_tidy$token, to = "ASCII//TRANSLIT") 

# data_text_corpus <- Corpus(VectorSource(tweets_tidy))
# data_text_corpus <- tm_map(data_text_corpus,content_transformer(function(x) iconv(x, to='ASCII', sub='byte')))
# 
# data_text_corpus <- tm_map(tweets_tidy, removeWords, stopwords("spanish"))
# tweets_tidy <- removeWords(tweets_tidy, words = stopwords("spanish"))



#red de palabras por usuario 
wordcloud_custom <- function(grupo, df){
  print(grupo)
  wordcloud(words = df$token, freq = df$frecuencia,
            max.words = 400, random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
}

df_grouped <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
  group_by(autor) %>% mutate(frecuencia = n / n()) %>%
  arrange(autor, desc(frecuencia)) %>% nest() 

walk2(.x = df_grouped$autor, .y = df_grouped$data, .f = wordcloud_custom)
















# remove punctuation, convert to lowercase, add id for each tweet!
tweets_clean <- tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

data("stop_words")

st <-tbl_df(data.frame(stopwords("es")))

nrow(tweets_clean)
last_error()

# remove stop words from your list of words
cleaned_tweet_words <- tweets_clean %>%
  anti_join(stop_words)

nrow(tweets_clean)

# Build corpus
corpus <- Corpus(VectorSource(tweets))
# Create term document matrix
tdm <- TermDocumentMatrix(corpus, 
                          control = list(minWordLength=c(1,Inf)))
t <- removeSparseTerms(tdm, sparse=0.98)
m <- as.matrix(t)
# Plot frequent terms
freq <- rowSums(m)
freq <- subset(freq, freq>=50)
barplot(freq, las=2, col = rainbow(25))
# Hierarchical word/tweet clustering using dendrogram 
distance <- dist(scale(m))
print(distance, digits = 2)
hc <- hclust(distance, method = "ward.D")
plot(hc, hang=-1)
rect.hclust(hc, k=12)
# Nonhierarchical k-means clustering of words/tweets
m1 <- t(m)
set.seed(222)
k <- 3
kc <- kmeans(m1, k)
kc
fviz_cluster(kc.cut, tweets)

