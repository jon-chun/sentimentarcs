# Setup
getwd()
setwd('./sentimentarcs')
getwd()
list.files(pattern='*.csv')


###
# Begin SyuzhetR Preprocessing
library('syuzhet')

# import the RAW Sentences.csv, not CLEAN
# because SentimentR assigns greater polarity to HATE > hate
# TODO: Compare CLEAN vs RAW for SentimentR and VADER for impact of such Heuristics


# >>> CUSTOMIZE THIS FOR EACH CORPUS <<<
corpus_subdir = './mtwain_huckleberryfinn/'
corpus_name = 'mtwain_huckleberryfinn' 
# >>> CUSTOMIZE THIS FOR EACH CORPUS <<<

# SentimentTime Preprocessing (e.g. corpus_text_sents_raw_ddefoe_robinsoncrusoe.csv)
input_filename_prefix = 'corpus_text_sents_raw_'
input_filename_suffix = '.csv'

corpus_input_filename = trimws(paste0(corpus_subdir, input_filename_prefix, corpus_name, input_filename_suffix, sep=' '))

###
syuzhet_output_prefix = 'sum_sentiments_syuzhetR_4models_'
syuzhet_output_suffix = '.csv'
syuzhet_output = trimws(paste0(corpus_subdir, syuzhet_output_prefix, corpus_name, syuzhet_output_suffix, sep=' '))


# Use 4 Models in Syuzhet to parse Corpus and generate 4 Sentiment Time Series

# (OPTION A) Read preprocessed Corpus *.csv (Sentence Tokenized and text cleaned by sentimenttime)
# corpus_str <- read.csv(corpus_input_filename,header=T)$sent_raw
# corpus_str <- readLines(corpus_input_filename)

# corpus_sents_v <- syuzhet::get_sentences(corpus_str) # SyuzhetR often splits a line with one Sentence into several lines
# corpus_sents_v <- read.csv(corpus_input_filename, header = TRUE, row.names = 1)
corpus_sents_v <- readLines(corpus_input_filename) # , header = TRUE, row.names = 1)
corpus_sents_v = corpus_sents_v[corpus_sents_v != ",sent_raw"]

# library(readr)
# corpus_sents_v <- read_lines(corpus_input_filename, skip=1) # , header = TRUE, row.names = 1)

# (OPTION B) Read raw textfile using Syuzhet Sentence Tokenizer and text preprocessing
# syuzhet_str <- syuzhet::get_text_as_string(corpus_input_filename)
# syuzhet_sents_v <- syuzhet::get_sentences(corpus_str)

# syuzhet_str <- NLP::as.String(paste(text_of_file, collapse = " "))
# syuzhet_sents_v <- textshape::split_sentence(syuzhet_str)

# Read in RAW Sentences
syuzhet_all_df <- data.frame(sent_raw = corpus_sents_v)

# Compute Sentiment values for each Model 
syuzhet_all_df$syuzhet <- syuzhet::get_sentiment(corpus_sents_v, method='syuzhet')
syuzhet_all_df$bing <- syuzhet::get_sentiment(corpus_sents_v, method='bing')
syuzhet_all_df$afinn <- syuzhet::get_sentiment(corpus_sents_v, method='afinn')
syuzhet_all_df$nrc <- syuzhet::get_sentiment(corpus_sents_v, method='nrc')

# Save Syuzhet Results    
write.csv(syuzhet_all_df, syuzhet_output)



# Begin SentimentR Processing
library('sentimentr')

# Set Output Sentiments Datafile names
sentimentr_output_prefix = 'sum_sentiments_sentimentR_7models_'
sentimentr_output_suffix = '.csv'
sentimentr_output = trimws(paste0(corpus_subdir, sentimentr_output_prefix, corpus_name, sentimentr_output_suffix, sep=' '))

# Use vector of RAW sentences already read (originally parsed by SentimentTime.py) 
# sentimentr_sents_v <- sentimentr::get_sentences(corpus_sents_v)

# ERROR: could not find function 'duplicate'
# sentimentr_sents_v <- duplicate(corpus_sents_v, shallow = FALSE)
  
# Create data.frame with jockers_rinker sentiments
# sentimentr_all_df <- data.frame(sent_raw = sentimentr_sents_v)
# TODO:
sentimentr_all_df <- data.frame(sent_raw = corpus_sents_v)

# sentimentr_sents_v <- read.csv(corpus_input_filename, header = TRUE, row.names = 1)
# Create data.frame with jockers_rinker sentiments
# sentimentr_all_df <- data.frame(sent_raw = sentimentr_sents_v)

# Code from sentimentr.R
# 
# SentimentR function sentiment will not work on native R data types, only
#   types of 'get_sentences'/'get_sentences_char' created by passing text
#   through the sentence tokenizer textshape::split_sentence
# We fool SentimentR by copying and calling it's make_class function in utils.R
#   and passing our preprocessed text (sentence tokenization done in Python) to
#   ensure that SentimentR has the same number/alignment of Sentences in our Corpus
#   as all other Sentiment analysis methods
# 
# get_sentences.character <- function(x, ...) {
#   out <- textshape::split_sentence(x, ...)
#   make_class(out, "get_sentences", "get_sentences_character")
# }

# Code from SentimentR.r (in utils.R)
make_class <- function(x, ...) {
  class(x) <- unique(c(..., class(x)))    
  x
}

# Add other lexicon sentiments
sentimentr_all_df$jockers_rinker <- sentimentr::sentiment(make_class(corpus_sents_v, "get_sentences", "get_sentences_character"), polarity_dt=lexicon::hash_sentiment_jockers_rinker, hyphen=" ", neutral.nonverb.like=TRUE)$sentiment
sentimentr_all_df$jockers <- sentimentr::sentiment(make_class(corpus_sents_v, "get_sentences", "get_sentences_character"), polarity_dt=lexicon::hash_sentiment_jockers, hyphen=" ", neutral.nonverb.like=TRUE)$sentiment
sentimentr_all_df$huliu <- sentimentr::sentiment(make_class(corpus_sents_v, "get_sentences", "get_sentences_character"), polarity_dt=lexicon::hash_sentiment_huliu, hyphen=" ", neutral.nonverb.like=TRUE)$sentiment
sentimentr_all_df$lmcd <- sentimentr::sentiment(make_class(corpus_sents_v, "get_sentences", "get_sentences_character"), polarity_dt=lexicon::hash_sentiment_loughran_mcdonald, hyphen=" ", neutral.nonverb.like=TRUE)$sentiment
sentimentr_all_df$nrc <- sentimentr::sentiment(make_class(corpus_sents_v, "get_sentences", "get_sentences_character"), polarity_dt=lexicon::hash_sentiment_nrc, hyphen=" ", neutral.nonverb.like=TRUE)$sentiment
sentimentr_all_df$senticnet <- sentimentr::sentiment(make_class(corpus_sents_v, "get_sentences", "get_sentences_character"), polarity_dt=lexicon::hash_sentiment_senticnet, hyphen=" ", neutral.nonverb.like=TRUE)$sentiment
sentimentr_all_df$sentiword <- sentimentr::sentiment(make_class(corpus_sents_v, "get_sentences", "get_sentences_character"), polarity_dt=lexicon::hash_sentiment_sentiword, hyphen=" ", neutral.nonverb.like=TRUE)$sentiment

write.csv(sentimentr_all_df, sentimentr_output)


