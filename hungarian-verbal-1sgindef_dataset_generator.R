# get -om/-ok pairs for 1sg indef from szoszablya
# make up odds
# get following information
# lemma freq
# is the verb transitive (= does it have a transitive form in szoszablya)
# does the verb stem vary (= does it have a form in szoszablya that has no -ik)


setwd('/Users/pracz/Work/Bristol/lectures_apps/ceuka/ikes_igek_acta/')
# load('/users/pracz/Work/Bristol/iekin/kinterm-frequency/multiplot.Rfnc')

library(stringr)
library(dplyr)
# library(ggplot2)
# library(mgcv)


####################################################################################################
# phase I: indef
# webkorpusz results for 1sg indef
d <- read.delim('/Users/pracz/Work/Bristol/lectures_apps/ceuka/verbspersoneindef.csv', sep=';')
# R likes its factors but I don't
d <- d %>% mutate_if(is.factor,as.character)

d <- d %>% mutate(base = str_replace(word, '.$', ''), indef_m = paste(base, 'm', sep=''), indef_um = paste(base, 'k', sep=''))
# there is some rubbish lying round
d <- d %>% subset(str_detect(d$word, '[mk]$'))
# the parser made up different stems for varying forms like eszem/eszik, so we make new stem tags by chopping off the suffix from the form
d$stem <- str_replace(d$word,'..$','')
# what else do we need from this df? word, stem, freq, syllable count. stemfreq is not reliable for the above reasons, we'll use 3sg freq instead
d <- d %>% filter(str_detect(lemma, 'ik$'))

####################################################################################################
# phase II: -m and -k pairs
# get most of the word
# pairs <- d %>% select(indef,lemma) %>% mutate(indef = str_replace(indef, '.$', ''))
# glue the appropriate suffix to the end of it
# pairs <- pairs %>% mutate(indef_m = str_replace(indef, '$', 'm'), indef_um = str_replace(indef, '$', 'k')) %>% select(-indef)
# now every stem that has two forms is here twice
# pairs <- pairs[!duplicated(pairs$indef_m),]
# retrieve frequencies for marked forms (will be 0 and so NA for heaps of forms)
indef_m <- d %>% filter(str_detect(word, 'm$')) %>% mutate(indef_m = word, freq_m = freq) %>% select(indef_m,freq_m)
indef_um <- d %>% filter(str_detect(word, 'k$')) %>% mutate(indef_um = word, freq_um = freq) %>% select(indef_um,freq_um)
# merge them together
d <- merge(d,indef_m, all.x=T)
d <- merge(d,indef_um, all.x=T)
# filter for repeats
d <- d %>% select(-freq,-word)
d <- d[!duplicated(d),]

####################################################################################################
# phase III: def forms
# all 1sg<def> forms from szablya
d2 <- read.delim('/Users/pracz/Work/Bristol/lectures_apps/ceuka/verbspersonedef.csv', sep=';')
d2 <- d2 %>% mutate_if(is.factor,as.character)
d2 <- d2 %>% subset(str_detect(d2$word, 'm$'))
# all I want to know is: does verb have transitive form in corpus?
d$verb_transitive <- ifelse(d$lemma %in% d2$lemma, T, F)
# yeah we'll need to tidy this up


####################################################################################################
# phase IV: all 3sg indef forms
# all 3sg indef forms from szablya
d3 <- read.delim('/Users/pracz/Work/Bristol/lectures_apps/ceuka/verbspersthree.csv', sep=';')
d$lemma2 <- str_replace(d$lemma, 'ik$', '')
d$verb_stem_varies <- ifelse(d$lemma2 %in% d3$word,T,F)

# ikes <- d3 %>% select(word) %>% filter(str_detect(word, 'ik$')) %>% mutate(stem = str_replace(word, 'ik$','')) %>% select(stem)
# pairs <- pairs %>% mutate(ikes = ifelse(stem %in% ikes$stem, T, F))


# if a freq value is NA it means there was 0 of that form in the webkorpusz
d[is.na(d)] <- 0

# fix very Hungarian vowels
hungarian <- function(x){
x <- str_replace_all(x, "û","ű")
x <- str_replace_all(x, "õ","ő")
return(x)}

d <- d %>% mutate_if(is.character, funs(hungarian))
  
# set up odds, log everything in sight
d <- d %>% mutate(log_freq_m = log(freq_m),
log_freq_um = log(freq_um),
log_freq_lemma = log(lemmafreq),
odds_m = (freq_m)/(freq_um),
log_odds_m = log(odds_m))

plot(density(d$log_odds_m))

# throwing out exotic forms
d <- d %>% filter(freq_m > 0 & freq_um > 0 & (freq_um+freq_m) > 4)

# d <- d %>% mutate(medial = str_detect(indef_um, '[^k]([oö]d[oö]k|[óő]d[oö]k|d[oö]kl[oö]k|l[oö]k)'))

####################################################################################################
# labelling endings, marking outliers

# d$verb_transitive <- ifelse(d$verb_transitive_notes=='x', F, d$verb_transitive) %>% as.factor
# d$verb_medial <- ifelse(d$verb_medial_notes=='x', F, d$verb_medial) %>% as.factor
d$verb_odik <- str_detect(d$lemma, '[oöóő]dik$') %>% as.factor
# d$verb_odik <- str_detect(d$lemma, '[oö]dik$') %>% as.factor
# d$verb_oodik <- str_detect(d$lemma, '[óő]dik$') %>% as.factor
d$verb_lik <- str_detect(d$lemma, 'lik$') %>% as.factor
d$verb_zik <- str_detect(d$lemma, '[^s]zik$') %>% as.factor
d$c.log_freq_lemma <- d$log_freq_lemma - mean(d$log_freq_lemma)
d$verb_stem_varies <- d$verb_stem_varies %>% as.factor


d$outlier <- ifelse(d$log_odds_m > mean(d$log_odds_m) + 2.5 * mad(d$log_odds_m) | d$log_odds_m < mean(d$log_odds_m) - 2.5 * mad(d$log_odds_m), T, F)

write.csv(d, file='ikes_pairs_dataset.csv', row.names = F)

