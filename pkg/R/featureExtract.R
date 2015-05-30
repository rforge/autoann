#input values:
#thread_id:the list of thread id
#title: the list of thread titles
#post_time: the list of post_time
#post_text: the list of post_text
#relevant_words: the list of relevant words in post text
#key_words: the list of key words in thread titles
#note: all these lists should have same length
#output : a new data frame which contains 
#number of relevant words, number of key words,
#number of words post, ratio of relevant words, number of question marks,
#index, number of sentence and number of spell errors in each post text
# along with the number of key words and ratio of key words in each thread title
featureExtract<-function(thread_id,title,post_time,post_text,relevant_words,key_words){
        library(qdap)
        post_text<-bracketX(post_text,"angle")
        post_text<-tolower(post_text)
        title<-bracketX(title,"angle")
        title<-tolower(title)
        strcount <- function(x, pattern, split){
                
                unlist(lapply(
                        strsplit(x, split),
                        function(z) na.omit(length(grep(pattern, z)))
                ))
                
        }
        l<-length(post_text)
        lR<-length(relevant_words)
        num_relevant<-0
        #count the number of relevant words
        for(i in 1:lR){
        num_relevant=num_relevant+sapply(post_text,strcount,
                            pattern=relevant_words[i],split=" ")}
        #count the number of key words in thread titles
        num_key<-0
        lK<-length(key_words)
        for(i in 1:lK){
        num_key=num_key+sapply(title,strcount,
                            pattern=key_words[i],split=" ")}

        #count the number of words in post text
        num_words_post<-sapply(gregexpr("\\W+",post_text),length)+1
        #count the number of words in titles
        num_words_title<-sapply(gregexpr("\\W+",title),length)+1
        #count the ratio of relevant words in post text
        ratio_relevant_words<-num_relevant/num_words_post
        #count the ratio of key words in title
        ratio_key_words<-num_key/num_words_title
        #count the number of question marks
        countQue<-function(textSet){sum(gregexpr("[?]", textSet)[[1]] > 0)}
        num_question_marks<-sapply(post_text,countQue)
        #find index in threads
        tL<-length(thread_id)
        index<-rep(0,time=tL)
        indexData<-data.frame(thread_id,post_time)
        for (i in 1:tL){indexDataSub<-subset(indexData,indexData[,1]==thread_id[i]);
            indexDataSub<-indexDataSub[order(indexDataSub[,2]),];
            index[i]<-match(post_time[i],indexDataSub[,2])
        }

        #count the number of sentence for each post
        require("NLP")
        library(openNLP)
        sent_token_annotator <- Maxent_Sent_Token_Annotator()
        num_sentence<-rep(0,time=l)
        for(i in 1:l){if (post_text[i]=="") next else num_sentence[i]<-length(annotate(post_text[i], sent_token_annotator))}
        #count number of spelling errors
        num_spell_Error <- sapply(post_text, function(x){
        length(which_misspelled(x, suggest = FALSE))
         })
        num_spell_Error<-as.numeric(num_spell_Error)
        #return the new data frame
        feature_table<-data.frame(num_relevant,num_key,num_words_post,num_words_title,ratio_relevant_words,ratio_key_words,num_question_marks,index,num_sentence,num_spell_Error)
        feature_table        
}