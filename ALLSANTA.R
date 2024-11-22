library(tidyverse)
library(readxl)
library(english)
library(stringr)
rm(list = ls())

# prep --------------------------------------------------------------------

#change this to point to ur input. Must be formatted like example input excel.
og <- read_xlsx("excel.xlsx")
#cleanup I needed for my file
#og <- og %>%
#  rename(Discord_username = `Discord username`) %>%
#  rename(`gift_give_type (Other)` = `gift_give_type (Other - Please specify)`)
#og <- og[-18,]
  
types_list <- c("Art",
                "Remix art",
                "Creative writing",
                "Other writing",
                "Craft or physical piece",
                "Audio",
                "Other")

gift_give_list <- paste0("gift_give_type (",
                        types_list,
                        ")")

gift_receive_list <- paste0("gift_receive_type (",
                         types_list,
                         ")")

# Matches prep -----------------------------------------------------------------
horrible_sep = ";.," #for consistent joining and splitting

num_participants = length(unique(og$Discord_username))
unique_participants = unique(og$Discord_username)

## SETTING UP
#creates a summary view of all unique santas + giftees for matching purposes

fandoms = paste(og$Source_1,
                og$Source_2,
                og$Source_3,
                og$Source_4,
                og$Source_5,
                og$Source_6,
                sep = horrible_sep
)
all_santas <- data.frame(Santa = og$Discord_username,
                         Fandoms = fandoms,
                         Entries = og$entries_count)
for (type in types_list) {
  all_santas[[type]] <- NA
}

all_giftees <- all_santas %>%
  rename(Giftee = Santa)

#future proofing in case more or less gift types
start <- which(colnames(all_santas) %in% types_list)[1]
end <- start + length(types_list) - 1
all_santas[start:end] = og[c(gift_give_list)]
all_giftees[start:end] = og[c(gift_receive_list)]

#all_giftees
# Giftee    Fandoms
# 1 a       GG;.,Blue;.,Sailor Moon;.,Zelda;.,Xenoblade;.,FGO
# 2 b       Vocaloid;.,Pokemon;.,Arknights;.,Conan;.,Bocchi;.,MDZS
# 3 c       ...
# 4 d       ...
# 5 e       ...
# 6 f       ...
#   Entries  Art Remix art Creative writing Other writing Craft or physical piece Audio Other
# 1       1 TRUE     FALSE             TRUE          TRUE                   FALSE  TRUE FALSE
# 2       2 TRUE      TRUE            FALSE         FALSE                    TRUE FALSE FALSE
# 3       1 TRUE      TRUE            FALSE         FALSE                    TRUE FALSE FALSE
# 4       1 TRUE     FALSE            FALSE         FALSE                   FALSE FALSE FALSE
# 5       2 TRUE      TRUE             TRUE         FALSE                   FALSE FALSE FALSE
# 6       3 TRUE     FALSE            FALSE         FALSE                    TRUE  TRUE  TRUE


## BLACKIST
blacklist <- data.frame(Santa = og$Discord_username,
                        Blacklist = rep(NA, num_participants))

#manual blacklist adding goes here 

#blacklist
#           Santa     Blacklist
# 1             a      <NA>
# 2             b      x, y
# 3             c      <NA>
# 4             d      <NA>
# 5             e      <NA>


## CREATING POTENTIAL MATCHES
options <- data.frame(Santa = og$Discord_username) #df, col 1 is santa cols 2-n are giftee names w TF
for (user in unique_participants) {
  options[[user]] <- NA
}

option_maker <- function(row){
  person <- row[1]
  print(person)
  
  row_name <- names(row)

  get_compatible_types <- function(username, types) {
    compatible <- og %>%
      filter(Discord_username == username) %>%
      select(c(types)) %>%
      unlist()
    return(types_list[compatible])
  }
  
  give_types <- get_compatible_types(person, gift_give_list)
  print(paste("Give types:", paste(give_types, collapse = ", ")))
  
  #compatible give/receive
  give_get <- function(col){

    giftee <- names(row)[col]
    print(paste("Giftee:", giftee))
    
    #making sure not same person
    if(giftee == person){
      print("This is the same person. Setting to FALSE")
      return (FALSE)
    }
    
    #viable give -> get check
    get_types <- get_compatible_types(giftee, gift_receive_list)
    
    print(paste("Give types:", paste(give_types, collapse = ", ")))
    print(paste("Get types:", paste(get_types, collapse = ", ")))
    

    if (!any(get_types %in% give_types)) {
      print(paste("Setting", giftee, "to FALSE"))
    }
    
    return(any(get_types %in% give_types))
  }

  #For each column (giftee), use above to check their compatibility with row (santa)
  for (col_idx in seq_along(row)[-1]) {
    row[col_idx] <- give_get(col_idx)
  }
  
  #not in blacklist
  blacklist_entries <- blacklist$Blacklist[blacklist$Santa == person]
  blacklist_entries <- blacklist_entries[!is.na(blacklist_entries)]
  
  if (length(blacklist_entries) > 0) {
    print(paste(blacklist_entries, "blacklisted for", person))
    row[which(row_name %in% blacklist_entries)] <- FALSE
  }

  return(row)
}

#will refer back to options df for valid matches
options <- as.data.frame(t(apply(options, 1, option_maker)))
# Santa   a         b     c       d     e
# 1 a    FALSE   FALSE  FALSE   FALSE   TRUE
# 2 b    FALSE   FALSE   TRUE   FALSE  FALSE
# 3 c    FALSE    TRUE  FALSE   FALSE  FALSE
# 4 d     TRUE    TRUE   TRUE   FALSE   TRUE
# 5 e     TRUE    TRUE   TRUE    TRUE  FALSE



## COLS FOR MULT ENTRIES
#add multiple entries again to all santa *entries*
add_mult_entries <- function(row) {
  x = data.frame(name = rep(row[1], row[3]))
  if (row[3]>1) {print(paste(row[1], "added", row[3], "times"))}
  return(x)
}

# Apply the function and combine results into a single data frame
start_santas <- apply(all_santas, 1, add_mult_entries) %>%
  bind_rows()

start_giftees <- start_santas


# MATCH THING ----------------------------------------------------------------

remaining_santas <- start_santas
remaining_giftees <- start_giftees

## FANDOM
#prioritize people with fandoms in common, deal with rest in later functions

temp_remaining_santas <- remaining_santas
temp_remaining_giftees <- remaining_giftees
temp_match <- data.frame(Santa = character(),
                         Giftee = character())


fandom_match <- function(santas,
                         giftees,
                         match){
  randlist <- sample(santas$name)

  clean_fandom <- function(fandom_string) {
    cleaned_string <- gsub("[^a-zA-Z]", "", toupper(fandom_string))
    return(cleaned_string)
  }
  
  for(i in randlist){
    
    fandom_ready <- function(x){
      x_fandoms <- all_santas$Fandoms[all_santas$Santa == x]
      x_fandoms <- unlist(str_split(x_fandoms, horrible_sep))
      x_fandoms <- clean_fandom(x_fandoms)
      return (unique(x_fandoms))
    }
    
    i_fandoms <- fandom_ready(i)
    
    print(i)
    print(i_fandoms)
    
    elist <- sample(giftees$name)
    print(elist)
  
    for(e in elist){
      
      #check not in blacklist
      if(!as.logical(options[options$Santa == i, e])){
        print(paste(e, "not viable giftee for", i))
        next
      }
      if(length(match$Giftee[match$Santa == i]) > 0 &&
         i %in% match$Santa){
        temp_filter <- match[match$Santa == i, "Giftee"]
        if(e %in% temp_filter){
          print(paste(e, "already giftee for", i))
          next
        }
      }
      
      e_fandoms <- fandom_ready(e)
      print(e)
      #print(e_fandoms)
      
      if(any(i_fandoms %in% e_fandoms)){
        match[nrow(match)+1, ] = c(i,e)
        santas <- santas %>%
          filter(!(name == i & row_number() == which(name == i)[1]))
        giftees <- giftees %>%
          filter(!(name == e & row_number() == which(name == e)[1]))
        
        print(paste(i, "will make something for", e, "because of shared fandom"))
        break
      }
      
    }
  }
  temp <- list(santas,
               giftees,
               match)
  
  print("Success")
  return(temp)
}

temp_fandom <- fandom_match(temp_remaining_santas,
                            temp_remaining_giftees,
                            temp_match)
temp_remaining_santas <- temp_fandom[[1]]
temp_remaining_giftees <- temp_fandom[[2]]
temp_match <- temp_fandom[[3]]
temp_match
#       Santa Giftee
# 1        j p
# 2        a k
# 3        e d
# 4        p j
# 5        k a
# 6        t h
# 7        t v
# 8        t b
#temp_remaining_giftees
# s
# s
# h
# e
# t
# t
# t
# f
# y
# p
# i
# i
#temp_remaining_santas
# s
# s
# h
# d
# f
# y
# p
# i
# i
# v
# b
# k


## VERIFICATION

#Any low counts?
did_you_break_it <- function(santa_df,
                             giftee_df,
                             match_df
                             ){
  
  for(santa in santa_df$name){
    santa_row <- options[options$Santa == santa, -1]
    remaining_options <- colnames(options)[-1][as.logical(santa_row)]
    remaining_options <- remaining_options[remaining_options %in% giftee_df$name]
    already_matched <- match_df$Giftee[match_df$Santa == santa]
    remaining_options <- remaining_options[!remaining_options %in% already_matched]
    
    if(length(remaining_options) == 0){
      print(paste("Warning!!!!!", santa, "has NO valid options left!"))
    }
    else if(length(remaining_options) < 3){
      print(paste(santa, "only has these options left:", remaining_options))
    }
  }
  
  for(giftee in giftee_df$name){
    giftee_col <- options[, giftee]
    #print(giftee_col)
    
    remaining_elves <- options$Santa[as.logical(giftee_col)]
    #print(remaining_elves)
    
    remaining_elves <- remaining_elves[remaining_elves %in% santa_df$name]

    already_matched <- match_df$Santa[match_df$Giftee == giftee]
    remaining_elves <- remaining_elves[!remaining_elves %in% already_matched]

    if(length(remaining_elves) == 0){
      print(paste("Warning!!!!!", giftee, "has NO valid santas left!"))
    }
    else if(length(remaining_elves) < 3){
      print(paste(giftee, "only has these santas left:", remaining_elves))
    }
  }
  
}

did_you_break_it(temp_remaining_santas, temp_remaining_giftees, temp_match)
# [1] "Warning!!!!! s has NO valid santas left!"
# [1] "Warning!!!!! h has NO valid santas left!"
# [1] "d only has these santas left: a"


#deletion restoration
kill_the_match <- function(match_df, santa_df, giftee_df, name1, name2) {
  row <- which(match_df[, 1] == name1 & match_df[, 2] == name2)
  
  if (length(row) > 0) {
    print(paste(name1, "->", name2, "found"))
    
    match_df <- match_df[-row, ]
    santa_df <- rbind(santa_df, match_df[row, 1])
    giftee_df <- rbind(giftee_df, match_df[row, 2])
    
    print(paste(name1, "->", name2, "removed"))
  }
  
  else {
    print(paste(name1, "->", name2, "not found"))
  }
  
  edited <- list(match_df, santa_df, giftee_df)
  return(edited)
}

edits <- kill_the_match(temp_match,
               temp_remaining_santas,
               temp_remaining_giftees,
               "santa",
               "giftee")
edit_match <- edits[[1]]
edit_santa <- edits[[2]]
edit_giftee <- edits[[3]]


## MANUAL ADJUSTING GOES HERE



## MERGE ONCE HAPPY
remaining_santas <- temp_remaining_santas
remaining_giftees <- temp_remaining_giftees
current_match <- temp_match


## RANDOM
#assigning the remaining santas + giftees to each other

rando_match <- function(santas,
                         giftees,
                         match){
  randlist <- sample(santas$name)
  
  for(i in randlist){
    print(i)

    elist <- na.omit(sample(giftees$name))
    print(elist)
    
    elist <- elist[sapply(elist, function(e) {
      # Is it valid option?
      valid_cell <- as.logical(options[options$Santa == i, e])
      print(valid_cell)
      
      # Is it already matched?
      no_match <- !(any(match$Santa == i & match$Giftee == e))
      print(no_match)
      
      valid_cell && no_match
    })]
    print(elist)
    
    if (length(elist) == 0){
      print("Oh no! 0 giftees possible for santa. Run me again with better luck?")
      return()
    }
    
    for(e in elist){
      
      #matched
      match[nrow(match)+1, ] = c(i,e)
      santas <- santas %>%
        filter(!(name == i & row_number() == which(name == i)[1]))
      giftees <- giftees %>%
        filter(!(name == e & row_number() == which(name == e)[1]))
      
      print(paste(i, "will make something for", e))
      break
      }
      
  }
  
  final_match <- match
  print("Success")
  return(final_match)
}

my_match <- rando_match(remaining_santas,
                          remaining_giftees,
                          current_match)


## MANUAL ADJUSTING AGAIN



## MERGE AGAIN ONCE HAPPY
match <- my_match

match
#       Santa Giftee #first rows are from fandom match!
# 1        j p
# 2        a k
# 3        e d
# 4        p j
# 5        k a
# 6        t h
# 7        t v
# 8        t b
# 9        i p
# 10       i f             
# 11       c s             
# 12       h t                           
# 13       o e                          
# 14       e h     
# 15       y t 
#...       ...

# Emails ------------------------------------------------------------------

email_ctrl_c_ctrl_v_csv <- function(row) {
  print("STARTING NEW")
  name = row[4]
  print(name)
  if (name %in% email_csv[1]) next #no dupes in case mult entries
  
  greet <- paste0("Hi ", name, ", this is an update on our Secret Santa Gift Exchange! 🎅 We have your giftee information ready:")
  
  just_matches <- match %>%
    filter (Santa == name)
  print(just_matches)
  
  santa_count <- nrow(just_matches)
  print(santa_count)
  if (santa_count > 1) { #check if in final_match_csv twice. if so:
    mult <- paste("\n\nWhen filling out the signup you said you'd be down to do",
                  santa_count,
                  "x Secret Santas, so you'll create gifts for *",
                  as.english(santa_count),
                  "* giftees and you'll receive *",
                  as.english(santa_count),
                  "* gifts!")
  }
  
  custom_all <- NULL
  
  for (i in 1:santa_count) {
    if (santa_count > 1) {
      xth <- paste0("\n\n\n✨ Your ", ordinal(i), " giftee is:")
    }
    else {
      xth <- "\n\n\n✨ Your giftee is:"
    }
    
    giftee_name <- just_matches$Giftee[i]
    print(giftee_name)
    giftee_df <- og %>%
      filter (Discord_username == giftee_name)

    giftee_charas <- giftee_df %>%
      select(c("Character_1",
               "Character_2",
               "Character_3",
               "Character_4",
               "Character_5",
               "Character_6")) %>%
      unlist()
    giftee_fandoms <- giftee_df %>%
      select(c("Source_1",
               "Source_2",
               "Source_3",
               "Source_4",
               "Source_5",
               "Source_6")) %>%
      unlist()
    
    message_twice <- paste(xth, "**", giftee_name,"**")

    charas_fandos <- paste0("> ", giftee_charas, ": ", giftee_fandoms, collapse = "\n")
    charas_fandos <- paste("\n\nHere are the six characters and the source material that they wrote on their wishlist in order:\n",
                           charas_fandos)

    types_index <- giftee_df %>%
      select(c(gift_receive_list)) %>%
      unlist()
    types <- types_list[types_index]
    giftee_types_formatted <- paste0("• ", types, collapse = "\n")
    happyto <- paste("\n\nThey're happy to receive gifts of these types:\n", giftee_types_formatted)

    pref_pull <- giftee_df %>%
      select(receive_pref) %>%
      unlist()
    if (!is.na(pref_pull)) {
      pref <- paste0("\n\nAny preferences they had:\n", "*", pref_pull, "*")
      happyto <- paste(happyto, pref)
      }
    
    custom <- paste(message_twice,
                    charas_fandos,
                    happyto
                    )
    custom_all <- paste(custom_all, custom)
  }

  close <- paste0("\n\n\nAnd now you have all the info you need to get creating!! 🖊️ You only gotta make 1 \"thing\" per person, pick any character from their wishlist and do your best!",
  "\nAs a reminder you have until **December 21st to finish your gift(s)** *unless* you're creating a craft/physical piece, in which case you'll have until Dec 8th to finish. We'll **check in on Dec 6th** to make sure you feel like you're on track to have your gift ready and again on the 21st to confirm you're done. More info about how to share your creation with your giftee at checkin! 🎁",
  "\n\nThanks for participating, we're super excited to see what everyone whips up :D",
  "\n\nWe know this was a lot,,,, have any questions, concerns, have you changed your username? Feel free to reach out to the person you dmed you this or by email.",
  "\n\n# IMPORTANT:",
  "\nTo confirm you've received the info and you're still interested and available, please reply to this message with the name of your giftee(s) or react to this message with the first letter of their username(s) within 72 hours, otherwise we'll assume you can't participate and have dropped out and we'll go fix santa-giftee pairings'")
  
  concat_message <- paste(
    greet,
    custom_all,
    close
  )
  print(concat_message)
  return(c(name, concat_message))
}

email_csv <- data.frame(Santa = character(),
                        Message = character(),
                        stringsAsFactors = FALSE)
email_csv <- t(apply(og, 1, email_ctrl_c_ctrl_v_csv))
email_csv <- data.frame(Santa = email_csv[, 1], Message = email_csv[, 2], stringsAsFactors = FALSE)

#write_csv(email_csv, "/whereyouwanttosave.csv")
