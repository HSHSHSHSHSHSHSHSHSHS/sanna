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

all_santas <- data.frame(Santa = rep(NA,num_participants),
                         Fandoms = rep(NA,num_participants),
                         Entries = rep(NA, num_participants))
for (type in types_list) {
  all_santas[[type]] <- NA
}
all_santas$Santa = og$Discord_username
all_santas$Fandoms = paste(og$Source_1,
                           og$Source_2,
                           og$Source_3,
                           og$Source_4,
                           og$Source_5,
                           og$Source_6,
                           sep = horrible_sep
                           )
all_santas$Entries = og$entries_count

all_giftees <- all_santas %>%
  rename(Giftee = Santa)

all_santas[4:10] = og[c(gift_give_list)]
all_giftees[4:10] = og[c(gift_receive_list)]

## BLACKIST
blacklist <- data.frame(Santa = og$Discord_username,
                        Blacklist = rep(NA, num_participants))

#manual blacklist adding goes here

## CREATING POTENTIAL MATCHES
options <- data.frame(Santa = og$Discord_username) #df, col 1 is santa cols 2-n are giftee names w TF
for (user in unique_participants) {
  options[[user]] <- NA
}

option_maker <- function(row){
  person <- row[1]
  print(person)
  
  row_name <- names(row)
  
  types_index <- og %>%
    filter(Discord_username == person) %>%
    select(c(gift_give_list)) %>%
    unlist()
  give_types <- types_list[types_index]
  print(paste("Give types:", paste(give_types, collapse = ", ")))
  
  #compatible give/receive
  give_get <- function(col){
    returner <- TRUE
    
    giftee <- names(row)[col]
    print(paste("Giftee:", giftee))
    
    giftee_types_index <- og %>%
      filter(Discord_username == giftee) %>%
      select(c(gift_receive_list)) %>%
      unlist()
    get_types <- types_list[giftee_types_index]
    
    print(paste("Get types:", paste(get_types, collapse = ", ")))
    

    if (!any(get_types %in% give_types)) {
      returner <- FALSE
      print(paste("Setting", giftee, "to FALSE"))
    }
    
    return(returner)
  }

  for (col_idx in seq_along(row)[-1]) {
    row[col_idx] <- give_get(col_idx)
  }
  
  #making sure not same person
  col_index <- which(row_name == person)
  if (length(col_index) > 0) {
    row[col_index] <- FALSE
  }
  
  #not in blacklist
  blacklist_entries <- blacklist$Blacklist[blacklist$Santa == person]
  blacklist_entries <- blacklist_entries[!is.na(blacklist_entries)]
  
  if (length(blacklist_entries) > 0) {
    #print(blacklist_entries)
    row[which(row_name %in% blacklist_entries)] <- FALSE
  }

  return(row)
}

#will refer back to options df for valid matches
options <- as.data.frame(t(apply(options, 1, option_maker)))


## COLS FOR MULT ENTRIES
#add multiple entries again to all santa *entries*
add_mult_entries <- function(row) {
  x = data.frame(name = rep(row[1], row[3]))
  return(x)
}

# Apply the function and combine results into a single data frame
start_santas <- apply(all_santas, 1, add_mult_entries) %>%
  bind_rows()

start_giftees <- start_santas


# MATCH THING ----------------------------------------------------------------

match <- data.frame(Santa = character(),
                    Giftee = character())

remaining_santas <- start_santas
remaining_giftees <- start_giftees

## FANDOM
#prioritize people with fandoms in common, deal with rest in later functions

temp_remaining_santas <- remaining_santas
temp_remaining_giftees <- remaining_giftees
temp_match <- match

fandom_match <- function(santas,
                         giftees,
                         match){
  randlist <- sample(santas$name)

  clean_fandom <- function(fandom_string) {
    cleaned_string <- gsub("[^a-zA-Z]", "", toupper(fandom_string))
    return(cleaned_string)
  }
  
  for(i in randlist){
    i_fandoms <- all_santas$Fandoms[all_santas$Santa == i]
    i_fandoms <- unlist(str_split(i_fandoms, horrible_sep))
    i_fandoms <- clean_fandom(i_fandoms)
    
    print(i)
    #print(i_fandoms)
    
    elist <- sample(giftees$name)
    print(elist)
  
    for(e in elist){
      
      #check not in blacklist
      if(options[options$Santa == i, e] == FALSE){
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
      
      e_fandoms <- all_santas$Fandoms[all_santas$Santa == e]
      e_fandoms <- unlist(str_split(e_fandoms, horrible_sep))
      e_fandoms <- clean_fandom(e_fandoms)

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

## VERIFICATION

#any low counts?
did_you_break_it <- function(santa_df,
                             giftee_df,
                             match_df
                             ){

  for(santa in santa_df$name){
    santa_row <- options[options$Santa == santa, -1]
    remaining_options <- colnames(options)[-1][santa_row == TRUE]
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
    remaining_elves <- options[1, giftee_col == TRUE]
    remaining_elves <- remaining_elves[remaining_elves %in% santa_df$name]
    already_matched <- match_df$Santa[match_df$Giftee == giftee]
    remaining_elves <- remaining_elves[!remaining_elves %in% already_matched]
    
    if(length(remaining_options) == 0){
      print(paste("Warning!!!!!", giftee, "has NO valid santas left!"))
    }
    else if(length(remaining_options) < 3){
      print(paste(giftee, "only has these santas left:", remaining_elves))
    }
  }
  
}

did_you_break_it(temp_remaining_santas, temp_remaining_giftees, temp_match)

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
      # Check if the cell in 'options' where row = i and column = e is TRUE
      valid_cell <- options[options$Santa == i, e] == TRUE
      
      # Ensure there is no match where Santa = i and Giftee = e
      no_match <- !(any(match$Santa == i & match$Giftee == e))
      
      valid_cell && no_match
    })]
    print(elist)
    
    if (length(elist) == 0){
      print("Oh no! 0 giftees possible for santa. Run me again with better luck?")
      return("")
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

# Emails ------------------------------------------------------------------

email_ctrl_c_ctrl_v_csv <- function(row) {
  print("STARTING NEW")
  name = row[4]
  print(name)
  if (name %in% email_csv[1]) next #no dupes
  
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
               "Character_6"))
    giftee_charas <- unlist(giftee_charas)
    giftee_fandoms <- giftee_df %>%
      select(c("Source_1",
               "Source_2",
               "Source_3",
               "Source_4",
               "Source_5",
               "Source_6"))
    giftee_fandoms <- unlist(giftee_fandoms)
    
    message_twice <- paste(xth, "**", giftee_name,"**")

    charas_fandos <- paste0("> ", giftee_charas, ": ", giftee_fandoms, collapse = "\n")
    charas_fandos <- paste("\n\nHere are the six characters and the source material that they wrote on their wishlist in order:\n",
                           charas_fandos)

    types_index <- giftee_df %>%
      select(c(gift_receive_list))
    types_index <- unlist(types_index)
    types <- types_list[types_index]
    giftee_types_formatted <- paste0("• ", types, collapse = "\n")
    happyto <- paste("\n\nThey're happy to receive gifts of these types:\n", giftee_types_formatted)

    pref_pull <- giftee_df %>%
      select(receive_pref)
    pref_pull <- unlist(pref_pull)
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

#write_csv(email_csv, "/Users/weeb/Downloads/*data proj/Santa/export.csv")
