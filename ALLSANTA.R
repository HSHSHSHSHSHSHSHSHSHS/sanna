library(tidyverse)
library(readxl)
library(english)
rm(list = ls())

# prep --------------------------------------------------------------------

og <- read_xlsx("/Users/weeb/Downloads/*data proj/Santa/MSAC Secret Santa Creative Fanwork Exchange 2024.xlsx")
og <- og %>%
  rename(Discord_username = `Discord username`)
og <- og[-18,]
  
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

# Matches -----------------------------------------------------------------


Input CSV
- Name
- What they give (cols, TF)
- What they get (cols, TF)
- All sources in a list w commas to break
- Notes on give prefs

Script (R)
- Any excludes?
    - Create a new df (list w commas to break), this person won’t be matched with any names in that column
- Create a new df (empty) tracker
    - Cols Santa -> giftee
- Create a new df  (all Santas), new df (all giftees)
- Create a df where col 1 is Santa col 2 is all compatible (based on give receive align and not in blacklist

- For row in compatinle: for thing in list, if thing in [any other row] and give[index] true and receive[index]true, new df remove row where x2 and append to tracker
    - And remove from compatible, name and row
    - Probably go in fandom order, all of source 1 not person a 1-6??
    - Print Santa: x, giftee: y because shared fandom [thing], gift options:
- Stop if 0 compats
- Run it back:
    - Put people back if not happy with, typing name 1 then name 2 resets the 4 dfs
- Print compatible
    - Warning message for if anyone left now has 1-2 compatible
- And a line for manual intervention
    - And other prefs

- Random hell scape
- Multiply total pairings: if under threshold infinite loop generate dfs store valid ones in a list
    - If over, start with lowest names and randomly pick one going up to people w most. If at any point 0 then reset
        - If hits end ping « manual fix needed » explain person problem

- Second round hell for mults
- Only look at separate df??
    - Need to consider 3 entries aka 2 in df, maybe put it as also name cannot be the same, OR put them in their own blacklist??? lmfao
    - Check dupes also!! Against tracker so not giving same person 2+ gifts




match <- read_xlsx("/Users/weeb/Downloads/*data proj/Santa/MSAC 2024 Secret Santa Tracker.xlsx", sheet = "Track")
match <- match[,c(1,3,6)]

# Emails ------------------------------------------------------------------

email_ctrl_c_ctrl_v_csv <- function(row) {
  print("STARTING NEW")
  name = row[4]
  print(name)
  if (name %in% email_csv[1]) next #no dupes
  
  greet <- paste0("Hi ", name, ", this is an update on MSAC's Secret Santa Gift Exchange! 🎅 We have your giftee information ready:")
  
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

    giftee_charas <- giftee_df %>% ##need to make this into a list??
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
      }
    
    custom <- paste(message_twice,
                    charas_fandos,
                    happyto,
                    pref)
    custom_all <- paste(custom_all, custom)
  }

  close <- paste0("\n\n\nAnd now you have all the info you need to get creating!! 🖊️ You only gotta make 1 \"thing\" per person, pick any character from their wishlist and do your best!",
  "\nAs a reminder you have until **December 21st to finish your gift(s)** *unless* you're creating a craft/physical piece, in which case you'll have until Dec 8th to finish. We'll **check in on Dec 6th** to make sure you feel like you're on track to have your gift ready and again on the 21st to confirm you're done. More info about how to share your creation with your giftee at checkin! 🎁",
  "\n\nThanks for participating, we're super excited to see what everyone whips up :D",
  "\n\nWe know this was a lot,,,, have any questions, concerns, have you changed your username? Feel free to reach out to the person you dmed you this or by [contact form](<https://tally.so/r/m6QvW5>)",
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

write_csv(email_csv, "/Users/weeb/Downloads/*data proj/Santa/export.csv")
