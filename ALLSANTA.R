library(tidyverse)
library(readxl)
rm(list = ls())

# prep --------------------------------------------------------------------

og <- read_xlsx("/Users/weeb/Downloads/*data proj/Santa/MSAC Secret Santa Creative Fanwork Exchange 2024.xlsx")
match <- read_xlsx("/Users/weeb/Downloads/*data proj/Santa/MSAC 2024 Secret Santa Tracker.xlsx", sheet = "Track")
match <- match[,c(1,3,6)]

# Matches -----------------------------------------------------------------


final_match_csv <- NULL

# Emails ------------------------------------------------------------------

email_csv <- NULL

email_ctrl_c_ctrl_v_csv <- function(og, final_match_csv) {
  #pull name
  #is name in email_csv? make sure no dupes
  greet <- paste("Hi ", name, ", this is an update on MSAC's Secret Santa Gift Exchange! 🎅 We have your giftee information ready:")
  if ("2") { #check if in final_match_csv twice. if so:
    twice <- "\n\nWhen filling out the signup you said you'd be down to do 2x Secret Santas, so you'll create gifts for *two* giftees and you'll receive *two* gifts!"
    
    #go once
    #pull giftee 1 name
    #pull giftee 1 charas -- list
    #pull giftee 1 fandoms -- list
    first_message_twice <- paste("\n\n\n✨ Your first giftee is: **", giftee_1_name,"**")
    first_charas_fandos <- paste()
    #pull giftee 1 types -- list
    #format pull types
    first_happyto <- paste("\n\nThey're happy to receive gifts of these types:", first_giftee_types_formatted)
    #pull pref
    #if pref exists:
    first_pref <- paste("\n\nAny preferences they had:", "*", first_giftee_prefs, "*")
    
    #go twice
    
    
    
    
    
    
    
    
    custom <- NULL
  }
  else {
  
    
    
    
    
    
    
    
    custom <- NULL
  }
  close <- "And now you have all the info you need to get creating!! 🖊️ You only gotta make 1 \"thing\" per person, pick any character from their wishlist and do your best!
As a reminder you have until **December 21st to finish your gift(s)** *unless* you're creating a craft/physical piece, in which case you'll have until Dec 8th to finish. We'll **check in on Dec 6th** to make sure you feel like you're on track to have your gift ready and again on the 21st to confirm you're done. More info about how to share your creation with your giftee at checkin! 🎁

Thanks for participating, we're super excited to see what everyone whips up :D

We know this was a lot,,,, have any questions, concerns, have you changed your username? Feel free to reach out to the person you dmed you this or by [contact form](<https://tally.so/r/m6QvW5>)

# IMPORTANT:
To confirm you've received the info and you're still interested and available, please reply to this message with the name of your giftee(s) or react to this message with the first letter of their username(s) within 72 hours, otherwise we'll assume you can't participate and have dropped out and we'll go fix santa-giftee pairings'"
  
  concat_message <- paste(
    greet,
    custom,
    close
  )
  
  return (concat_message)
}
