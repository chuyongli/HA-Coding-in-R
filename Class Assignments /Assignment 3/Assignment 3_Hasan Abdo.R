# (TP) Great job on your assignment, Hasan! I appreciated that you added some comments at the top of your code to provide 
# (TP) a note to whomever reads the code for making sure that the .txt file is within working directory. I also liked 
# (TP) that you provided the source of the words used in the text file. Your code is very well-structured and easy to follow. Your variables are
# (TP) also very clearly named and descriptive, helping with readability. I liked that you created functions ahead of time to comoplete specific tasks, which 
# (TP) enhanced the readability for the main code as well. I have just a few comments below that may enhance the efficiency and readability of the code. 
# (TP) However, overall, your code functions as needed, controlled the user input well, and fulfilled the requirements of the Hangman game. Amazing work!

#Coding in R Assignment 2 - Hasan Abdo

# (TP) It wasn't a requirement for this assignment but you could potentially add a step-by-step plan here.

#Created a dictionary of words and saved it as a .txt file. 
#File name is words.txt
#Note to reviewer, you may want to make sure the file words.txt is within your working directory for this code to run.
#Words generated using a random word generator found on https://www.randomlists.com/random-words , I did add a couple of my own words as well!


#Creating a function which will read the words from .txt file. Can call on this function later.
read_words <- function() {
#Reading the word list into the program using read.table, where our words are placed in coloumn called words
  wordfile <- read.table("words.txt", header = FALSE, stringsAsFactors = FALSE, col.names = "words")
#Function returns vector of strings from the column words, so should return the word list. 
  return(wordfile$words)
  
} #read_words function ends here


#Created a function to sample random word from the list
random_word_select <- function() {
#Placing return value of function "read_words" into a variable
  word_list <- read_words()
#This function returns a random word from the vector "word_list" using sample function
  return(sample(word_list, 1))
  
} #Random_word_select function ends here 


#Listing variables for main game loop +welcoming User to the game
#Selecting a random word from the word list to give to the user, running function "random_word_select" and storing it as vector into variable random word for later use
random_word <- random_word_select()
#Created a vector version of the random word which will help later on in checking for guess conditions.
#Unlist function changes the list resulting from strsplit function to a vector, reassigns it to variable.
random_word_vector <- unlist(strsplit(random_word, ""))
#Using nchar to find the length of the word
word_length <- nchar(random_word)

# (TP) I appreciate that you specified your logic behind the number of guesses the player has. However, in this case, you 
# (TP) mentioned it as the max number of guesses, whereas your variable name suggests it to be the maximum number of wrong
# (TP) guesses. Consider renaming the variable or adjusting the comment to reflect its purpose more accurately, so that it 
# (TP) can ensure clarity, consistency, and improved readability. 
#For each round of the game, I intend to give the User max guesses that depend on the length of the word, so word length plus 3 (in terms of guesses). 
max_wrong_guesses <- word_length + 3

#Welcome user and tell them of length of their word
#Inform the user about the number of wrong guesses allowed
cat("Welcome to Hangman\nYour word has", word_length, "letters.\n")
cat("You are allowed", max_wrong_guesses, "wrong guesses.\n")

# (TP) I appreciate the idea of tracking the user guesses - this can help with the user experience with the game!
#Creating vector variables for User guesses, variables now empty, but the idea is to add to each category depending on input from the User
#Creating an empty vector to store correct letters guessed by the User
correct_guesses <- character(0)  
#Creating an empty vector to store wrong letters guessed by the user
wrong_guesses <- character(0)  
#Create a vector with the number of attempts left equal to the maximum allowed guesses, here it is set to the max guesses as it is the start of the game still. 
attempts_left <- max_wrong_guesses  


# (TP) I like that you created a function with checks implemented to ensure that only specific user inputs are accepted. 
# (TP) Specifically, handling upper and lower cases and ensuring that it is a single letter within the alphabet. One
# (TP) suggestion: Instead of using `break`, consider using a loop that naturally exits when certain conditions are met or
# (TP) unmet, rather than force breaking it. For instance, using a while loop to continue prompting the user until a valid
# (TP) input is provided. Nonetheless, this function is great and improves the robustness of the code.

#Taking user guesses/input
#Creating a function for user guesses, which will check whether the user only inputs a single number, and an alphabetical letter.
user_guess <- function() {
#Adding a repeat loop so the User is prompted till they enter a valid letter
   repeat {
#Adding user input and assigning it to variable "guess", "tolower" function makes sure the values entered are lowercase
    guess <- tolower(readline(prompt = "Please enter a single letter: "))
#If statement to check whether input is 1 character and is a letter from a-z, loop breaks if this condition checks
    if (nchar(guess) == 1 && grepl("[a-z]", guess)) {
      break #break the loop
#If condition is not satisfied, loop continues and asks for valid input.
    } else {
      cat("Invalid input. Please enter a single letter: ")
    }
   }
#After breaking loop, the function will return the valid value for "guess" variable.
  return(guess) 
}

#Creating a vector with same size as the word, and will update this later in the Main Game loop as the User progresses through the game
#Rep function here will replicate underscores for the length of the word. This is to be updated as the game goes along to show the user the word they are guessing. 
current_state <- rep("_", word_length)

#Main game loop 
#While loop that will run as long as the attempts_left variable is greater than 0
while (attempts_left > 0) {

# Get user input, relates back to the created function "user_guess" and assigns it to the "guess" variable.
  guess <- user_guess() 

# (TP) I see your logic in creating the random_word_vector and it does make sense. One suggestion would be to use the
# (TP) grepl() function to check if guess is in random_word directly, instead of creating another vector. It makes the
# (TP) code simpler and reduces number of lines.
  
#Check if the "guess" variable or User input is correct or wrong
#If statement explaining what to do if the User inputs "guess" which is verified by being correct if it is in "random_word_vector" created previosly.
#The operator "%in% is used here to check whether the vector "guess" (since its one letter), is present in the vector "random_word_vector" which is a vector of the characters of the chosen word. This is why creating "random_word_vector" was useful.
  if (guess %in% random_word_vector) {
#Prints message
    cat("Correct guess! The letter", guess, "is in the word.\n")

# (TP) This suggestion is for correct_guesses. Consider adding an additional check to see if the letter has been previously guessed! When running your code, 
# (TP) I saw that when I guessed m a second time, it was added again to the list of correct guesses, which then showed two m's. While for wrong guesses, this
# (TP) did not occur as I noticed you implemented this check for wrong_guesses below. It's not a huge concern, but it may improve the user experience and 
# (TP) clarity of the game flow.
#Adds this guess to the "correct_guesses vector, updating it. Useful for later on when showing game status.
    correct_guesses <- c(correct_guesses, guess)

# (TP) This approach is very thorough and I appreciate that you update the current_state so that the user can have a
# (TP) visual sense of their progress! One suggestion: consider using the which() function to obtain the index or indices
# (TP) of where the guessed letter matches the letters in the word. This way, you can avoid using a loop!
#For loop iterates for every i position in the chosen word
    for(i in 1:word_length){
#If statement indicating if guess or the User input matches with the random word at position "i", remember, this is the random word vector, which split the word.
      if(guess == random_word_vector[i]){
#When the if condition holds True, I want to update the "current_state" vector at its "i" position with the guess or User input.
#This solves the issue of having to guess the word in the exact sequence of letters. For example, if the word is: c-a-t, I can still start by guessing "t" instead of "c" and it will update the word in the current game status appropriately. 
        current_state[i] <- guess
      }
    }

# (TP) From your comments above and the instruction provided to the user, it seemed like an attempt would only be lost if
# (TP) they guessed an incorrect letter. Subtracting an attempt for a correct guess might be then confusing for the player.
# (TP) Consider only reducing attempts for incorrect guesses or adjusting your instructions.
#Subtracts an attempt for every correct guess
    attempts_left <- attempts_left - 1

#New wrong guess, checking if User input letter "guess" is not in random_word_vector (indicating by !) AND that is has not already been guessed (so letter is in wrong_guesses variable)
  } else if (!(guess %in% random_word_vector) && !(guess %in% wrong_guesses)) {
#Printing message
    cat("Wrong guess! The letter", guess, "is not in the word.\n")
#Updating wrong_guesses vector with the new guess/User input which met the above conditions
    wrong_guesses <- c(wrong_guesses, guess)
#Subtracting an attempt from a wrong guess 
    attempts_left <- attempts_left - 1

# (TP) I really appreciate this extra else statement that informs the user that they had already guessed the letter. Nice touch!
#Repeated wrong guess, prints a message to let user know. This doesn't take away an attempt.
  } else {
    cat("You already guessed this letter", guess, ". Try again.\n")
  }

# (TP) Rather than having three conditional blocks, a suggestion is to remove the second conditional block and simply keep the first without altering 
# (TP) the number of attempts_left. For the condtional block that checks if they didn't win, you can simply remove the current_state != random_word_vector.
# (TP) That would have already been checked inherently by the previous conditional block. Instead, you can make the code more straightforward by only
# (TP) checking for the number of attempts. These changes would simplify the code, make it more efficient, and improve readability overall.
  
#Check if the word has been completely guessed
#All checks if all elements within the logical vector are true.
  if(all(current_state == random_word_vector)){
#If User wins on the last try they can still win, so if they have 1 try left and they guess correct, it adds an attempt and still notifies them they won. 
    attempts_left <- attempts_left + 1 
    cat("Congratulations! You have successfully completed the game!")
    break #breaks loop
  }
  
#Check if they won, print win message then break
  if(all(current_state == random_word_vector) && attempts_left < max_wrong_guesses){
    cat("Congratulations! You have successfully completed the game!")
    break #breaks loop
  }
  
#Check of they didn't win
  if(all(current_state != random_word_vector) && attempts_left == 0){
    cat("Game is over, better luck next time!")
    break #breaks loop
  }
  
  
  #Updating the user with current state of the game as it goes using printed messages
  cat("Word: ", current_state, "\n")
  
  #Printing out messages to update User with the state of the game as it progresses
  cat("Correct guesses: ", paste(correct_guesses, collapse = " "), "\n")
  cat("Wrong guesses: ", paste(wrong_guesses, collapse = " "), "\n")
  cat("Attempts left: ", attempts_left, "\n")
  
}


    
