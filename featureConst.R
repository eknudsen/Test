# Eliot Knudsen
# Feature Vector Constuction Module

library(MASS)


################ Important Design Decisions #########

##### Still need to determine exactly how to pass around the dictionary
### Option 1: pass it around the different functions to keep synced
### Option 2: declare it as global --- This is the option I picked


#### Need to choose the specific pattern

##############################

#redefine global dictionary
dictionary <<- c()

#This takes in a list of vectors with bag of words
#Out puts this same list of vectors but making sure all the vectors have the same length
fill.vector = function(list.vecs){
	for ( vector in list.vecs){
		if ( length(vector) != length(dictionary)){
			vector = c(vector, rep(0, length(dictionary) - length(vector)))
		}
	}
}

# takes in a list of words and a dictionary (ie another list of words)
# returns a list of number that correspond to the dictionary
# each number is the number of times that the dictionary word appeared in the list of words
findOccurences = function(words){

	occurences = rep(0, length(dictionary))
	for (word in words){
		if (word %in% dictionary){
			#find where in dictionary the word is
			word.index = which( word == dictionary) # Should return a unique index
			
			#Increment
			occurences[word.index] = occurences[word.index] + 1
			}
		else{
			#add new word to the global dictionary
			dictionary[length(dictionary) + 1] <<- word
			occurences[length(dictionary) + 1] = 1
		}
	}

	return(occurences)
}

####### Bug Occurences tends to have 1 NA at beginning#######

#Takes in the full text of a file
#returns a vector of the words of the text
parseText = function(fullText){
	
	# Strip out <p>, spaces and punctuation besides '
	pat = "[[:alpha:][:digit:]]+"  # Not exactly sure what this pattern should be
	
	# Reg exp gibberish I don't really understand
	m = gregexpr(pat, fullText, ignore.case = TRUE)
	x = regmatches(fullText, m)
	x = do.call(c, x)
	m = regexec(pat, x, ignore.case = TRUE)
    words = regmatches(x, m)
    words = do.call(c, words)

	#lowercase stuff
	words = tolower(words)

	# words is vector of character type now
	
	return(words)
}


#This function takes in the words of a file and a dictionary
# Outputs a feature vector in the form: 
# {dictionary.word.i: occurances} - this is a vector
getVector = function(fullText){
	
	# words is now a vector of all the pure words that appear in this text
	words = parseText(fullText)
	
	# construct the vector of occurances
	occ = findOccurences(words)
	
	return (occ)
}

#Takes element to a directory -> a vector of XML data
#grabs only the useful data
#returns a string without any tags
grabFullText = function(file.root){
	
	#reads in the XML
	data.xml = readLines(file.root)
	
	# These are two unique XML tages which begin the full text
	# Note that end is 1 after the end 
	start = "<block class=\"full_text\">"
	end = "</body.content>"
	
	# Search though our xml data
	vect.start = grep(start, data.xml) + 2 # Note add 2 because you need to remove the lead
	vect.end = grep(end, data.xml) -3 # Substract 2 because you need to remove the block

	result = data.xml[seq(vect.start, vect.end)]
	
	return(paste(result, collapse = ""))
}


#Takes in a working directory of files
#Reads in all the different files and converts them into feature vectors
# returns a list of feature vectors with 1 vector as each file 
getFiles = function(directory){
	
	#constructs a list of directory of files
	files = paste(directory, list.files(directory), sep = "/")
	
	# This should return a list of full text of content
	files.fullText = lapply(files, grabFullText)

	# This should return a list of bag of words 	
	files.vectors = lapply(files.fullText, getVector)

	return(files.vectors)
}


# This function returns a list of list of feature vectors
# The first list has 4 components: train art, train music, test art, test music
# NOTE: we are not validating our training algos for overfitting
# Each element in the list contains a list of vectors
shuffle = function(){
	# These roots are user dependent
	root.art = '/Users/eliotknudsen/Documents/Fall2011/statisticalComputing/Project/nyt_corpus/art'
	root.music = '/Users/eliotknudsen/Documents/Fall2011/statisticalComputing/Project/nyt_corpus/music'
	
	# calls getFiles to construct the list of vectors	
	list.art = getFiles(root.art)
	list.music = getFiles(root.music)
	
	#Shuffle both of these lists
	
	#Split both of these lists
	
	#Bind both of these lists 
	
}




############# This is testing for all the vector constructing fucntions ###############



#This returns two lists of vectors corresponding to our two different categories
root = function(){
	
	# These roots are user dependent
	root.art = '/Users/eliotknudsen/Documents/Fall2011/statisticalComputing/Project/nyt_corpus/art'
	root.music = '/Users/eliotknudsen/Documents/Fall2011/statisticalComputing/Project/nyt_corpus/music'
	
	# calls getFiles to construct the list of vectors	
	list.art = getFiles(root.art)
	list.music = getFiles(root.music)
	
# # 	#make sure that all the vectors are the same length
	# list.art = fill.vector(list.art)
	# list.music = fill.vector(list.music)
	
	return(c(list.art, list.music))
}


#Question How exactly do we handle a dictionary
#universely defined then must construct at the very beginning