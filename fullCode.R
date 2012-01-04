########### FULL CODE ##############

#########################################
####### Feature Construction ############
#########################################


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


#Takes in the full text of a file
#returns a vector of the words of the text
parseText = function(fullText){
	
	# Strip out <p>, spaces and punctuation besides '
	pat = "[[:alpha:][:digit:]]+"  
	
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


#########################################
####### Dimensionality Reduction ########
#########################################

### This file should take in a dictionary and list of vectors
### returns a vector with the 300 most common words in no particular order

common = function(list.vecs, dictionary){
	total.words = rep(0, length(dictionary))
	for (word in seq(1,length(dictionary))){
		word.sum = 0
		for (document in list.vecs){
			if( !is.na(document[word])){
				word.sum = word.sum + document[word]
			}
		}
		total.words[word] = word.sum
	}
	cutoff = sort(total.words, decreasing = TRUE)[150]
	
	return(dictionary[total.words > cutoff])
	
}

root.art = '/Users/eliotknudsen/Documents/Fall2011/statisticalComputing/Project/nyt_corpus/art'
root.music = '/Users/eliotknudsen/Documents/Fall2011/statisticalComputing/Project/nyt_corpus/music'

files.art = getFiles(root.art)
files.music = getFiles(root.music)

common.art = common(files.art, dictionary)
common.music = common(files.music, dictionary)


uncommon = function(words.this, words.that){
	result = c()
	for (word in words.this)
	{
		if ( word %in% words.that){ 
			result = result
			}
		else {
			result = c(result, word)
		}
	}
	return(result)
}

uncommon.art = uncommon(common.art, common.music)
uncommon.music = uncommon(common.music, common.art)
uncommon = c(uncommon.art, uncommon.music)

get.word.indices = function(dictionary, uncommon){
	result = c()
	for (word in uncommon){
		result = c(result, which(dictionary == word))
	}
	return(result)
}

words.indices = get.word.indices(dictionary, uncommon)

#Takes in files and indices 
# makes the vectors only be the uncommon word occurences
small.vectors = function(files, word.indices){
	# get the indices
	result = list()
	for (file in files){
		vector.new = rep(0, length(word.indices))
		for (index in seq(1, length(word.indices))){
			word.index = word.indices[index]
			if (is.na(file[word.index])){
				vector.new[index] = 0
			}
			else{
				vector.new[index] = file[word.index]
			}
		}
		result[[length(result) + 1]] = vector.new
	}
	return(result)
}

files.art.small = small.vectors(files.art, words.indices)
files.music.small = small.vectors(files.music, words.indices)

files.number = length(words.indices)

df.art = data.frame(matrix(unlist(files.art.small), ncol = files.number))
df.music = data.frame(matrix(unlist(files.music.small), ncol = files.number))


#########################################
####### GET VECTORS FOR TESTING #########
#########################################

# These Methods grabs the exact bag of words which you are looking for
# Use on the test cases

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

getVector = function(files, dictionary){
	files.fullText = lapply(files, grabFullText)
	files.parsed = lapply(files.fullText, parseText)
	
	files.vec = list()
	
	for (file in files.parsed){
		vector = rep(0, length(dictionary))
		for (word.index in seq(1, length(dictionary)){
			word = dictionary[word.index]
			vector[word.index] = sum(file == word)
		}
		files.vec = c(files.vec, vector)
	}
}

#########################################
####### Naive Bayes Classification and Testing #########
#########################################

#These functions find the parameters for a NBC
# and then predict and test on new data frames

setwd("/Users/eliotknudsen/Documents/Fall2011/statisticalComputing/Project/")
df = read.table("dataframe30.txt")
labels = df$cat
df = df[,-31]

art.test = read.table("test.art.txt")
music.test = read.table("test.music.txt")

out.words = c("museums", "opera", "images", "march")
out.index = c(3, 9, 8, 6)

df = df[,-out.index]
art.test = art.test[,-out.index]
music.test = music.test[,-out.index]

NBC.getParam = function(df, labels){
	total.words = sum(df)
	art.param = apply(df[labels == 1,], 2, sum)
	music.param = apply(df[labels == 0,], 2, sum)
	art.param = (art.param + 1)/(total.words + 27)
	music.param = (music.param + 1) / ( total.words + 27)
	return (cbind(art.param, music.param))
}


NBC.predict = function(param, new.vector){
	
	p.value.music = sum(new.vector * log(param[,2]))
	p.value.art = sum(new.vector * log(param[,1]))
	
	if (abs(p.value.music) > abs(p.value.art)){
		return (0)
	}
	else{ return(1)}
	
}

test = function(df, labels, param){
	correct = 0
	for (index in seq(1, length(labels))){
		if (NBC.predict(param, df[index,]) == labels[index]){
			correct = correct + 1
		}
	}
	return (correct / length(labels))
}

param = NBC.getParam(df, labels)
test(art.test, rep(0, 27), param)
test(music.test, rep(1, 15), param)