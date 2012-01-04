

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