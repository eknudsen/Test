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



