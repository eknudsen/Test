# Eliot Knudsen
# NBC Methods

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