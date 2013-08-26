library(shiny)
library(XML)

make_table_from_file = function(filename)
{
	data = read.csv(filename,header=T)
	rownames(data) = make.unique(data[,1],sep="")
	colnames(data) = gsub("\\.","",colnames(data))
	data = data[,2:ncol(data)]
	data
}

make_xtable = function(table)
{
	xtable(table)
}

make_phonemes_from_file = function(filename)
{
	ret = new.env()
	lines = readLines(filename)
	#TODO do this the functional way
	for (line in lines)
	{
		#TODO skip blank lines and such
		tokens = strsplit(line,"\\s+")[[1]]
		#ret[["a"]] = c(ret[["a"]],tokens[1])
		for(i in 2:length(tokens))
			# add phoneme to list of phonemes with that feature
			ret[[ tokens[i] ]] = c(ret[[ tokens[i]  ]], tokens[1])
	}
	ret[["?"]] = ".*"
	ret
}

#converts a set of features to a set of phonemes with that +feature
features_to_phonemes = function(features,phonemes)
{
	unique(unlist(lapply(features,function(feature) phonemes[[feature]])))
}
phonemes_to_bundle = function(phonemes)
{
	paste0("(",paste(phonemes,collapse="|"),")")
}


make_tree_from_file = function(filename)
{
	xmlParse(filename)
}

getChildrenNames = function(xml, node.names)
{
	unique(unlist(sapply(node.names,function(name)
						 {
							 xpathSApply(xml, paste0("//*/",name,"/*"),xmlName)
						 })
	))
}

getCities = function(tree,group.name=NA)
{
	if (2>length(group.name) && is.na(group.name)) group.name = xmlName(xmlRoot(tree))
	unique(c(
			 xpathSApply(tree,paste0("//",group.name,"//*[not(*)]"),xmlName),
			 xpathSApply(tree,paste0("//",group.name,"[not(*)]"),xmlName)
			 #this is retarded hack since first expression doesn't return anything if group.name is leaf
			 ))
}

getGroupNames = function(tree,group.name=NA)
{
	if (is.na(group.name)) group.name = xmlName(xmlRoot(tree))
	others = unique(xpathSApply(xmlRoot(tree),paste0("//",group.name,"//*"),xmlName))
}

#.GlobalEnv$phonemes = make_phonemes_from_file("features.txt")
#.GlobalEnv$raw_table = make_table_from_file("data.csv")
#.GlobalEnv$tree = make_tree_from_file("tree.xml")
