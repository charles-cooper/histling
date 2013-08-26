library(xtable)

shinyUI = pageWithSidebar(
						  headerPanel("Historical Linguistics"),
						  sidebarPanel(
									   selectInput("feature","Select phoneme 1",choices=c("")),
									   selectInput("feature2","Select phoneme 2",choices=c("")),
									   selectInput("search_groups","Select dialects to search in", choices=c("")),
									   checkboxGroupInput("display_groups","Select dialects to display", choices=c(""))
									   ),
						  mainPanel(
									textOutput("debug"),
									textOutput("error"),
									tableOutput("table")
									)
						  )


shinyServer = function(input,output,session)
{
	output$error = renderText("Fetching files failed!")
	output$table = renderTable({
		#initialize
		{
			dropbox.url = parseQueryString(session$clientData$url_search)$dropbox
			dropbox.url = sub("www","dl",dropbox.url)
			dropbox.url = sub("https","http",dropbox.url)
			dropbox.url = sub("sh","shz",dropbox.url)
			dropbox.url = paste0(dropbox.url,"/x")
			tmpfile = tempfile()
			download.file(dropbox.url,tmpfile)

			files = unzip(tmpfile,exdir=tempdir())
			data.file = files[grep("data.csv",files)]
			feature.file = files[grep("features.txt",files)]
			tree.file = files[grep("tree.xml",files)]

			raw_table = make_table_from_file(data.file)
			phonemes = make_phonemes_from_file(feature.file)
			tree = make_tree_from_file(tree.file)

			updateSelectInput(session, "feature", choices=ls(phonemes),selected=input$feature)
			updateSelectInput(session, "feature2", choices=ls(phonemes),selected=input$feature2)
			updateSelectInput(session, "search_groups",choices=c(xmlName(xmlRoot(tree)),getGroupNames(tree)),selected=input$search_groups)
			updateCheckboxGroupInput(session, "display_groups",choices=getGroupNames(tree),selected=input$display_groups)
		}

	output$error = renderText("Success!")

	grep_str = paste0(phonemes_to_bundle(features_to_phonemes(input$feature,phonemes))
					  ,phonemes_to_bundle(features_to_phonemes(input$feature2,phonemes)))
	#output$error = renderText(grep_str)

	cols = which(colnames(raw_table) %in% getCities(tree, input$search_groups))
	rows = unique(unlist(lapply(cols, function(col) {
								grep(grep_str,raw_table[,col],perl=T)
})))

	lects = unlist(unique({
		display_groups = input$display_groups
		if (0==length(display_groups))
			display_groups = xmlName(xmlRoot(tree))
		getCities(tree, display_groups)
	}))
	#TODO as.matrix is because otherwise one column case breaks for some reason. fix!!
	as.matrix(raw_table[rows,colnames(raw_table) %in% lects])
	})
}


