shinyUI(fluidPage(
  titlePanel("West African Toponym Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Search for a word or group of letters, and the map will show all villages that
               include those letters in their name. For example, many Bamaman villages include the letters 'Bougou',
               which means hut. You can see the spatial extent of Bamanan culture based on the density of points for Bougou. Similarly, 'Dougou' means
              village in many Mande languages (Bambara, Dioula, Malinke, Kuranko), and by searching for Dougou/Dugu, one can see
              the extent of the greater Mande culture. If you know any other West African langauges, good words to search for are trees, 
              last names, and geographic features like hill or river, as these are terms frequently incorporated in toponyms. Other interesting search terms include
               unusual consonant clusters found in West Africa like 'mb', 'gb', 'kp' and 'kw'. "),
      br(),         
      helpText("You can also chose to have French spellings normalized to English spellings, so Ouagadougou becomes
                Wagadugu. I hope that this kind of data could yield new insights about the distribution ethno-linguistic groups in West Africa."),
      checkboxInput('norm','Normalized Names',value=FALSE),
      textInput('str','Search for a string!',value='Bougou'),
      submitButton(text='Search')),
    mainPanel(plotOutput("map"))
  )
))