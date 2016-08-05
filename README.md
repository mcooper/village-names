# village-names

I want to classify village names in west Africa, based on n-gram similarity and spatial distance.

About a year ago, I started fooling around with a dataset of toponyms from [genames.org](geonames.org).  I found lots of cool patterns around toponym prefixes and suffixes that I was familiar with, and I would like to use machine learning to classify all of the villages, presumably revealing ethnic group boundaries and affinities.  To see the results of my original project, check out the [shiny app](https://amadoukone.shinyapps.io/DuguTogo/) I made, and a [blog post](http://theresalwaysmoretolearn.blogspot.com/2015/02/after-living-in-rural-village-in-mali.html) about why I think this kind of work could be useful.

With the hopes that I’ll pick up this project and work on it some more, I’m dumping all my old scripts into a github repo.  They might not be very well sorted.

The workflow is:

1. Normalize village names to try to remove French and English differences in spelling, so ‘gn’ -> ‘ny’ or ‘bougou’ -> ‘bugu’

2. Find all n-grams (in this case 3-grams) that show significant spatial clustering.

3. Using a dataset with latitude, longitude, and a binary presence-absence variable for every village name and n-gram, classify all of the village names using ML.
