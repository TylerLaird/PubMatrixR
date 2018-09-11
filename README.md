PubMatrixR

The default is input of a text file containing search terms. The lists should be separated with a line containing the '#' character. This splits the file into A and B lists of terms to do pairwise searches with. The API.key is an entrez eutils key which enables more searches per second (however this function does not seem to exceed the normal usage of 3 requests/second). The Database is either 'pubmed' or 'pmc' (I do not have the dates coded yet for pmc useage so it may be iffy). The daterange takes in two concatenated years if you would like to filter the search by a range of dates (example: c(2012,2017) )

