j=1
for file in apa_articles/*
do
 # Extract doi
 doi_extract=$(grep -o "Digital Object Identifier.*10\\..*</p>" $file | grep -o "[0-9]\{2,\}\.[0-9]\{4,\}/\([a-zA-Z0-9]\{1,\}.\?\)\{1,\}" | sed 's/<$//g')
 # Check whether all are available
 if [ -z "$doi_extract" ]; then
  doi_extract=$(echo nodoi$j)
  let "j++"
 fi
 doi_extract=$(echo $doi_extract | sed 's/\//_/g')
 mkdir -p apa_articles/$doi_extract
 mv $file apa_articles/$doi_extract/fulltext.html 
done
