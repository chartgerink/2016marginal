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

# Correct two erroneous folders.
mv apa_articles/10.1037_0278-6133.27.2\(Suppl. apa_articles/10.1037_0278-6133.27.2\(Suppl.\).S180
mv apa_articles/10.1037_0278-6133.27.3\(Suppl. apa_articles/10.1037_0278-6133.27.3\(Suppl.\).S271