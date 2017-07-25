for art in $(ls apa_articles)
do
infile=$(echo apa_articles/$art/fulltext.txt)
outfile=$(echo apa_articles/$art/results.csv)

doi=$(echo $art)

cat $infile | grep -oP "\s_?p_?\s*[=<>]\s*\d?\.\d*" | sed 's/_//g' | sed 's/ //g' > result

x=$(cat result | wc -l)
for i in `seq $x`; do echo $doi;done > doi

cat fulltext.txt | grep -oP ".{200}\s_?p_?\s*[=<>]\s*\d?\.\d*" | grep -oP ".{200}" > pre
cat fulltext.txt | grep -oP "\s_?p_?\s*[=<>]\s*\d?\.\d*.{200}" | grep -oP ".{200}" > post




pre
result
post
comparison
value

rm doi pre result post comparison value
done
