for art in $(ls apa_articles)
do
infile=$(echo apa_articles/$art/fulltext.html)
outfile=$(echo apa_articles/$art/fulltext.txt)
python html2text -b 0 $infile > $outfile
done
