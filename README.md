# Marginal *p*-values in psychology

This project investigates the claims made in Pritschet et al. 2016 (*Psych. Sci.*). HTML articles from APA journals were collected in another project by CHJH (see [here](https://github.com/chartgerink/2016statcheck_data)), which were scanned here for *p*-values. These articles are subject to copyright and are not shared in this repository (local backups exist so more than willing to share for verification purposes).

After neatly ordering the HTMLs into separate folders ([script](functions/apa_foldering.sh)), these articles were converted from HTML to TXT with the tool `html2text` ([developed by Aaron Swartz](https://github.com/aaronsw/html2text)). Shell command `sh functions/html2text.sh`.

The data were extracted from the articles with regular expressions. Variables included are: 

1. Digital Object Identifier (`doi`); if available

2. Text before the *p*-value in the article (`pre`); 200 characters included. 

3. Raw text of the *p*-value result itself (`result`); e.g., `p=.048`

4. Text after the *p*-value in the article (`post`); 200 characters included. 

5. The sign used in the *p*-value reporting (`comparison`); possibilities include [=><≥≤]

6. The *p*-value reported (`value`)

These data are extracted from each article first and then collated into one final dataset. Each article folder (folder name = doi) contains `fulltext.html`, `fulltext.txt` (after `html2text` was used) and the file `results.csv` (which is the result of the regexes).

Before collecting the *p*-values from the articles, first the metadata for each article are collected. To this end, the following script is run.

```bash
ls apa_articles > data/doi
mkdir data/metadata

for i in $(cat data/doi)
do
 ruby functions/terrier.rb $i
done

rm data/doi
```

`results.csv` for each article was generated in `R` (see [script](functions/data_extraction.R)). This is run from a shell with `Rscript functions/data_extraction.R`. This also collates the individual files into a big one in the `data/` folder.