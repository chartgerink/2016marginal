# Marginal *p*-values in psychology

This project investigates the claims made in Pritschet et al. 2016 (*Psych. Sci.*). HTML articles from APA journals were collected in another project by CHJH (see [here](https://github.com/chartgerink/2016statcheck_data)), which were scanned here for *p*-values. These articles are subject to copyright and are not shared in this repository (local backups exist so more than willing to share for verification purposes).

These articles were first converted from HTML to TXT with the tool `html2text` ([developed by Aaron Swartz](https://github.com/aaronsw/html2text)).

The data were extracted from the articles with regular expressions. Variables included are: 

1. Digital Object Identifier (`doi`); if available

2. Text before the *p*-value in the article (`pre`); 200 characters included. 

3. Raw text of the *p*-value result itself (`result`); e.g., `p=.048`

4. Text after the *p*-value in the article (`post`); 200 characters included. 

5. The sign used in the *p*-value reporting (`comparison`); possibilities include [=><≥≤]

6. The *p*-value reported (`value`); can be numeric or *ns* (nonsignificant; not sure if needed, check with MvA and JMW?)
