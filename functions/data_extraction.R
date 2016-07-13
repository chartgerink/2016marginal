if(!require(stringr)){install.packages('stringr')}
library(stringr)
if(!require(httr)){install.packages('httr')}
library(httr)

# Get a list of articles to go through
dois <- list.files('apa_articles')

res <- data.frame(doi = NULL,
                  journal = NULL,
                  year = NULL,
                  pre = NULL,
                  result = NULL,
                  post = NULL,
                  comparison = NULL,
                  value = NULL)

for(doi in dois)
{
  filename <- sprintf('apa_articles/%s/fulltext.txt', doi)
  txt <- readChar(filename, file.info(filename)$size)
  
  locs <- gregexpr(pattern = "\\s_?p_?\\s*[=<>]\\s*\\d?\\.\\d*", txt, perl = TRUE)
  
  pre <- str_sub(txt,
                 start = locs[[1]] - 200 - attr(locs[[1]], "match.length"),
                 end = locs[[1]] - attr(locs[[1]], "match.length"))
  
  result <- str_match_all(pattern = "\\s_?p_?\\s*[=<>]\\s*\\d?\\.\\d*", txt)[[1]][,1] %>%
    gsub(patter = '[_ ]', replacement = '')
  
  post <- str_sub(txt,
                  end = locs[[1]] + 200 + attr(locs[[1]], "match.length"),
                  start = locs[[1]] + attr(locs[[1]], "match.length"))
  
  comparison <- unlist(str_match_all(result, pattern = "[<>=]"))
  
  value <- unlist(str_match_all(result, pattern = "\\d?\\.\\d*"))
  
  # Combine with previously collected metadata
  sel <- grepl(list.files('data/metadata'), pattern = doi)
  x <- read.csv(sprintf('data/metadata/%s', list.files('data/metadata')[sel]), header = FALSE)
  
  if(!is.null(comparison))
  {
    df <- data.frame(doi,
                     journal = as.character(x$V1),
                     year = as.numeric(x$V2),
                     pre,
                     result,
                     post,
                     comparison,
                     value)
    
    write.csv(df, 
              sprintf('apa_articles/%s/results.csv', doi),
              row.names = FALSE)
    rbind(res, df)
  }
  cat(doi)
}

write.csv(res, 
          'data/marginal_dataset.csv',
          row.names = FALSE)