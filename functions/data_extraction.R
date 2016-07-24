if(!require(stringr)){install.packages('stringr')}
library(stringr)
if(!require(httr)){install.packages('httr')}
library(httr)

# Get a list of articles to go through
dois <- list.files('apa_articles')

# Extract the results from each paper
for(doi in dois)
{
  if(!file.exists(sprintf('apa_articles/%s/results.csv', doi)))
  {
    filename <- sprintf('apa_articles/%s/fulltext.txt', doi)
    txt <- readChar(filename, file.info(filename)$size)
    
    txt <- gsub(pattern = '_', replacement = '', x = txt)
    txt <- gsub(pattern = '&lt;', replacement = '<', x = txt)
    txt <- gsub(pattern = '&gt;', replacement = '>', x = txt)
    
    locs <- str_locate_all(pattern = "\\sp\\s*[=<>]\\s*\\d?\\.\\d*", txt)[[1]]
    
    pre <- str_sub(txt,
                   start = locs[, 1] - 200,
                   end = locs[, 1])
    
    result <- str_match_all(pattern = "\\sp\\s*[=<>]\\s*\\d?\\.\\d*", txt)[[1]][,1] %>%
      gsub(patter = '[_ ]', replacement = '')
    
    post <- str_sub(txt,
                    end = locs[, 2] + 200,
                    start = locs[, 2])
    
    comparison <- unlist(str_match_all(result, pattern = "[<>=]"))
    
    value <- unlist(str_match_all(result, pattern = "\\d?\\.\\d*"))
    
    # Combine with previously collected metadata
    sel <- grepl(list.files('data/metadata'), pattern = doi)
    x <- tryCatch(read.csv(sprintf('data/metadata/%s',
                                   list.files('data/metadata')[sel]),
                           header = FALSE),
                  error = function (e) data.frame(V1 = NA, V2 = NA))
    
    if(!is.null(comparison))
    {
      df <- data.frame(doi,
                       journal = as.character(x$V1)[1],
                       year = as.numeric(x$V2)[1],
                       pre,
                       result,
                       post,
                       comparison,
                       value)
      
      write.csv(df, 
                sprintf('apa_articles/%s/results.csv', doi),
                row.names = FALSE)
    }
    cat(sprintf('%s\n', doi)) 
  }
}