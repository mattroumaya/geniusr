geniusr
================

Tools for working with the Genius API.

-   Genius Developers Site: <https://genius.com/developers>
-   Genius API Docs: <https://docs.genius.com/>

Install
-------

Development version

``` r
devtools::install_github('ewenme/geniusr')
```

Authenticate
------------

1.  [Create a Genius API client](https://genius.com/api-clients/new)
2.  Generate a client access token from your [API Clients page](https://genius.com/api-clients)
3.  Set your credentials in the System Environment variable `GENIUS_API_TOKEN` by calling the `genius_token()` function and entering your Genius Client Access Token when prompted.

Use
---

``` r
library(geniusr)
library(dplyr)
library(tidytext)
```

### How many times did Kanye West say "good morning", on the track "Good Morning"?

``` r
# Get song search results for the term 'good morning'
gm_search <- search_song(search_term = "good morning") %>%
  # look for Kanye as the primary artist
  filter(artist_name == "Kanye West")

# get lyrics
gm_lyrics <- scrape_lyrics(song_id = gm_search$song_id)

# tokenization of the lyrics
gm_lyrics %>%
  # get bigrams
  unnest_tokens(bigram, line, token = "ngrams", n = 2) %>%
  # count bigram frequency
  count(bigram) %>%
  # look for good morning
  filter(bigram == "good morning")
```

    ## # A tibble: 1 x 2
    ##         bigram     n
    ##          <chr> <int>
    ## 1 good morning    18
