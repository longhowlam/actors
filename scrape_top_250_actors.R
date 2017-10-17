library(rvest)

actors = "http://www.imdb.com/list/ls000004615/"
out = read_html(actors)

actorlink = html_nodes(out, xpath = '//*[@id="main"]/div/div/div/div/a') %>% 
  html_attr(name="href")

actorlink = paste0("http://www.imdb.com", actorlink)


ff = function(x)
{
  out2 = read_html(x)
  out2 %>% html_node( xpath = '//*[@id="name-poster"]') %>% html_attr(name="src")
}

### get the links to the images
out = NULL
for(i in 1:length(actorlink))
{
  out = c(out,ff(actorlink[i]))
  print(i)
}

## now download all the pictures so that we can use them for similarity scores
for(i in 1:100)
{
  download.file(
    out[i], 
    destfile = paste0("images/act", i, ".jpg"),
    mode = "wb"
  )
}
