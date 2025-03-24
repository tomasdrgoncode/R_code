library(tidyverse)
library(rvest)

#download selectorgadget.com for Chrome, it helps select elements

html <- read_html("https://krakenkratom.com/")
#or
html <- read_html("https://krakenkratom.com/accessories/empty-capsules")

html <- read_html("https://www.amazon.com/gp/bestsellers/hpc/6973704011?ref_=Oct_d_obs_S&pd_rd_w=Moxgb&content-id=amzn1.sym.68cf20ef-f2f0-42ca-8c87-ad9617594532&pf_rd_p=68cf20ef-f2f0-42ca-8c87-ad9617594532&pf_rd_r=VJJCD5QE9GCRCM2SYZWX&pd_rd_wg=KmyHH&pd_rd_r=7480c271-46d4-4707-a717-b0068da351e2")


class(html)

html_text(html)

elements <- html %>% html_elements(".product-name")

result<-html_text(elements)
result

#___ not sure about this
deeper <- elements <- html %>% 
  html_element("a") %>% 
  html_text2()

deeper



#FROM TUTORIAL
# Start by reading a HTML page with read_html():
starwars <- read_html("https://rvest.tidyverse.org/articles/starwars.html")

# Then find elements that match a css selector or XPath expression
# using html_elements(). In this example, each <section> corresponds
# to a different film
films <- starwars %>% html_elements("section")
films

# Then use html_element() to extract one element per film. Here
# we the title is given by the text inside <h2>
title <- films %>% 
  html_element("h2") %>% 
  html_text2()
title

# Or use html_attr() to get data out of attributes. html_attr() always
# returns a string so we convert it to an integer using a readr function
episode <- films %>% 
  html_element("h2") %>% 
  html_attr("data-id") %>% 
  readr::parse_integer()
episode