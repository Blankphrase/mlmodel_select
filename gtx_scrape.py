from urllib.request import urlopen
from bs4 import BeautifulSoup as soup
import re
import pandas as pd
import time
def newegg_gtx_scrape(filename,url):
    # Read the page into python with urlopen. Then close the connection afterwards.
    urlClient=urlopen(myURL1)
    # read the html file
    gtx1080_newegg=urlClient.read()
    # parse the file as html.
    gtx1080_newegg_parsed=soup(gtx1080_newegg,"html.parser")
    # find a div with class lable item-container
    gtx1080_items=gtx1080_newegg_parsed.findAll("div",{"class":"item-container"})


    # write a function to return the value after regular expression match.
    # If there is no match, an NA value is returned.
    def regex_value(regex,string):
        return_string="NA"
        match=re.compile(regex).search(string)
        if not match ==None:
            return_string=match.group(0)
        return return_string

    GPU_info=[]
    for gtx1080_item in gtx1080_items:
        brand=gtx1080_item.div.div.a.img["title"]
        gpu_type=regex_value("GTX\s\d{4}\s{0,1}T{0,1}i{0,1}",gtx1080_item.a.img["title"])
        mem_size=regex_value("\d{1,2}GB",gtx1080_item.a.img["title"])
        mem_type=regex_value("GDDR\d.{0,1}",gtx1080_item.a.img["title"])
        price=regex_value("\d{3}.{0,1}",gtx1080_item.findAll("strong")[-1].text)
        if price=="NA":
            price=regex_value("\d{3}.{0,1}",gtx1080_item.findAll("strong")[-3].text)
        ship=gtx1080_item.findAll("li",{"class":"price-ship"})[0].get_text().strip()
        http_adr=gtx1080_item.a["href"]
        gpu_info=[brand,gpu_type,mem_size,mem_type,price,ship,http_adr]
        GPU_info.append(gpu_info)
    # write the information into a csv file.
    GPU_info=pd.DataFrame(GPU_info)
    GPU_info.to_csv(filename, index=False, mode='a', header=False)
    time.sleep(9)

# scrape the information here.
myURL1="https://www.newegg.com/Product/ProductList.aspx?Submit=ENE&DEPA=0&Order=BESTMATCH&Description=gtx+1080&ignorear=0&N=-1&isNodeId=1"
myURL2="https://www.newegg.com/Product/ProductList.aspx?Submit=ENE&DEPA=0&Order=BESTMATCH&Description=gtx+1070&ignorear=0&N=-1&isNodeId=1"
myURL3="https://www.newegg.com/Product/ProductList.aspx?Submit=ENE&DEPA=0&Order=BESTMATCH&Description=gtx+1060&ignorear=0&N=-1&isNodeId=1"

newegg_gtx_scrape(filename="gtx1080_newegg.csv",url=myURL1)
newegg_gtx_scrape(filename="gtx1080_newegg.csv",url=myURL2)
newegg_gtx_scrape(filename="gtx1080_newegg.csv",url=myURL3)


for i in [2,3,4]:
    url1="https://www.newegg.com/Product/ProductList.aspx?Submit=ENE&N=-1&IsNodeId=1&Description=gtx%201080&bop=And&Page={0}&PageSize=36&order=BESTMATCH".format(i)
    url2="https://www.newegg.com/Product/ProductList.aspx?Submit=ENE&N=-1&IsNodeId=1&Description=gtx%201070&bop=And&Page={0}&PageSize=36&order=BESTMATCH".format(i)
    url3="https://www.newegg.com/Product/ProductList.aspx?Submit=ENE&N=-1&IsNodeId=1&Description=gtx%201060&bop=And&Page={0}&PageSize=36&order=BESTMATCH".format(i)
    newegg_gtx_scrape(filename="gtx1080_newegg.csv", url=url1)
    newegg_gtx_scrape(filename="gtx1080_newegg.csv", url=url2)
    newegg_gtx_scrape(filename="gtx1080_newegg.csv", url=url3)
