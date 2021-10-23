from newsapi import NewsApiClient
import pandas as pd
from datetime import datetime

# Init
benedikt_key = '07288a2d35394938b113ad3cf504d9cd'
newsapi = NewsApiClient(api_key=benedikt_key)

newsapi.get_everything(q="Karl Lauterbach", sort_by="relevancy")

# get today's date
today_date = datetime.today().strftime('%Y-%m-%d')

# sources
sources = newsapi.get_sources()["sources"]

english_sources_names = []
german_sources_names = []
austrian_sources_names = []
italian_sources_names = []
french_sources_names = []

for source in sources:
    if source["country"] in ["gb", "us"]:
        english_sources_names.append(source["id"])
    elif source["country"] == "de":
        german_sources_names.append(source["id"])
    elif source["country"] == "fr":
        french_sources_names.append(source["id"])
    elif source["country"] == "it":
        italian_sources_names.append(source["id"])
    elif source["country"] == "at":
        austrian_sources_names.append(source["id"])

# parse top_headlines
#top_headlines = newsapi.get_top_headlines(page_size=100)["articles"]

src = newsapi.get_sources(language="de")["sources"]
general_sources = []
print(len(src))
for source in src:
    general_sources.append(source["id"])


# parse all articles
all_articles = newsapi.get_everything(sources=", ".join(english_sources_names),sort_by='relevancy')["articles"]
print(len(all_articles))
all_articles.append(newsapi.get_everything(sources=", ".join(general_sources), sort_by="relevancy")["articles"])
print(len(all_articles))
all_articles.append(newsapi.get_everything(sources=", ".join(french_sources_names), sort_by="relevancy")["articles"])
print(len(all_articles))
all_articles.append(newsapi.get_everything(sources=", ".join(italian_sources_names), sort_by="relevancy")["articles"])
#all_articles.append(newsapi.get_everything(from_param=today_date, sources=", ".join(austrian_sources_names), sort_by="relevancy")["articles"])
print(len(all_articles))
# full raw response
raw_response = top_headlines
raw_response.append(all_articles)

print("First 10 articles of final raw_respnse:")
print(raw_response[1:10])

def transform_to_df(resp_dict, is_top_headline):
    # create empty dataframe for filling unprocessed parsing data in
    df_articles = pd.DataFrame(
        columns=['source', 'title', 'author', 'description', 'content', 'url', 'urlToImage', 'publishedAt'])

    output_dict = {}
    output_dict['source'] = []
    output_dict['title'] = []
    output_dict['author'] = []
    output_dict['description'] = []
    output_dict['content'] = []
    output_dict['url'] = []
    output_dict['url_image'] = []
    output_dict['date_published'] = []
    output_dict['is_top_headline'] = []


    for article in resp_dict:
        try:
            output_dict['source'].append(article['source']['name'])
            output_dict['title'].append(article['title'])
            output_dict['author'].append(article['author'])
            output_dict['description'].append(article['description'])
            output_dict['content'].append(article['content'])
            output_dict['url'].append(article['url'])
            output_dict['url_image'].append(article['urlToImage'])
            output_dict['date_published'].append(article['publishedAt'])
            if is_top_headline:
                output_dict['is_top_headline'].append("TRUE")
            else:
                output_dict['is_top_headline'].append("FALSE")
        except TypeError:
            print("An TypeError occurred in the following article!")
            print(article)


    # create df with the articles from this country
    df_articles_dict = pd.DataFrame.from_dict(output_dict)
    # print(df_country_dict.head(3))

    # append it to the general output df
    df_articles = df_articles.append(df_articles_dict, ignore_index=True, sort=False)

    print("Number of parsed articles: " + str(len(df_articles)))
    print(df_articles.head(5))

    return df_articles

df_top_headlines = transform_to_df(top_headlines,True)
df_all_articles = transform_to_df(all_articles,False)

