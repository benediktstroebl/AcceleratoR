from newsapi import NewsApiClient
import pandas as pd

# Init
api_key = '07288a2d35394938b113ad3cf504d9cd'
newsapi = NewsApiClient(api_key=api_key)

deu_politicians = pd.read_csv("../data/deu_politicians.csv", encoding="latin-1")
print(deu_politicians)

# initialize dict with keys in list
key_dict = {"politician", "source", "title", "author", "description", "content", "url", "url_image", "date_published"}
deu_politicians_articles = dict([(key, []) for key in key_dict])

# create empty dataframe for filling unprocessed parsing data in
df_articles = pd.DataFrame(
    columns=['politician', 'source', 'title', 'author', 'description', 'content', 'url', 'urlToImage', 'publishedAt'])

# iterate over rows with iterrows()
# TODO: Remove head()
for index, row in deu_politicians.head(5).iterrows():
    response = newsapi.get_everything(q=row["name"], sort_by="relevancy")["articles"]

    for article in response:
        deu_politicians_articles['politician'].append(row["name"])
        deu_politicians_articles['source'].append(article['source']['name'])
        deu_politicians_articles['title'].append(article['title'])
        deu_politicians_articles['author'].append(article['author'])
        deu_politicians_articles['description'].append(article['description'])
        deu_politicians_articles['content'].append(article['content'])
        deu_politicians_articles['url'].append(article['url'])
        deu_politicians_articles['url_image'].append(article['urlToImage'])
        deu_politicians_articles['date_published'].append(article['publishedAt'])

print(deu_politicians_articles)

df_deu_politicians_articles = pd.DataFrame.from_dict(deu_politicians_articles)
# append it to the general output df
df_deu_politicians_articles = df_articles.append(df_deu_politicians_articles, ignore_index=True, sort=False)
print(df_deu_politicians_articles.head())
df_deu_politicians_articles.to_csv('df_deu_politicians_articles.csv', index=True, encoding="utf-8-sig")
