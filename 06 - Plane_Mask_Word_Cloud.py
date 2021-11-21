#!/usr/bin/env python
# coding: utf-8

# In[2]:


pip install wordcloud


# In[85]:


from PIL import Image
from wordcloud import WordCloud, STOPWORDS

#importing the libraries and data
import numpy as np 
import pandas as pd 
import seaborn as sns
import matplotlib.pyplot as plt
from datetime import date, timedelta, datetime


# In[97]:


Data = pd.read_csv('../Downloads/Airplane_Crashes_and_Fatalities_Since_1908_final.csv', encoding = 'Latin1')


# In[98]:


text = str(Data.Summary.tolist())
mask = np.array(Image.open('../Downloads/msdk.png'))


# In[116]:


def random_color_func(word=None, font_size=None, position=None, orientation=None, font_path=None, random_state=None):
    h = int(340.0 * 21.0 / 255.0)
    s = int(100.0 * 255.0 / 255.0)
    l = int(100.0 * float(random_state.randint(60, 120)) / 255.0)
    return "hsl({}, {}%, {}%)".format(h, s, l)

def red_color_func(word, font_size, position, orientation, random_state=None,
                    **kwargs):
    return "hsl(0, 100%%, %d%%)" % random.randint(30, 50)


# In[121]:


wc = WordCloud(background_color = 'white', max_words = 100, mask = mask, stopwords = stopwords, random_state = 1)
wc.generate(text)

plt.figure(figsize = (10,10))
plt.imshow(wc.recolor(color_func=red_color_func), interpolation = 'bilinear')
plt.tight_layout(pad=0)
plt.axis("off")
plt.show()

