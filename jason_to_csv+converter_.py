
# coding: utf-8

# In[1]:


import csv

import json

import os

import sys

import codecs


# In[ ]:


srcpath = "C:\\Avishek\\Booth\\Autumn 2017\\37304 Digital And Algorithmic Marketing\\YELP\\JSON\\Dataset\\review.json"
destpath = "C:\\Avishek\\Booth\\Autumn 2017\\37304 Digital And Algorithmic Marketing\\YELP\\JSON\\Dataset\\review_all.csv"
csv_data = open(destpath,"w", encoding='utf-8', newline='')
csvwriter = csv.writer(csv_data)

counter = 0
with open(srcpath,"r", encoding='utf-8') as json_file:
    for json_line in json_file:
        #if counter < 2110:
            try:
                parsed_data = json.loads(json_line)
                if counter == 0:
                    #csvwriter.writerow(['user_id','business_id','stars','date','useful','funny','cool'])
                    csvwriter.writerow(parsed_data.keys())
                counter = counter + 1
                #print(parsed_data.keys())
                #break;
                #csv_input =[[parsed_data['user_id'],parsed_data['business_id'],parsed_data['stars'],parsed_data['date'],parsed_data['useful'],parsed_data['funny'],parsed_data['cool']]]
                #csvwriter.writerow([csv_input[0][0],csv_input[0][1],csv_input[0][2],csv_input[0][3],csv_input[0][4],csv_input[0][5],csv_input[0][6]])
                csvwriter.writerow(parsed_data.values())
            except:
                pass
        #else:
           #break
csv_data.close()

