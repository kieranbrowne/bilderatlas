import os
import json


f = open("public/data/category_list", "w")

f.write(json.dumps(os.listdir("public/data/categories")))

f.close()
