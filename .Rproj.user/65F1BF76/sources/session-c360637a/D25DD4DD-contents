# read the libraries
import os
import glob
import cv2
from matplotlib import pyplot as plt
import numpy as np
import pandas as pd

#### set the parameters ####
year = "2023"
day_dict = {"0218":[30, 0, 200]}

for day in day_dict.keys():
  # day = "0218"
  base_folder = "raw_data\\" + year + day
  data_list = glob.glob(base_folder  + "\\*")
  
  # make the save folder
  img_folder = "data\\0.1.img\\" + year + day
  if not os.path.exists(img_folder):
    os.makedirs(img_folder)
  else:
    continue

  for i in range(len(data_list)):
    # i = 0
    img = cv2.imread(data_list[i],  cv2.IMREAD_UNCHANGED)
    # plt.imshow(img)
    # plt.show()
    
    # smoothing the image
    img = cv2.medianBlur(img, 15)
    # img_float = img.astype("float64")

    # extracting each color info
    R = img[:, :, 0]
    G = img[:, :, 1]
    B = img[:, :, 2]
    
    thre1 = day_dict[day][0]
    thre2 = day_dict[day][1]
    thre3 = day_dict[day][2]

    img[B < thre1] = 0
    img[G < thre2] = 0
    img[R > thre3] = 0
    
    # save the segmented img
    cv2.imwrite(img_folder + "\\" + str(i + 1).zfill(2) + ".png", img)
    
