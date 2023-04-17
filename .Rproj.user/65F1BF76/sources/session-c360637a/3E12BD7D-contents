#### read the packages ####
import os
import glob
import cv2
from matplotlib import pyplot as plt
import numpy as np
import pandas as pd

#### set the function ####
def ImgToMat(imgPath, colStart, colEnd, thre):
  img = cv2.imread(imgPath, cv2.IMREAD_UNCHANGED)
  
  # smoothing the image
  img = cv2.medianBlur(img, 15)
  img[:, :colEnd, :] = 0
  img[:, colStart:, :] = 0
  img_float = img.astype("float64")
  
  # change array to matrix
  pixelNum = img_float.shape[1] * img_float.shape[0]
  plantMatAll = np.reshape(img_float, (pixelNum, 3))
  plantMat = plantMatAll[plantMatAll.sum(axis = 1) > thre, :]
  return plantMat

#### set the parameters ####
year = "2023"

# {Day : existence of refBoard}
day_dict = {"0218":False}
index = ["num", "R", "G", "B"]

#### set the data path ####
dataPath = "data\\0.1.img"
refPath = "data\\0.2.EasyPCC\\2.refBoard\\4.segmented"

for day in day_dict.keys():
  # day = "0218"
  # get the data path
  dataList = glob.glob(dataPath  + "\\" + year + day + "\\*")
  
  # make the save folder
  valFolder = "data\\0.3.val\\"
  if not os.path.exists(valFolder):
    os.makedirs(valFolder)
  
  # set the save file name
  saveFile = valFolder + "\\" + year + day + ".csv"
  if os.path.exists(saveFile):
    continue
  
  # get the refBoard path
  if day_dict[day]:
    refList0 = glob.glob(refPath  + "\\*")
    refList = [x for x in refList0 if day in x]
  
  # make the df to input the values extracted from images
  val_df = pd.DataFrame(np.zeros((len(dataList), len(index))))
  val_df.columns = index
  val_df.index = np.arange(1, len(dataList) + 1, 1)
  
  for i in range(len(dataList)):
    # i = 0
    # calculating the value of segmented plant
    plantMat = ImgToMat(imgPath = dataList[i], colStart = -1, colEnd = 0, thre = 0)
    
    # calculating the ref value
    if day_dict[day]:
      refMat = ImgToMat(imgPath = refList[i], colStart = -1, colEnd = 0, thre = 0)
      refValue = np.mean(refMat, axis = 0)
    else:
      refValue = np.array([1, 1, 1])
    
    # calculating the reflectane value
    R = plantMat[:, 0] / refValue[0]
    G = plantMat[:, 1] / refValue[1]
    B = plantMat[:, 2] / refValue[2]
    
    # input the values to the matrix
    col1_val = [len(R), 
                np.mean(R), 
                np.mean(G), 
                np.mean(B)]
    val_df.iloc[i, :] = col1_val
    
  # save the NDVI value
  val_df.to_csv(valFolder + "\\" + year + day + ".csv")

