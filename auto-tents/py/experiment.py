#!/usr/bin/env python3.7
"""
  This program is assumed to be executed in the directory where it lives in,
  so all file paths are relative to that.
"""

import cv2
from matplotlib import pyplot

def find_and_mark_matches(img, result, pat_dims):
    h, w = result.shape
    pat_h, pat_w = pat_dims
    for r in range(h):
        for c in range(w):
            if (result[r,c] > 0.9995):
                print(r,c, result[r,c])
                top_left = (c,r)
                bottom_right = (c + pat_w, r + pat_h)
                cv2.rectangle(img,top_left, bottom_right, 255, 2)

def main():
  pat = cv2.imread('../sample/tree-sample.png')
  print(pat.shape)
  pat_h, pat_w, _ = pat.shape
  img = cv2.imread('../private/sample-5x5.png')
  result = cv2.matchTemplate(img,pat,cv2.TM_CCORR_NORMED)
  result_norm = cv2.normalize(result,0, 255)
  find_and_mark_matches(img, result, [pat_h, pat_w])
  min_val, max_val, min_loc, max_loc = cv2.minMaxLoc(result)
  print(f'min: {min_val}, max: {max_val}')
  top_left = max_loc
  bottom_right = (top_left[0] + pat_w, top_left[1] + pat_h)
  pyplot.subplot(121)
  pyplot.imshow(result_norm,cmap = 'gray')
  pyplot.title('result'), pyplot.xticks([]), pyplot.yticks([])
  # opencv stores in BGR while pyplot in RGB. (https://stackoverflow.com/a/41869419/315302)
  pyplot.subplot(122),pyplot.imshow(img[:,:,[2,1,0]])
  pyplot.title('origin'), pyplot.xticks([]), pyplot.yticks([])
  pyplot.show()

if __name__ == '__main__':
    main()
    while True:
        r = cv2.waitKey() & 0xFF
        if r == ord('q'):
            break

