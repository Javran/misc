#!/usr/bin/env python3.7
"""
  This program is assumed to be executed in the directory where it lives in,
  so all file paths are relative to that.
"""

import cv2
from matplotlib import pyplot

def find_and_mark_matches(img, result, pat_dims, mx):
    h, w = result.shape
    pat_h, pat_w = pat_dims
    threshold = 0.9995 * mx
    for r in range(h):
        for c in range(w):
            if (result[r,c] > threshold):
                print(r,c, result[r,c])
                top_left = (c,r)
                bottom_right = (c + pat_w, r + pat_h)
                cv2.rectangle(img,top_left, bottom_right, 255, 2)

def main():
  scale = 83 / 211
  # width of the original pattern: 211
  pat = cv2.imread('../sample/tree-sample.png')

  pat_w = int(pat.shape[1] * scale)
  pat_h = int(pat.shape[0] * scale)

  pat = cv2.resize(pat, (pat_w, pat_h))

  print(pat.shape)
  # pat_h, pat_w, _ = pat.shape  # for 14x14, the target width seems to be 83
  img = cv2.imread('../private/sample-14x14.png')

  result = cv2.matchTemplate(img,pat,cv2.TM_CCORR_NORMED)
  result_norm = cv2.normalize(result,0, 255)
  min_val, max_val, min_loc, max_loc = cv2.minMaxLoc(result)
  find_and_mark_matches(img, result, [pat_h, pat_w], max_val)
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

