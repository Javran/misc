#!/usr/bin/env python3.7
"""
  This program is assumed to be executed in the directory where it lives in,
  so all file paths are relative to that.
"""

import cv2


def main():
  pat = cv2.imread('../sample/tree-sample.png')
  print(pat.shape)
  pat_h, pat_w, _ = pat.shape
  img = cv2.imread('../private/sample-5x5.png')
  result = cv2.matchTemplate(img,pat,cv2.TM_CCORR_NORMED)
  min_val, max_val, min_loc, max_loc = cv2.minMaxLoc(result)
  top_left = max_loc
  bottom_right = (top_left[0] + pat_w, top_left[1] + pat_h)
  cv2.rectangle(img, max_loc, bottom_right, 255, 2)
  cv2.imshow('whatever', img)


if __name__ == '__main__':
    main()
    while True:
        r = cv2.waitKey() & 0xFF
        if r == ord('q'):
            break

