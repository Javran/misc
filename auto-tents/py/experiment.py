#!/usr/bin/env python3.7
"""
  This program is assumed to be executed in the directory where it lives in,
  so all file paths are relative to that.
"""

import cv2
from matplotlib import pyplot

# finding best width:
# original width 211, let's say minimal is 30.
# try numbers 16 steps apart, keep only 20% best values as candidate.
# try numbers 8 steps apart from candidates, keep only 20%
# ...
# until we get one best value.

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


def scale_pattern(pat_orig, target_width):
  pat_orig_h, pat_orig_w, _ = pat_orig.shape
  scale = target_width / pat_orig_w
  pat_h = round(pat_orig_h * scale)
  pat_w = round(pat_orig_w * scale)
  return cv2.resize(pat_orig, (pat_w, pat_h), cv2.INTER_LANCZOS4)

def main():
  # pat_h, pat_w, _ = pat.shape  # for 14x14, the target width seems to be 83
  img = cv2.imread('../private/sample-14x14.png')

  # width of the original pattern: 211
  pat_orig = cv2.imread('../sample/tree-sample.png')

  best_target_width = None
  best_max_val = None
  for target_width in range(70,90,1):
    pat = scale_pattern(pat_orig, target_width)
    pat_w, pat_h, _ = pat.shape
    result = cv2.matchTemplate(img,pat,cv2.TM_CCORR_NORMED)
    min_val, max_val, min_loc, max_loc = cv2.minMaxLoc(result)
    if best_max_val is None or best_max_val < max_val:
      best_max_val, best_target_width = max_val, target_width
    print(target_width, max_val)

  print(f'Best target width is: {best_target_width}')
  pat = scale_pattern(pat_orig, best_target_width)
  pat_h, pat_w, _ = pat.shape
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

