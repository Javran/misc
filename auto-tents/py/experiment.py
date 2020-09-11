#!/usr/bin/env python3.7
"""
  This program is assumed to be executed in the directory where it lives in,
  so all file paths are relative to that.
"""

import cv2
import functools
import math
from matplotlib import pyplot

# finding best width:
# original width 211, let's say minimal is 30.
# try numbers 16 steps apart, keep only 20% best values as candidate.
# try numbers 8 steps apart from candidates, keep only 20%
# ...
# until we get one best value.

def find_and_mark_matches(img, result, pat_dims, mx):
  """Find and mark matching places given a result of matchTemplate.
  """
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
  return cv2.resize(pat_orig, (pat_w, pat_h), cv2.INTER_AREA)


def optimize_pattern_width(pat_orig, img):
  @functools.lru_cache()
  def evaluate_width(width):
    pat = scale_pattern(pat_orig, width)
    pat_w, pat_h, _ = pat.shape
    result = cv2.matchTemplate(img,pat,cv2.TM_CCORR_NORMED)
    min_val, max_val, min_loc, max_loc = cv2.minMaxLoc(result)
    print(f'{width} => {max_val}')
    return max_val

  # search within this range, with decreasing steps per iteration until
  # we reach a local maxima
  min_width, max_width = 30, 220
  step = 16
  candidates = set(range(min_width, max_width, step))

  while True:
    sorted_candidates = sorted(candidates,key=evaluate_width,reverse=True)
    # Only top 20% survives.
    keep = max(1, math.floor(len(sorted_candidates) * 0.2))
    candidates = sorted_candidates[:keep]
    step //= 2
    if not step:
      break
    # candidate expansion for next iteration.
    candidates = {
      y
      for x in candidates
      for y in [x-step, x, x+step]
      if min_width <= y <= max_width
    }

  # note that here candidates are sorted
  best_target_width = candidates[0]
  print(f'Best target width is: {best_target_width}')
  return best_target_width


def main():
  img = cv2.imread('../private/sample-22x22.png')

  # width of the original pattern: 211
  pat_orig = cv2.imread('../sample/tree-sample.png')

  best_target_width = optimize_pattern_width(pat_orig, img)
  pat = scale_pattern(pat_orig, best_target_width)
  pat_h, pat_w, _ = pat.shape
  print(pat.shape)
  result = cv2.matchTemplate(img,pat,cv2.TM_CCORR_NORMED)
  result_norm = cv2.normalize(result,0, 255)
  min_val, max_val, min_loc, max_loc = cv2.minMaxLoc(result)
  find_and_mark_matches(img, result, [pat_h, pat_w], max_val)
  print(f'min: {min_val}, max: {max_val}')
  top_left = max_loc
  bottom_right = (top_left[0] + pat_w, top_left[1] + pat_h)
  pyplot.figure().canvas.set_window_title('@dev')
  pyplot.subplot(121)
  pyplot.imshow(result_norm,cmap = 'gray')
  pyplot.title('result'), pyplot.xticks([]), pyplot.yticks([])
  # opencv stores in BGR while pyplot in RGB. (https://stackoverflow.com/a/41869419/315302)
  pyplot.subplot(122),pyplot.imshow(img[:,:,[2,1,0]])
  pyplot.title('origin'), pyplot.xticks([]), pyplot.yticks([])
  pyplot.show()

if __name__ == '__main__':
    main()

