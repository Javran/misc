#!/usr/bin/env python3.7
"""
  This program is assumed to be executed in the directory where it lives in,
  so all file paths are relative to that.
"""

import cv2
import functools
import math
import numpy as np
from matplotlib import pyplot

tm_method = cv2.TM_CCOEFF_NORMED


def find_and_mark_matches(img, result, pat_dims, threshold):
  """Find and mark matching places given a result of matchTemplate.
  """
  img_marked = img.copy()
  h, w = result.shape
  pat_h, pat_w = pat_dims
  for r in range(h):
    for c in range(w):
      if (result[r,c] > threshold):
        print(r,c, result[r,c])
        top_left = (c,r)
        bottom_right = (c + pat_w, r + pat_h)
        cv2.rectangle(img_marked, top_left, bottom_right, 255, 2)
  return img_marked


def scale_pattern(pat_orig, target_width):
  pat_orig_h, pat_orig_w, _ = pat_orig.shape
  scale = target_width / pat_orig_w
  pat_h = round(pat_orig_h * scale)
  pat_w = round(pat_orig_w * scale)
  return cv2.resize(pat_orig, (pat_w, pat_h), cv2.INTER_AREA)


def optimize_pattern_width(pat_orig, img):
  eval_count = 0

  @functools.lru_cache()
  def evaluate_width(width):
    nonlocal eval_count
    pat = scale_pattern(pat_orig, width)
    pat_w, pat_h, _ = pat.shape
    result = cv2.matchTemplate(img,pat,tm_method)
    min_val, max_val, min_loc, max_loc = cv2.minMaxLoc(result)
    eval_count += 1
    return max_val

  # search within this range, with decreasing steps per iteration until
  # we reach a local maxima
  min_width, max_width = 30, 220
  step = 16
  candidates = set(range(min_width, max_width, step))

  while True:
    sorted_candidates = sorted(candidates,key=evaluate_width,reverse=True)
    # Only top few candidates survive.
    keep = max(1, math.floor(len(sorted_candidates) * 0.1))
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
  print(f'Best target width is: {best_target_width}, evaluations: {eval_count}')
  return best_target_width


def resample_pattern_from_image(pat_orig, img):
  best_target_width = optimize_pattern_width(pat_orig, img)
  pat = scale_pattern(pat_orig, best_target_width)
  pat_h, pat_w, _ = pat.shape
  result = cv2.matchTemplate(img,pat,tm_method)
  _, _, _, max_loc = cv2.minMaxLoc(result)
  c, r = max_loc
  return img[r:r+pat_h,c:c+pat_w]

def main_scale_pattern_and_match():
  img = cv2.imread('../private/sample-18x18.png')
  pat_orig = cv2.imread('../sample/tree-sample.png')
  pat = resample_pattern_from_image(pat_orig, img)
  pat_h, pat_w, _ = pat.shape
  print(pat.shape)
  result = cv2.matchTemplate(img,pat,tm_method)
  result_norm = cv2.normalize(result, 0, 255)
  min_val, max_val, min_loc, max_loc = cv2.minMaxLoc(result)

  # now the problem lies in how should we find this threshold.
  # it is promising here to analyze histogram to determine this value.
  img_marked = find_and_mark_matches(img, result, [pat_h, pat_w], 0.95)
  print(f'min: {min_val}, max: {max_val}')
  top_left = max_loc
  bottom_right = (top_left[0] + pat_w, top_left[1] + pat_h)
  pyplot.figure().canvas.set_window_title('@dev')

  pyplot.subplot(131), pyplot.imshow(result_norm,cmap = 'gray')
  pyplot.title('result'), pyplot.xticks([]), pyplot.yticks([])
  # opencv stores in BGR while pyplot in RGB. (https://stackoverflow.com/a/41869419/315302)
  pyplot.subplot(132), pyplot.imshow(img_marked[:,:,[2,1,0]])
  pyplot.title('origin'), pyplot.xticks([]), pyplot.yticks([])

  pyplot.subplot(133), pyplot.hist(result.flatten(), range=(0.9, 1.0))
  pyplot.title('hist')
  # pyplot.subplot(223),pyplot.imshow(pat_orig[:,:,[2,1,0]])
  # pyplot.title('pat_orig'), pyplot.xticks([]), pyplot.yticks([])
  # pyplot.subplot(224),pyplot.imshow(pat[:,:,[2,1,0]])
  # pyplot.title('pat'), pyplot.xticks([]), pyplot.yticks([])

  pyplot.show()


def main_all_samples():
  pat_orig = cv2.imread('../sample/tree-sample.png')
  for i in range(5,22+1):
    img = cv2.imread(f'../private/sample-{i}x{i}.png')
    target_width = optimize_pattern_width(pat_orig, img)
    print(f'{i}: {target_width}')


def main_find_blanks():
  img = cv2.imread('../private/sample-18x18.png')
  h, w, _ = img.shape
  # This is the exact color that game uses for blank cells.
  bk = (49, 49, 52)
  result = cv2.inRange(img, bk, bk)

  found = None
  mask = np.zeros((h+2,w+2), dtype=np.uint8)
  for r in range(h):
    if found:
      break
    for c in range(w):
      if (result[r,c] != 0):
        print(r,c,result[r,c])
        retval, result, _, _ = cv2.floodFill(result, mask, (c,r), 0)
        found = True
        break

  pyplot.figure().canvas.set_window_title('@dev')
  pyplot.subplot(131), pyplot.imshow(img[:,:,[2,1,0]])
  pyplot.title('origin'), pyplot.xticks([]), pyplot.yticks([])
  pyplot.subplot(132), pyplot.imshow(result,cmap = 'gray')
  pyplot.title('result'), pyplot.xticks([]), pyplot.yticks([])
  pyplot.subplot(133), pyplot.imshow(mask,cmap = 'gray')
  pyplot.title('mask'), pyplot.xticks([]), pyplot.yticks([])
  pyplot.show()


if __name__ == '__main__':
  #main_scale_pattern_and_match()
  main_find_blanks()
