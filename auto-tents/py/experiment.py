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
import collections

tm_method = cv2.TM_CCOEFF_NORMED

def load_sample(size):
  return cv2.imread(f'../private/sample-{size}x{size}.png')


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
  img = load_sample(18)
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
    img = load_sample(i)
    target_width = optimize_pattern_width(pat_orig, img)
    print(f'{i}: {target_width}')


def main_find_blanks():
  size = 18
  img = load_sample(18)
  h, w, _ = img.shape
  # This is the exact color that game uses for blank cells.
  bk = (49, 49, 52)
  result = cv2.inRange(img, bk, bk)

  mk_stat = lambda: collections.defaultdict(lambda: 0)

  row_begins_stat = mk_stat()
  row_ends_stat = mk_stat()

  col_begins_stat = mk_stat()
  col_ends_stat = mk_stat()

  mask = np.zeros((h+2,w+2), dtype=np.uint8)

  # skip first region encountered, which is likely just the difficulty box
  # on the top right corner.
  first_skipped = False
  for r in range(h):
    for c in range(w):
      if (result[r,c] != 0):
        x,y = c,r
        retval, result, _, rect = cv2.floodFill(result, mask, (x,y), 0)
        rect_x, rect_y, rect_w, rect_h = rect

        if not first_skipped:
          first_skipped = True
          continue

        row_begins_stat[rect_y] += 1
        col_begins_stat[rect_x] += 1

        rect_x_end = rect_x + rect_w - 1
        rect_y_end = rect_y + rect_h - 1
        row_ends_stat[rect_y_end] += 1
        col_ends_stat[rect_x_end] += 1

  for stat in [row_begins_stat, row_ends_stat, col_begins_stat, col_ends_stat]:
    for k, v in sorted(stat.items()):
      print(k, f', item count: {v}')

  show = False
  if show:
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
