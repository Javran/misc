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


def subplot_gray(num, img, title):
  pyplot.subplot(num), pyplot.imshow(img,cmap = 'gray')
  pyplot.title(title), pyplot.xticks([]), pyplot.yticks([])


def subplot_color(num, img, title):
  pyplot.subplot(num), pyplot.imshow(img[:,:,[2,1,0]])
  pyplot.title(title), pyplot.xticks([]), pyplot.yticks([])


def main_all_samples():
  pat_orig = cv2.imread('../sample/tree-sample.png')
  for i in range(5,22+1):
    img = load_sample(i)
    target_width = optimize_pattern_width(pat_orig, img)
    print(f'{i}: {target_width}')


def resolve_stat(d, size, threshold = 3):
  """
    Given a dict d and an expected # of elements,
    derive a list of row values (or column values) from it.
  """
  hold = None  # or (k, <sub dict>)
  grouping = []
  for k, v in sorted(d.items(), key=lambda x: x[0]):
    if hold is None:
      hold = (k, {k: v})
    else:
      kh, sd = hold
      if k - kh < threshold:
        sd[k] = v
      else:
        grouping.append(sd)
        hold = (k, {k: v})

  if hold is not None:
    grouping.append(hold[1])
    hold = None

  # TODO: given sufficient info we might be able to
  # "fill in the blank" if there are missing elements,
  # but for now it seems good enough to not worry about
  # this issue.
  assert len(grouping) == size

  # calculate weighted average from grouping elements.
  def ave(sub_dict):
    numer = sum(k * v for k, v in sub_dict.items())
    denom = sum(sub_dict.values())
    return numer / denom

  return map(ave, grouping)


def main_find_blanks():
  size = 22
  img = load_sample(size)
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

  def make_bounds(begin_stat, end_stat):
    begin_coords = map(round, resolve_stat(begin_stat, size))
    end_coords = map(round, resolve_stat(end_stat, size))
    return list(map(lambda x,y: (x,y), begin_coords, end_coords))

  row_bounds = make_bounds(row_begins_stat, row_ends_stat)
  col_bounds = make_bounds(col_begins_stat, col_ends_stat)

  cells = [ [ None for _ in range(size) ] for _ in range(size)]
  for r, (row_lo, row_hi) in enumerate(row_bounds):
    for c, (col_lo, col_hi) in enumerate(col_bounds):
      cells[r][c] = img[row_lo:row_hi+1, col_lo:col_hi+1]

  recombined = np.concatenate([ np.concatenate(row, axis=1) for row in cells], axis=0)

  max_cell_side = max(map(lambda x: x[1] - x[0] + 1, row_bounds + col_bounds))
  def extract_digit(row,col):
    return img[row:row+max_cell_side-1,col:col+max_cell_side-1]

  # Suppose first two cells are A and B, we can then find a cell C if we extend
  # difference between A and B but in the other direction.
  # A - (B - A) = 2A - B

  digit_row_start = 2 * row_bounds[0][0] - row_bounds[1][0]
  digit_col_start = 2 * col_bounds[0][0] - col_bounds[1][0]

  # digits accompanying every column.
  col_digits = [ extract_digit(digit_row_start,col_lo) for col_lo, _ in col_bounds ]
  # same but for rows
  row_digits = [ extract_digit(row_lo,digit_col_start) for row_lo, _ in row_bounds ]

  digits = np.concatenate([np.concatenate(row_digits, axis=1),np.concatenate(col_digits, axis=1)])
  color_unsat = (0x41, 0x4e, 0x7e)  # B,G,R
  digits_result_unsat = cv2.inRange(digits, color_unsat, color_unsat)
  color_sat = (0x97, 0xa7, 0xc8)
  digits_result_sat = cv2.inRange(digits, color_sat, color_sat)

  show = True
  if show:
    pyplot.figure().canvas.set_window_title('@dev')
    subplot_color(221, img, 'origin')
    subplot_color(222, recombined, 'extracted')
    subplot_color(223, digits, 'digits')
    subplot_gray(
      224,
      np.concatenate([digits_result_unsat, digits_result_sat]),
      'digits_inrange')
    pyplot.show()


def main_edge_detection():
  size = 22
  img = load_sample(size)
  # as long as the threshold is good for digits,
  # it is good enough.
  edges = cv2.Canny(img,9,10)
  pyplot.figure().canvas.set_window_title('@dev')
  pyplot.subplot(121), pyplot.imshow(img[:,:,[2,1,0]])
  pyplot.title('origin'), pyplot.xticks([]), pyplot.yticks([])
  pyplot.subplot(122), pyplot.imshow(edges,cmap = 'gray')
  pyplot.title('result'), pyplot.xticks([]), pyplot.yticks([])
  pyplot.show()


if __name__ == '__main__':
  # main_scale_pattern_and_match()
  main_find_blanks()
  # main_edge_detection()
