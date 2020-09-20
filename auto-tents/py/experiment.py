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
import os

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
  pat_orig_h, pat_orig_w = pat_orig.shape[0], pat_orig.shape[1]
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


def rescale_and_match(img, templ_in, tm_method):
  (_,_,w,h) = cv2.boundingRect(img)
  if w == 0 or h == 0:
    return None
  else:
    # try to rescale pattern to match image width (of the bounding rect)
    # we are targeting width here because we can prevent one digit pattern
    # to match with multiple digit ones this way.
    # also because digits tend to vary more in horizontal direction
    # so we are actually eliminating lots of candidates this way.
    templ_in_h, templ_in_w = templ_in.shape
    scale = w / templ_in_w
    templ_h = round(templ_in_h * w / templ_in_w)
    if templ_h > h:
      return None
    templ = cv2.resize(templ_in, (w, templ_h), cv2.INTER_AREA)

  result = cv2.matchTemplate(img, templ, tm_method)
  _, max_val, _, _ = cv2.minMaxLoc(result)
  return max_val


def main_experiment():
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


  def find_tree(cell_img):
    color_shade = (0x55, 0xc8, 0x87)
    result = cv2.inRange(cell_img, color_shade, color_shade)
    (_,_,w,h) = cv2.boundingRect(result)
    if w != 0 and h != 0:
      color = 0xFF
    else:
      color = 0
    return np.full((4,4), color)

  recombined = np.concatenate([ np.concatenate(row, axis=1) for row in cells], axis=0)

  cell_results_recombined = np.concatenate([
    np.concatenate([ find_tree(c) for c in row], axis=1) for row in cells
  ], axis=0)

  max_cell_side = max(map(lambda x: x[1] - x[0] + 1, row_bounds + col_bounds))
  def extract_digit(row,col):
    return img[row:row+max_cell_side-1,col:col+max_cell_side-1]

  # Suppose first two cells are A and B, we can then find a cell C if we extend
  # difference between A and B but in the other direction.
  # A - (B - A) = 2A - B

  digit_row_start = 2 * row_bounds[0][0] - row_bounds[1][0]
  digit_col_start = 2 * col_bounds[0][0] - col_bounds[1][0]

  color_unsat = (0x41, 0x4e, 0x7e)  # B,G,R
  color_sat = (0x97, 0xa7, 0xc8)

  side_length_for_display = math.ceil(max_cell_side * 1.1)
  def process_digit_cell(dg_img):
    result = cv2.inRange(dg_img, color_unsat, color_unsat)
    (x,y,w,h) = cv2.boundingRect(result)
    if w == 0 or h == 0:
      return None
    return result[y:y+h,x:x+w]

  def padding_digit_img(dg_img):
    if dg_img is None:
      return np.full((side_length_for_display, side_length_for_display), 0x7F)

    h, w = dg_img.shape
    top = math.floor((side_length_for_display - h) / 2)
    bottom = side_length_for_display - top - h
    left = math.floor((side_length_for_display - w) / 2)
    right =  side_length_for_display - left - w
    return cv2.copyMakeBorder(dg_img, top, bottom, left, right, borderType=cv2.BORDER_CONSTANT, value=0x7F)

  # TODO: make a matrix of matching results of matchTemplate for row / col digits.
  # where the template is digits cropped by bounding rect,
  # and image is the digit picture after inRange filter.
  # TODO: scaling is for now ignored but we'll do something about it
  # plan: run boundingRect on digit image, and use height of resulting
  # rectangle to scale the template into same height before matchTemplate run.

  # digits accompanying every column.
  col_digits = [
    extract_digit(digit_row_start,col_lo)
    for col_lo, _ in col_bounds
  ]
  col_digit_templs = [ process_digit_cell(d) for d in col_digits ]
  # same but for rows
  row_digits = [
    extract_digit(row_lo,digit_col_start)
    for row_lo, _ in row_bounds
  ]
  row_digit_templs = [ process_digit_cell(d) for d in row_digits ]

  def debug_cross_compare(digits, digit_templs):
    for dg_img_pre in digits:
      dg_img = cv2.inRange(dg_img_pre, color_unsat, color_unsat)
      line = []
      for templ in digit_templs:
        if templ is None:
          line.append('------')
          continue
        max_val = rescale_and_match(dg_img,templ,tm_method)
        if max_val is None:
          line.append('------')
          continue

        line.append(f'{max_val:.4f}')
      print(', '.join(line))

  print('Mat for row digits:')
  debug_cross_compare(row_digits, row_digit_templs)
  print('Mat for col digits:')
  debug_cross_compare(col_digits, col_digit_templs)

  digits = np.concatenate(
    [
      np.concatenate([padding_digit_img(x) for x in row_digit_templs], axis=1),
      np.concatenate([padding_digit_img(x) for x in col_digit_templs], axis=1),
    ])

  # digit sample extraction steps (for each single cell image)
  # (TODO: for simplicity, let's only consider color of unsat digits for now)
  # - cv2.inRange to extract shape of the digit
  # - cv2.boundingRect to find the bounding rectangle
  # - crop it and save it as image.
  # - for sat digits, the checkmark needs to be extracted,
  #   but that's not an immediate issue as most of the digits are indeed unsat.
  # - note that a digit cell can contain multiple digits,
  #   we could get only a partial digit, but that doesn't really affect
  #   the correctness of matchTemplate.

  show = True
  if show:
    pyplot.figure().canvas.set_window_title('@dev')
    subplot_color(221, img, 'origin')
    subplot_color(222, recombined, 'extracted')
    subplot_gray(223, digits, 'digits')
    subplot_gray(224, cell_results_recombined, 'find tree')
    pyplot.show()


def main_tagging():
  # TODO:
  # the idea of this function is to turn this program into an iterative loop to
  # gradually tag sample images with digits, recognized from board of various sizes.
  store_path = '../private/digits'
  if not os.path.exists(store_path):
    os.makedirs(store_path)

  # limit the # of samples stored to disk per execution.
  store_quota = 12

  for size in range(6,22+1):
    if store_quota <= 0:
      break
    img = load_sample(size)
    h, w, _ = img.shape


if __name__ == '__main__':
  # main_experiment()
  main_tagging()
