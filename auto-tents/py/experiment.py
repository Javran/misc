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
import uuid
import re
import functools
import json

tm_method = cv2.TM_CCOEFF_NORMED
color_unsat = (0x41, 0x4e, 0x7e)  # B,G,R
color_sat = (0x97, 0xa7, 0xc8)
color_shade = (0x55, 0xc8, 0x87)

store_path = '../private/digits'
preset_path = '../private/preset.json'

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
  assert size is None or len(grouping) == size

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


def find_cell_bounds(img, size=None):
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

  if size is None:
    assert len(row_bounds) == len(col_bounds), f'Mismatched bound length {len(row_bounds)} vs {len(col_bounds)}.'

  return row_bounds, col_bounds


def extract_digits(img, cell_bounds):
  h, w, _ = img.shape
  row_bounds, col_bounds = cell_bounds
  max_cell_side = max(map(lambda x: x[1] - x[0] + 1, row_bounds + col_bounds))
  def extract_digit(row,col):
    return img[row:row+max_cell_side-1,col:col+max_cell_side-1]

  # Suppose first two cells are A and B, we can then find a cell C if we extend
  # difference between A and B but in the other direction.
  # A - (B - A) = 2A - B

  digit_row_start = 2 * row_bounds[0][0] - row_bounds[1][0]
  digit_col_start = 2 * col_bounds[0][0] - col_bounds[1][0]

  # digits accompanying every row.
  row_digits = [
    extract_digit(row_lo,digit_col_start)
    for row_lo, _ in row_bounds
  ]
  # same but for columns
  col_digits = [
    extract_digit(digit_row_start,col_lo)
    for col_lo, _ in col_bounds
  ]
  return row_digits, col_digits

def crop_digit_cell(img):
  result = cv2.inRange(img, color_unsat, color_unsat)
  (x,y,w,h) = cv2.boundingRect(result)
  if w == 0 or h == 0:
    return None
  return result[y:y+h,x:x+w]


def main_experiment():
  size = 22
  img = load_sample(size)
  h, w, _ = img.shape

  cell_bounds =  find_cell_bounds(img, size)
  row_bounds, col_bounds = cell_bounds

  cells = [ [ None for _ in range(size) ] for _ in range(size)]
  for r, (row_lo, row_hi) in enumerate(row_bounds):
    for c, (col_lo, col_hi) in enumerate(col_bounds):
      cells[r][c] = img[row_lo:row_hi+1, col_lo:col_hi+1]

  def find_tree(cell_img):

    result = cv2.inRange(cell_img, color_shade, color_shade)
    (_,_,w,h) = cv2.boundingRect(result)
    if w != 0 and h != 0:
      color = 0xFF
    else:
      color = 0
    return np.full((4,4), color)

  recombined = np.concatenate([ np.concatenate(row, axis=1) for row in cells ], axis=0)

  cell_results_recombined = np.concatenate([
    np.concatenate([ find_tree(c) for c in row], axis=1) for row in cells
  ], axis=0)

  max_cell_side = max(map(lambda x: x[1] - x[0] + 1, row_bounds + col_bounds))
  side_length_for_display = math.ceil(max_cell_side * 1.1)

  def padding_digit_img(dg_img):
    if dg_img is None:
      return np.full((side_length_for_display, side_length_for_display), 0x7F)

    h, w = dg_img.shape
    top = math.floor((side_length_for_display - h) / 2)
    bottom = side_length_for_display - top - h
    left = math.floor((side_length_for_display - w) / 2)
    right =  side_length_for_display - left - w
    return cv2.copyMakeBorder(dg_img, top, bottom, left, right, borderType=cv2.BORDER_CONSTANT, value=0x7F)

  # digits accompanying every row and col.
  row_digits, col_digits = extract_digits(img, cell_bounds)

  row_digit_templs = [ crop_digit_cell(d) for d in row_digits ]
  col_digit_templs = [ crop_digit_cell(d) for d in col_digits ]

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

  # digit sample extraction steps (for each single cell image):
  # - cv2.inRange to extract shape of the digit
  # - cv2.boundingRect to find the bounding rectangle
  # - crop it and save it as image.

  show = True
  if show:
    pyplot.figure().canvas.set_window_title('@dev')
    subplot_color(221, img, 'origin')
    subplot_color(222, recombined, 'extracted')
    subplot_gray(223, digits, 'digits')
    subplot_gray(224, cell_results_recombined, 'find tree')
    pyplot.show()


SAMPLE_FILENAME_PATTEN = re.compile(r'^([^_]+)_.*.png$')


def load_samples():
  """Returns a dict from tag to a list of images."""

  if not os.path.exists(store_path):
    os.makedirs(store_path)
    return {}

  d = collections.defaultdict(list)
  untagged_count = 0

  for filename in os.listdir(store_path):
    result = SAMPLE_FILENAME_PATTEN.match(filename)
    if result is None:
      continue
    tag = result.group(1)
    if tag == 'UNTAGGED':
      untagged_count += 1
      continue
    d[tag].append(cv2.imread(os.path.join(store_path, filename),cv2.IMREAD_GRAYSCALE))

  if untagged_count:
    print(f'There are {untagged_count} untagged samples.')
  return d


def find_tag(tagged_samples, img_pre):
  img = cv2.inRange(img_pre, color_unsat, color_unsat)
  best_val, best_tag = None, None
  for tag, samples in tagged_samples.items():
    for pat in samples:
      val = rescale_and_match(img,pat,tm_method)
      if val is None:
        continue
      if best_val is None or best_val < val:
        best_val, best_tag = val, tag
  return best_val, best_tag


def main_tagging(dry_run=True):
  # the idea of this function is to turn this program into an iterative loop to
  # gradually tag sample images with digits, recognized from boards of various sizes.

  tagged_samples = load_samples()
  sample_count = functools.reduce(lambda acc, l: acc + len(l), tagged_samples.values(), 0)
  print(f'Loaded {len(tagged_samples)} tags, {sample_count} tagged samples in total.')

  # limit the # of samples stored to disk per execution.
  # for now this is effectively not doing anything but we want to avoid
  # running into a situation that saves too many files at once.
  store_quota = 100
  visit_count = 0
  good_count = 0

  for size in range(6,22+1):
    if store_quota <= 0:
      break
    print(f'Processing image sample of size {size} ...')
    img = load_sample(size)
    h, w, _ = img.shape
    cell_bounds = find_cell_bounds(img, size)
    row_digits, col_digits = extract_digits(img, cell_bounds)
    for digit_img in row_digits + col_digits:
      digit_img_cropped = crop_digit_cell(digit_img)
      if digit_img_cropped is None:
        continue

      visit_count += 1
      # use original image for this step as we want some room around
      # the sample to allow some flexibility.
      best_val, best_tag = find_tag(tagged_samples, digit_img)
      if best_val is not None and best_val >= 0.9:
        good_count += 1
        continue

      if best_val is None:
        print(f'Found new sample with no good guesses.')
      else:
        print(f'Found new sample with best guess being {best_tag}, with score {best_val}')

      nonce = str(uuid.uuid4())
      fpath = os.path.join(store_path, f'UNTAGGED_{nonce}.png')
      if dry_run:
        print(f'(Dry run) Saving a sample shaped {digit_img_cropped.shape} to {fpath}...')
      else:
        print(f'Saving a sample shaped {digit_img_cropped.shape} to {fpath}...')
        cv2.imwrite(fpath, digit_img_cropped)
      store_quota -= 1
      if store_quota <= 0:
        break
  print(f'Store quota is now {store_quota}.')
  print(f'Visited {visit_count} samples and {good_count} of them found good matches.')


# TODO: Given that most of the processing time is spent on doing floodFill to figure out cell bounds,
# it makes sense that we have this info pre-processed. In order to achieve so, we must extract size of the board.
# Note that despite regular puzzle shows size info (size x size), daily puzzles do not.
# one potential alternative is to examine an empty cell of the board and see if it's possible to establish size this way
# (assuming that all puzzles are squares)
def main_generate_preset():
  # TODO: plan to serialize to json file.
  # schema:
  # top level is an Object keyed by screen width and height i.e. "1440x2880"
  # then values are Object keyed by size e.g. "16x16", which is then keyed by "row_bounds" and "col_bounds",
  # which are Arrays whose elements are Array of two elements [lo, hi].
  # e.g.:
  # {
  #   "1440x2880": {"16x16": {"row_bounds": [[a,b], [c,d], ...], "col_bounds": [[a,b], [c,d], ...]}}
  # }
  cell_bounds_mapping = {}
  for size in range(6,22+1):
    print(f'Processing {size}x{size} ...')
    img = load_sample(size)
    h, w, _ = img.shape
    assert (h,w) == (2880,1440)
    cell_bounds = find_cell_bounds(img, size)
    row_bounds, col_bounds = cell_bounds
    cell_bounds_mapping[f'{size}x{size}'] = {
      'row_bounds': row_bounds,
      'col_bounds': col_bounds,
    }
  full = {'1440x2880': cell_bounds_mapping}
  print(json.dumps(full,sort_keys=True,separators=(',', ':')))


if __name__ == '__main__':
  # main_experiment()
  # main_tagging()
  main_generate_preset()

