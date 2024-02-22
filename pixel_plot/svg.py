from typing import TypeAlias, Any

import os
import sys

import attr
import yaml


@attr.s
class Config:
  pixel_len = attr.ib(type=int)
  colors = attr.ib(type=dict[str,str])
  input_path = attr.ib(type=str)
  output_path = attr.ib(type=str)


def load_config(path: str) -> Config:
  def _resolve_path(path: Any) -> str:
    assert isinstance(path, str), 'Invalid path, expected str.'
    if os.path.isabs(path):
      return path

    base_dir = os.path.dirname(os.path.abspath(path))
    return os.path.join(base_dir, path)

  with open(path, 'r') as f:
    data = yaml.load(f, Loader=yaml.SafeLoader)

  pixel_len = data.get('pixel_len', None)
  assert isinstance(pixel_len, int) and pixel_len > 0, 'Invalid pixel_len.'

  colors = data.get('colors', None)
  assert isinstance(colors, dict), 'Invalid colors, expected an object.'
  for key, value in colors.items():
    assert isinstance(key, str) and len(key) == 1, f'Invalid key: {key}.'

  input_path = _resolve_path(data.get('input', None))
  output_path = _resolve_path(data.get('output', None))
  return Config(pixel_len=pixel_len, colors=colors, input_path=input_path, output_path=output_path)


Dims: TypeAlias = tuple[int,int]  # (<x or column count>, <y or row count>)


def load_input(config: Config) -> tuple[list[str], Dims]:
  with open(config.input_path, 'r') as f:
    xs = f.read().split('\n')

  dims = len(max(xs, key=len)), len(xs)
  print(f'Dimensions of input file: {dims}')

  uniques = set(ch for line in xs for ch in line)
  not_founds = { u for u in uniques if u not in config.colors}
  if len(not_founds):
    print(f'Warning: ignoring characters not defined in colors mapping: {not_founds}')
  return xs, dims


def generate_svg(config: Config, content: list[str], dims: Dims) -> str:
  sz = config.pixel_len
  cols, rows = dims
  rects = []

  for y, line in enumerate(content):
    for x, ch in enumerate(line):
      if color := config.colors.get(ch, None):
        rects.append(f'<rect x="{sz*x}" y="{sz*y}" width="{sz}" height="{sz}" fill="{color}" />')

  return (
    f'<svg version="1.1" width="{sz*cols}" height="{sz*rows}" xmlns="http://www.w3.org/2000/svg">' +
    ''.join(rects) +
    '</svg>')


if __name__ == '__main__':
  # Exactly one positional arg for config as input.
  [_, config_path] = sys.argv
  config = load_config(config_path)
  print(f'Will read from {config.input_path},')
  print(f'  and write to {config.output_path}.')
  content, dims = load_input(config)
  with open(config.output_path, 'w') as f:
    f.write(generate_svg(config, content, dims))
  print('Written.')
