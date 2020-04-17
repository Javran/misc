#include <string>
#include <iostream>
#include <opencv2/opencv.hpp>

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/string_view.h"

namespace mt {
enum class MatchMode {
  kSQDIFF,
  kSQDIFF_NORMED,
  kCCORR,
  kCCORR_NORMED,
  kCCOEFF,
  kCCOEFF_NORMED,
};

bool AbslParseFlag(absl::string_view text,
                   MatchMode *mode,
                   std::string *error) {
  #define DECL(NAME) \
    if (text == #NAME) { \
      *mode = MatchMode::k##NAME; \
      return true; \
    }
  #define DECL2(NAME) \
    DECL(NAME) \
    DECL(NAME##_NORMED)
  DECL2(SQDIFF)
  DECL2(CCORR)
  DECL2(CCOEFF)
  #undef DECL2
  #undef DECL
  *error = "unknown value for MatchMode";
  return false;
}

std::string AbslUnparseFlag(MatchMode mode) {
  #define CASE(NAME) \
    case MatchMode::k##NAME: return #NAME;
  #define CASE2(NAME) \
    CASE(NAME) \
    CASE(NAME##_NORMED)

  switch (mode) {
    CASE2(SQDIFF)
    CASE2(CCORR)
    CASE2(CCOEFF)
    default: return absl::StrCat(mode);
  }
  #undef CASE2
  #undef CASE
}

}

ABSL_FLAG(std::string, image_path, "", "Image path.");
ABSL_FLAG(std::string, template_path, "", "Template path.");
ABSL_FLAG(std::string, output_path, "", "Output path.");
ABSL_FLAG(mt::MatchMode, match_mode, mt::MatchMode::kSQDIFF, "Match mode.");

int main(int argc, char* argv[]) {
  absl::ParseCommandLine(argc, argv);
  cv::Mat image = cv::imread(absl::GetFlag(FLAGS_image_path), 1);
  cv::namedWindow("Display Image", cv::WINDOW_AUTOSIZE);
  cv::imshow("Display Image", image);
  cv::waitKey(0);
  return 0;
}
