"""match-template cli"""

load("@rules_cc//cc:defs.bzl", "cc_binary")

cc_binary(
    name = "mt_main",
    srcs = ["mt_main.cc"],
    deps = [
        "@com_google_absl//absl/flags:flag",
        "@com_google_absl//absl/flags:parse",
        "@com_google_absl//absl/strings",
        "@opencv",
    ],
)
