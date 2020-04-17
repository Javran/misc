cc_library(
    name = "opencv",
    srcs = glob([
        "lib64/*.so*",
    ]),
    hdrs = glob([
        "include/**/*.hpp",
        "include/**/*.h",
    ]),
    includes = ["include/opencv4"],
    linkstatic = 1,
    visibility = ["//visibility:public"],
)
