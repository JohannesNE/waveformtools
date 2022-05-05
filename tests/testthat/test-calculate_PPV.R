test_that("find PP minmax works", {
  samp_beats <- sample_record2$beats[50:100,]
  expect_error(find_PP_minmax(samp_beats))
  expect_warning(find_PP_minmax(samp_beats, resp_rate = 2, resp_len = 10))

  test_minmax <- find_PP_minmax(samp_beats, resp_len = 4.6)
  expect_equal(min(test_minmax$PP_min), 41.875)
  expect_equal(max(test_minmax$PP_max), 47.125)

  # ggplot(samp_beats, aes(time, PP)) +
  #     geom_point() +
  #     geom_line() +
  #     geom_point(aes(time_max, PP_max), data = test_minmax, color = "red") +
  #     geom_point(aes(time_min, PP_min), data = test_minmax, color = "green")
})

test_that("find PP minmax works with excluded beats", {
    samp_beats_excl <- sample_record2$beats[c(1:20, 30:50),]

    test_minmax <- find_PP_minmax(samp_beats_excl, resp_len = 4.6)
    expect_length(test_minmax$PP_min, 4)

    # ggplot(samp_beats_excl, aes(time, PP)) +
    #     geom_point() +
    #     geom_line() +
    #     geom_point(aes(time_max, PP_max), data = test_minmax, color = "red") +
    #     geom_point(aes(time_min, PP_min), data = test_minmax, color = "green")
})

test_that("calculate_classic_ppv works", {
    samp_beats <- sample_record2$beats[50:100,]
    ppv <- calculate_classic_ppv(samp_beats, resp_len = 4.6)
    expect_equal(ppv, 8.211, tolerance = 0.01)
})
