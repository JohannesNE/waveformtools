test_that("find_resp_len_from_PP works", {
    samp_beats <- sample_record2$beats[1:50,]
    resp_len <- find_resp_len_from_PP(samp_beats)
    expect_equal(resp_len, 4.6, tolerance = 0.01)
})

test_that("PP_gam_analysis works", {
    samp_beats2 <- sample_record2$beats[50:100,]
    gam_analysis <- PP_gam_analysis(samp_beats2)
    expect_equal(gam_analysis$resp_len, 4.6, tolerance = 0.05)
    expect_equal(gam_analysis$PPV, 0.084, tolerance = 0.001)
})
