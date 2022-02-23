context('Misc Functions')

test_that("join nearest works", {
  sample_slow <- sample_record$vital$tracks$Intellivue$ECG_II[seq(1, 10000, by = 1000), ]
  sample_abp <- sample_record$vital$tracks$Intellivue$ABP[1:5000,]
  res <- join_nearest(sample_slow, sample_abp)

  # Expect unchanged
  expect_known_hash(res, 'd82a73499b')
  expect_lt(abs(mean(with(res, time - time.y))), 0.001)

  slow_df <- data.frame(key_slow = seq(0, 100, by = 10), slow_val = "a")
  fast_df <- data.frame(key_fast = seq(1, 100, by = 3), fast_val = seq(1, 100, by = 3))

  res2 <- join_nearest(slow_df, fast_df, xkey = 'key_slow', ykey = 'key_fast')

})

test_that('Subset record works', {
  interval <- c(lubridate::ymd_hms('1970-01-01 10:24:05', tz = 'UTC'),
                lubridate::ymd_hms('1970-01-01 10:25:05', tz = 'UTC'))
  sample_rec_sub <- subset_record(sample_record, interval)

  expect_equal(nrow(sample_rec_sub$vital$tracks$Intellivue$ABP), 7500)
})

test_that('record_time_to_hms works', {
  sample_rec_hms <- record_time_to_hms(sample_record)

  expect_equal(hms::trunc_hms(sample_rec_hms$vital$tracks$Intellivue$ABP$time[1], 1) %>% as.character(),
               format(sample_record$vital$tracks$Intellivue$ABP$time[1],
                      "%H:%M:%S"))
})
