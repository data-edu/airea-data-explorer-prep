library(shinyloadtest)
shinyloadtest::record_session("http://127.0.0.1:5790/")

shinycannon recording.log http://127.0.0.1:5790/ \
--workers 20 \
--loaded-duration-minutes 5 \
--output-dir loadtest-20

shinyloadtest_report("loadtest-20")
