library(shinyloadtest)
shinyloadtest::record_session("http://127.0.0.1:5790/")

shinycannon recording.log http://127.0.0.1:5790/ --workers 5 --loaded-duration-minutes 2 --output-dir run1