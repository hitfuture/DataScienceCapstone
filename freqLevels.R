calculateFrequencyLevels <- function(data) {
        data %>%count(freq, sort = TRUE)%>%rename(count=n)
        
}

uniFreq <- calculateFrequencyLevels(frequencyBigramDF)
