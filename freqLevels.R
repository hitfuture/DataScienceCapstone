calculateFrequencyLevels <- function(data) {
        data %>%count(freq)%>%rename(count=n)
        
}