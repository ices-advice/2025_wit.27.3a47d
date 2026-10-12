getTotalCatch <- function(x, catchName = "CATON") {
  x %>% ungroup() %>% summarise(total = sum(!!as.name(catchName), na.rm = TRUE))
}
makePlotTable <- function(tbl1, cat1, cat2 = NULL, digits = 2) {
  if(is.null(cat2)) {
    tbl1 %>%
      group_by(!!cat1) %>%
      summarise(Total = round(sum(CATON, na.rm = TRUE) / 1000, digits = digits))  %>%
      arrange(desc(Total)) %>% {
        ggplot(.) + geom_bar(aes(reorder(!!cat1, -Total), Total), fill = "skyblue2", stat = "Identity") +
          theme(axis.text.x = element_text(angle = 90)) +
          xlab("") +
          tableGrob(., rows = NULL, theme = ttheme_default(base_size = bs))
      }
  } else {
    frml <- as.formula(paste(cat1, "~", cat2))
    cbgc <- tbl1 %>%
      group_by(!!cat1, !!cat2) %>%
      summarise(Total = round(sum(CATON, na.rm = TRUE) / 1000, digits = digits))  %>%
      filter(Total > 0) %>%
      reshape2::dcast(frml, value.var = "Total") %>%
      mutate(Catch = ifelse(!is.na(Discards), Discards + Landings, Landings)) %>%
      group_by(!!cat1) %>%
      arrange(desc(Catch))
    ggplot(data = cbgc %>% reshape2::melt() %>% filter(variable != "Catch")) +
      geom_bar(aes(reorder(!!cat1, -value, sum, na.rm = TRUE), y = value, fill = variable), stat = "Identity") +
      xlab("") + ylab("Catch (tonnes)") +
      scale_fill_brewer("") +
      theme(legend.position = c(0.8, 0.8), axis.text.x = element_text(angle = 90)) +
      gridExtra::tableGrob(cbgc %>% tidyr::replace_na(list("Discards" = 0)), rows = NULL,
                           theme = ttheme_default(base_size = bs)) +
      plot_layout(nrow = 1, widths = c(1,2))
  }
}