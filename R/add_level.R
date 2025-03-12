add_level <- function(physeq, level, level_name, look_up_table){

  new_tax_table  <- as.data.frame(tax_table(physeq)) |>
    as_tibble() |>
    mutate({{level_name}} := recode({{level}},
                                    !!!look_up_table,
                                    .default = "Unassigned")) |>
    relocate({{level_name}}) |>
    as.matrix() |>
    tax_table()

  tax_table(physeq) <- new_tax_table

  return(physeq)
}
