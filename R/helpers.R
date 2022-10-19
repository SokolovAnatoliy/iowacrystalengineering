
#' A function to show me a logo for a university
#'  The function returns a path to the system directory location of the specific picture
#' @param image a single image name
#' @param what_are_all_the_names a Logical that will return all known logos in the package
#' @export
show_me_a_logo = function(image, what_are_all_the_names = FALSE) {

  system_path = system.file("extdata/logos", package = "iowacrystalengineering")

  all_files = list.files(system_path, full.names = TRUE)

  all_files_table = tibble(file_path = all_files) %>%
    mutate(without_ext = file_path_sans_ext(basename(all_files)))

  if(what_are_all_the_names) {
    return(all_files_table$without_ext)
  }

  if(image == "random"){
    random_image = sample(1:length(all_files), size = 1, replace = TRUE)
    all_files[random_image] -> chosen
    return(chosen)
  }

  rlang::arg_match(image, all_files_table$without_ext)
  stopifnot("Only 1 image is allowed" = length(image) == 1)
  all_files_table %>%
    filter(without_ext == image) %>%
    pull(file_path)
}



#' a function to lay out a table in exactly the way Len likes it.
#' @param table a tibble or dataframe to be laid out
#' @param caption a string that will be used as the caption for the table
#' @param format arguments to the format function of kable. Defaults to latex
#' @importFrom knitr kable
#' @importFrom kableExtra linebreak kable_styling
#' @export
make_len_style_table = function(table, caption, format = "latex"){
  column_names = names(table)

  table %>%
    knitr::kable(
      format = "latex",
      align = "l",
      booktabs = TRUE,
      longtable = FALSE,
      linesep = "",
    ) %>%
    kableExtra::kable_styling(
      position = "left",
      latex_options = c("striped", "repeat_header", "scale_down"),
      stripe_color = "gray!15"
    )
}

#' a function to make a table using gt
#' @param data the data set as a tibble
#' @param title the title of the table
#' @param merge_cols a vector of two column names in the table to be merged
#' @param images_col_name the name of the column that holds the path to the images
#' @param container_size numeric for the size of the html container
#' @importFrom gt gt tab_header
#' @importFrom gtExtras gt_theme_nytimes gt_merge_stack gt_img_rows
#' @export
make_cool_table = function(data, title, merge_cols = NULL, images_col_name = NULL, container_size = 450) {


  if(!is.null(merge_cols)){
    stopifnot("Please provide two column names for merge or none" = length(merge_cols) == 2)
    rlang::arg_match(merge_cols, names(data), multiple = TRUE)
  }

  data %>%
    gt::gt() %>%
    gt::tab_header(
      title = title
    )%>%
    gtExtras::gt_theme_nytimes() %>%
    {
      if(length(merge_cols) == 2) {

        gtExtras::gt_merge_stack(., col1 = !!sym(merge_cols[1]), col2 = !!sym(merge_cols[2]))
      }
      else .
    }  %>%
    {
      if(length(images_col_name) == 1){
        gtExtras::gt_img_rows(., columns = .data[[{{images_col_name}}]], height = 20, img_source = "local")
      }
      else .
    } %>%
    suppressWarnings() %>%
    tab_options(container.height = px(container_size)) %>%
    gt::as_raw_html()
}
