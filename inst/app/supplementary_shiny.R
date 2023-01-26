
read_data_app <- function(input, dat) {
  
  # clinical data
  if(is.null(input[["clinical_path"]])) {
    showNotification("No clinical data provided.", 
                     type = "warning",
                     duration = 2)
    clinical_data <- NULL
  }else {
    file <- input[["clinical_path"]]
    ext <- tools::file_ext(file[["datapath"]])
    req(file)
    validate(need(ext == "xlsx", "Please upload a xlsx file"))
    clinical_data <- read_clinical_data(path = file[["datapath"]])
  }
  
  #biocrates data
  if(is.null(input[["biocrates_path"]])) {
    showNotification("No Biocrates data provided.", 
                     type = "error",
                     duration = 2)
  } else {
    file <- input[["biocrates_path"]]
    ext <- tools::file_ext(file[["datapath"]])
    req(file)
    validate(need(ext == "xlsx", "Please upload a xlsx file"))
    biocrates_data <- read_biocrates(path = file[["datapath"]], 
                                     clinical_data = clinical_data)
    dat[["biocrates_data"]] <- biocrates_data
    dat[["removed_LOD"]] <- biocrates_data
  }
  
  dat[["clinical_data"]] <- clinical_data
  dat
}


custom_datatable <- function(dat, paging = TRUE, scrollY = 380) {
  DT::datatable(dat,
                editable = FALSE, 
                options = list(paging = paging, 
                               scrollX = TRUE,
                               scrollY = scrollY,
                               pageLength = 15,
                               searching = FALSE))
}