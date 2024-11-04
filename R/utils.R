prepare_index <- function(){
    slides <- list.files("slides/", 
                         all.files = TRUE,
                         recursive = TRUE)
    slides <- slides[!grepl("_qrcode.svg", slides)]
    data <- readxl::read_xlsx("conferences.xlsx")
    data$file <- slides[order(match(data$folder, dirname(slides)))]
    ext <- xfun::file_ext(data$file)
    links <- sprintf("[[%s](%s)]", ext, slides)
    data$out <- sprintf("**%s**, %s (%s) - %s", data$title, data$conference, data$place, links)
    return(data)
}

make_index <- function(){
    data <- prepare_index()
    data$year <- lubridate::year(data$date)
    data_by_year <- split(data, data$year)
    
    for(i in 1:length(data_by_year)){
        cat("#", names(data_by_year)[i], "\n\n")
        cat("- ", data_by_year[[i]]$out, "\n\n")
    }
    # creating qrcodes
    purrr::walk(data$file, make_qr_code)
}

make_qr_code <- function(x){
    code <- qrcode::qr_code(x)
    qr_code_file <- sprintf("%s_qrcode.svg", xfun::sans_ext(x))
    qrcode::generate_svg(code, filename = qr_code_file)
}
