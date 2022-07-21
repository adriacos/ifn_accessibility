

get_municipalities <- function(){
  iconv(read.csv("data/municipis.csv")$municipi,to="ASCII//TRANSLIT")
}

get_population <- function(){
  read.csv("data/municipis.csv")$hab
}
